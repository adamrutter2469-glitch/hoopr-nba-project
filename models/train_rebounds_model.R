# ============================================================
# Script: models/train_rebounds_model.R
# Purpose: Predict a team's total rebounds (REB) in a game from
#          pre-game information only: the team's own rolling form,
#          the opponent's own rolling rebounding form, what each
#          team typically allows, and explicit matchup-differential
#          features. See data_processed/team_game_features.parquet,
#          built by R/features_rolling.R + R/features_combine.R.
#
#          A separate, user-run analysis script - NOT wired into
#          run_pipeline.bat, since retraining a model isn't an ETL
#          step that should happen on every incremental data pull
#          (same reasoning as scripts/wip_injuries.R being kept
#          separate).
#
#          Run: Rscript models/train_rebounds_model.R
#          (from the project root, after scripts/00_install_packages.R)
# ============================================================

suppressMessages({
  library(dplyr)
  library(arrow)
  library(glmnet)
  library(xgboost)
})

source("config/config.R")

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------
team_features <- read_parquet(cfg$path_team_game_features)

# ------------------------------------------------------------
# 2. Predictor allow-list (hand-maintained, NOT a regex/pattern
#    select). Every column here is fixed before tip-off - none of
#    them reflect this game's actual outcome. Raw same-game columns
#    (reb, oreb, dreb, adv_*, pts, etc.) are deliberately excluded:
#    using them would mean predicting a game's rebounds from that
#    same game's actual box score. A pattern-based selector could
#    silently absorb a future new rolling column and defeat the
#    point of an allow-list for leakage safety - keep this explicit.
# ------------------------------------------------------------
predictor_cols <- c(
  # this team's own rolling form
  "reb_roll10", "oreb_roll10", "dreb_roll10",
  "adv_c_reb_roll10", "adv_c_oreb_roll10", "adv_c_dreb_roll10",
  "adv_reb_0_3_roll10", "adv_reb_3_6_roll10", "adv_reb_6_10_roll10", "adv_reb_10_plus_roll10",
  # the opponent's own rolling form
  "opp_reb_roll10", "opp_oreb_roll10", "opp_dreb_roll10",
  "opp_adv_c_reb_roll10", "opp_adv_reb_0_3_roll10",
  # this team's own history of what opponents typically do to them
  "reb_allowed_roll10", "oreb_allowed_roll10", "dreb_allowed_roll10",
  # what the opponent typically allows (mirrored)
  "opp_reb_allowed_roll10", "opp_oreb_allowed_roll10", "opp_dreb_allowed_roll10",
  "opp_adv_c_reb_allowed_roll10",
  # explicit matchup differentials
  "reb_matchup_edge_roll10", "oreb_matchup_edge_roll10", "dreb_matchup_edge_roll10",
  "adv_c_reb_matchup_edge_roll10", "adv_reb_0_3_matchup_edge_roll10",
  # game context
  "is_home", "rest_days", "opp_rest_days", "is_b2b", "opp_is_b2b",
  "is_3in4", "opp_is_3in4", "est_flight_time", "time_zone_shift"
)
target_col <- "reb"  # the actual box-score total, not adv_reb

missing_cols <- setdiff(predictor_cols, c(names(team_features), "is_home"))
if (length(missing_cols) > 0) {
  stop("Predictor columns missing from team_game_features.parquet: ",
       paste(missing_cols, collapse = ", "),
       " - did the feature pipeline run with rebounding data available?")
}

# ------------------------------------------------------------
# 3. Prepare model-ready data: derive is_home, coerce logicals to
#    0/1, drop rows with any NA among the chosen predictors (early-
#    season games before a full rolling window exists) or target.
# ------------------------------------------------------------
model_data <- team_features %>%
  mutate(
    is_home = as.integer(home_away == "home"),
    across(where(is.logical), as.integer)
  ) %>%
  select(season, game_date, team_id, opponent_id, all_of(target_col), all_of(predictor_cols)) %>%
  filter(if_all(all_of(c(target_col, predictor_cols)), ~ !is.na(.x)))

cat("Model-ready rows after dropping incomplete windows:", nrow(model_data), "\n")
cat("By season:\n")
print(model_data %>% count(season))

# ------------------------------------------------------------
# 4. Time-based split (never random - rows are temporally
#    autocorrelated, and a real deployment always predicts an
#    unplayed future game from past data only):
#      train      2022-23 + 2023-24            (fit glmnet & xgboost)
#      validation 2024-25                      (xgboost early stopping only)
#      test       2025-26                      (final holdout, untouched
#                                                until evaluation - both
#                                                models and the baseline
#                                                are scored on this once)
#    glmnet fits on train+validation combined, since cv.glmnet does
#    its own internal CV for lambda selection and doesn't need a
#    separate external validation partition the way xgboost's
#    early-stopping watchlist does.
# ------------------------------------------------------------
train_seasons <- c("2022-23", "2023-24")
val_season    <- "2024-25"
test_season   <- "2025-26"

train_df <- model_data %>% filter(season %in% train_seasons)
val_df   <- model_data %>% filter(season == val_season)
test_df  <- model_data %>% filter(season == test_season)

cat(sprintf("\ntrain: %d rows (%s) | validation: %d rows (%s) | test: %d rows (%s)\n",
            nrow(train_df), paste(train_seasons, collapse = "+"),
            nrow(val_df), val_season, nrow(test_df), test_season))

to_matrix <- function(df) as.matrix(df %>% select(all_of(predictor_cols)))

x_train <- to_matrix(train_df);            y_train <- train_df[[target_col]]
x_val   <- to_matrix(val_df);              y_val   <- val_df[[target_col]]
x_test  <- to_matrix(test_df);             y_test  <- test_df[[target_col]]
x_train_full <- to_matrix(bind_rows(train_df, val_df))
y_train_full <- c(y_train, y_val)

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))

# ------------------------------------------------------------
# 5. Baseline: "assume a team does what it usually does" - just
#    predict its own reb_roll10 directly, no opponent information
#    at all. Everything below needs to beat this to justify the
#    added complexity.
# ------------------------------------------------------------
baseline_pred <- test_df$reb_roll10
baseline_rmse <- rmse(y_test, baseline_pred)
baseline_mae  <- mae(y_test, baseline_pred)

# ------------------------------------------------------------
# 6. Elastic net (glmnet) - interpretable: every coefficient is a
#    direct, readable effect size. alpha=0.5 mixes L1 (feature
#    selection) and L2 (stability among correlated rolling stats,
#    which these are, by construction).
# ------------------------------------------------------------
set.seed(42)
cv_fit <- cv.glmnet(x_train_full, y_train_full, alpha = 0.5, nfolds = 5)
glmnet_pred <- as.numeric(predict(cv_fit, newx = x_test, s = "lambda.min"))
glmnet_rmse <- rmse(y_test, glmnet_pred)
glmnet_mae  <- mae(y_test, glmnet_pred)

# ------------------------------------------------------------
# 7. Gradient-boosted trees (xgboost) - accuracy: can find
#    nonlinear effects and interactions (e.g. how the matchup
#    differential's importance might depend on rest days) without
#    them being hand-engineered. Early stopping against the
#    validation season, NOT the test season, so test stays an
#    untouched final holdout.
# ------------------------------------------------------------
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dval   <- xgb.DMatrix(data = x_val,   label = y_val)
dtest  <- xgb.DMatrix(data = x_test,  label = y_test)

set.seed(42)
xgb_fit <- xgb.train(
  params = list(objective = "reg:squarederror", eta = 0.05, max_depth = 4,
                subsample = 0.8, colsample_bytree = 0.8),
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, validation = dval),
  early_stopping_rounds = 30,
  verbose = 0
)
xgb_pred <- predict(xgb_fit, dtest)
xgb_rmse <- rmse(y_test, xgb_pred)
xgb_mae  <- mae(y_test, xgb_pred)

# ------------------------------------------------------------
# 8. Results summary
# ------------------------------------------------------------
results <- tibble::tibble(
  model = c("Baseline (own reb_roll10)", "Elastic net (glmnet)", "Gradient-boosted trees (xgboost)"),
  rmse  = c(baseline_rmse, glmnet_rmse, xgb_rmse),
  mae   = c(baseline_mae, glmnet_mae, xgb_mae)
)

cat("\n==================== RESULTS (", test_season, " holdout) ====================\n", sep = "")
print(results, digits = 4)

cat("\n--- Elastic net: non-zero coefficients (sorted by |effect|) ---\n")
coefs <- as.matrix(coef(cv_fit, s = "lambda.min"))
coefs <- tibble::tibble(feature = rownames(coefs), coefficient = coefs[, 1]) %>%
  filter(feature != "(Intercept)", coefficient != 0) %>%
  arrange(desc(abs(coefficient)))
print(coefs, n = Inf)

cat("\n--- xgboost: top 15 features by gain ---\n")
importance <- xgb.importance(model = xgb_fit)
print(head(importance, 15))

cat("\n--- xgboost stopped at iteration:", xgb_fit$best_iteration, "of 1000 max ---\n")

# ------------------------------------------------------------
# 9. Save fitted models
# ------------------------------------------------------------
dir.create("models", showWarnings = FALSE)
saveRDS(cv_fit, "models/rebounds_elasticnet.rds")
invisible(xgb.save(xgb_fit, "models/rebounds_xgboost.model"))
saveRDS(predictor_cols, "models/rebounds_predictor_cols.rds")

cat("\nSaved: models/rebounds_elasticnet.rds, models/rebounds_xgboost.model, models/rebounds_predictor_cols.rds\n")
