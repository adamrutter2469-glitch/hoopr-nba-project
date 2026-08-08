@echo off
REM ============================================================
REM run_pipeline.bat
REM Double-click (or schedule) this to pull any NBA games that
REM have occurred since the last run and rebuild the comprehensive
REM team/player feature tables. Safe to run as often as you like -
REM everything is incremental.
REM
REM To skip the slow advanced-rebounding stage for one run, pass
REM --skip-rebounding, e.g.:
REM   run_pipeline.bat --skip-rebounding
REM ============================================================

setlocal
set "RSCRIPT=C:\Program Files\R\R-4.3.2\bin\Rscript.exe"

REM Run from this file's own folder, whatever that is, so the
REM pipeline's relative paths (data_raw/, data_processed/, etc.)
REM always resolve correctly regardless of where it's launched from.
cd /d "%~dp0"

if not exist "%RSCRIPT%" (
  echo *** Rscript.exe not found at "%RSCRIPT%" ***
  echo Edit run_pipeline.bat and update the RSCRIPT path for your R install.
  pause
  exit /b 1
)

echo Running NBA pipeline...
echo.

"%RSCRIPT%" "scripts\run_pipeline.R" %*

if %ERRORLEVEL% NEQ 0 (
  echo.
  echo *** Pipeline FAILED - see logs\ for details ***
) else (
  echo.
  echo Pipeline complete. See data_processed\team_game_features.rds and
  echo data_processed\player_game_features.rds, or logs\ for the run log.
)

pause
