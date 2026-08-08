library(rvest)
library(dplyr)

get_bbref_injuries <- function() {
  url <- "https://www.basketball-reference.com/friv/injuries.fcgi"
  
  page <- read_html(url)
  
  # Basketball Reference always puts the injury table inside the first <table>
  injuries <- page %>%
    html_element("table") %>%
    html_table()
  
  injuries %>%
    rename(
      date = `Date`,
      team = `Team`,
      player = `Player`,
      injury = `Injury`,
      status = `Status`,
      return = `Return`
    )
}
url <- "https://www.basketball-reference.com/friv/injuries.fcgi"
page <- read_html(url)
page %>%
  html_elements("table") %>%
  length()

x = page %>%
  html_elements("table") %>%
  purrr::map(html_table)

names(x)
