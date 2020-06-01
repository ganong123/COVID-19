library(tidyverse)
library(lubridate)

path <- "csse_covid_19_data/csse_covid_19_daily_reports/03-"

count_cases_one_day <- function(day_of_month, state, var_name) {
  read_csv(str_c(path, day_of_month, "-2020.csv")) %>%
    filter({{var_name}} == state) %>%
    summarise(n = sum(Confirmed)) %>%
    mutate(day = dmy(str_c(day_of_month, " March 2020")))
}

count_cases_all_days <- function(state) {
  #this function uses count_cases_one_day to extract data from March 10 forward
  #Why not earlier than March 10?
  #assembling state-level data for days before requires further data cleaning
  bind_rows(
    seq(10, 21) %>% map_dfr(count_cases_one_day, state, `Province/State`),
    seq(22, 25) %>% map_dfr(count_cases_one_day, state, Province_State)
  ) %>%
    mutate(state = state)
}

cases_daily_4state <-
  c("New York", "Illinois", "Washington", "Florida") %>%
  map_dfr(count_cases_all_days)

write_csv(cases_daily_4state, "ganong/2020_03_25_state_counts.csv")

(cases_daily_4state %>%
  ggplot(aes(x = day, y = n, group = state, color = state)) +
  geom_line() +
  labs(x = "", y = "n positive tests", title = "Covid cases by state") +
  scale_y_log10(breaks = c(10, 30, 100, 300, 1000, 3000, 10000, 30000)) +
  scale_x_date(date_breaks = "3 days",
               date_labels = "%b %d")) %>%
  ggsave("ganong/2020_03_25_state_level_plot.png", .)
