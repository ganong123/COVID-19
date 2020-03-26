library(tidyverse)
library(lubridate)

path <- "csse_covid_19_data/csse_covid_19_daily_reports/03-"

read_day <- function(date, state, var_name_) {
  var_name <- enquo(var_name_)
  read_csv(str_c(path, date, "-2020.csv")) %>%
    filter(!! var_name == state) %>%
    summarise(n = sum(Confirmed)) %>%
    mutate(day = dmy(str_c(date, " March 2020")))
}

read_month <- function(state) {
  #this function extracts data from March 10 forward
  #Why? assembling state-level data for days before requires further data cleaning
  bind_rows(
    seq(10, 21) %>% map_dfr(read_day, state, `Province/State`),
    seq(22, 25) %>% map_dfr(read_day, state, Province_State)
  ) %>%
    mutate(state = state)
}

df <- c("New York", "Illinois", "Washington", "Florida") %>%
  map_dfr(read_month)

write_csv(df, "ganong/2020_03_25_state_counts.csv")

(df %>%
  ggplot(aes(x = day, y = n, group = state, color = state)) +
  geom_line() +
  labs(x = "", y = "n positive tests", title = "Covid cases by state") +
  scale_y_log10(breaks = c(10, 30, 100, 300, 1000, 3000, 10000, 30000)) +
  scale_x_date(date_breaks = "3 days",
               date_labels = "%b %d")) %>%
  ggsave("ganong/2020_03_25_state_level_plot.png", .)

