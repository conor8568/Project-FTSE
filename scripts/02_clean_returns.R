# scripts/02_clean_returns.R
# Clean price data and compute returns
# Produces daily and monthly log returns

library(tidyverse)
library(lubridate)

# Create output folders if they don't exist
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Load prices
prices <- read_csv("outputs/tables/prices_daily.csv", show_col_types = FALSE)

stopifnot(all(c("symbol", "date", "adjusted") %in% names(prices)))

prices <- prices %>%
  mutate(
    date = as.Date(date),
    adjusted = as.numeric(adjusted),
    symbol = as.character(symbol)
  ) %>%
  arrange(symbol, date)

stopifnot(all(prices$adjusted > 0, na.rm = TRUE))

# Daily log returns
returns_daily <- prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(return = log(adjusted / dplyr::lag(adjusted))) %>%
  ungroup() %>%
  filter(!is.na(return)) %>%
  filter(is.finite(return)) %>%
  select(symbol, date, return)

# Monthly log returns (month-end prices)
returns_monthly <- prices %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  arrange(date, .by_group = TRUE) %>%  
  summarise(price = dplyr::last(adjusted), .groups = "drop") %>%
  group_by(symbol) %>%
  arrange(month) %>%
  mutate(return = log(price / dplyr::lag(price))) %>%
  ungroup() %>%
  filter(!is.na(return)) %>%
  filter(is.finite(return)) %>%
  select(symbol, month, return)

# Save
write_csv(returns_daily, "outputs/tables/returns_daily.csv")
write_csv(returns_monthly, "outputs/tables/returns_monthly.csv")

message("Saved: outputs/tables/returns_daily.csv")
message("Saved: outputs/tables/returns_monthly.csv")
