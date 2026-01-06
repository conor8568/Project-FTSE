# Packages
library(tidyverse)
library(tidyquant)
library(lubridate)

# Settings
start_date <- as.Date("2015-01-01")

#tickers
tickers <- c("^FTSE",     # FTSE 100 index
             "HSBA.L",    # HSBC
             "BP.L",      # BP 
             "SHEL.L",    # Shell
             "ULVR.L",    # Unilever
             "AZN.L",     # AstraZeneca
             "GSK.L",     # GSK
             "DGE.L",     # Diageo
             "RIO.L",     # Rio Tinto
             "BATS.L",    # British American Tobacco
             "VOD.L",     # Vodafone
             "LSEG.L",    # London Stock Exchange Group
             "NG.L"       # National Grid
)

#adjusted prices
prices_raw <- tq_get(tickers, from = start_date)

#checks
stopifnot(nrow(prices_raw) > 0)
stopifnot(all(c("symbol","date","adjusted") %in% names(prices_raw)))

prices <- prices_raw %>%
  transmute(
    symbol = symbol,
    date = as.Date(date),
    adjusted = as.numeric(adjusted)
  ) %>%
  arrange(symbol, date)

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(prices, "outputs/tables/prices_daily.csv")
