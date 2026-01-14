# scripts/03_analysis_portfolios.R
# Build benchmark portfolios using monthly returns:
# - Equal-weight
# - Minimum-variance (no shorting)
# Compare against FTSE index (^FTSE)

library(tidyverse)
library(quadprog)

# Load monthly returns 
rets <- read_csv("outputs/tables/returns_monthly.csv", show_col_types = FALSE)

stopifnot(all(c("symbol", "month", "return") %in% names(rets)))

# Ensure proper types
rets <- rets %>%
  mutate(
    month = as.Date(month),
    symbol = as.character(symbol),
    return = as.numeric(return)
  )

benchmark_symbol <- "^FTSE"

# Pivot to wide matrix: rows = dates, cols = symbols
rets_wide <- rets %>%
  select(month, symbol, return) %>%
  pivot_wider(names_from = symbol, values_from = return) %>%
  arrange(month)

# Drop rows with missing values (keeps covariance estimation clean)
rets_wide_cc <- rets_wide %>%
  drop_na()

dates <- rets_wide_cc$month

# Split benchmark vs stocks
stopifnot(benchmark_symbol %in% names(rets_wide_cc))

ftse_ret <- rets_wide_cc %>%
  select(all_of(benchmark_symbol)) %>%
  pull()

stocks_mat <- rets_wide_cc %>%
  select(-month, -all_of(benchmark_symbol)) %>%
  as.matrix()

# Portfolio 1: Equal-weight
w_eq <- rep(1 / n, n)

port_eq <- as.numeric(stocks_mat %*% w_eq)

# Portfolio 2: Minimum-variance (no shorting)
# Solve: min w' Σ w
# s.t. sum(w)=1 and w_i >= 0

Sigma <- cov(stocks_mat)
Sigma <- Sigma + diag(1e-6, n)  # small ridge here for numerical stability

# quadprog formulation:
# min 1/2 w' D w - d' w
Dmat <- 2 * Sigma
dvec <- rep(0, n)

# Constraints: sum(w) = 1 (equality) and w >= 0 (inequality)
Amat <- cbind(
  rep(1, n),          # sum(w) = 1
  diag(n)             # w_i >= 0
)
bvec <- c(1, rep(0, n))

sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)

w_minvar <- sol$solution

# Normalise (numerical stability)
w_minvar <- pmax(w_minvar, 0)
w_minvar <- w_minvar / sum(w_minvar)

port_minvar <- as.numeric(stocks_mat %*% w_minvar)

# Assemble returns dataframe 
port_rets <- tibble(
  month = dates,
  FTSE = ftse_ret,
  EqualWeight = port_eq,
  MinVariance = port_minvar
)

# Performance metrics (annualised, monthly data)
ann_factor <- 12

perf <- port_rets %>%
  select(-month) %>%
  summarise(across(everything(), list(
    ann_return = ~ (prod(1 + .x)^(ann_factor / length(.x)) - 1),
    ann_vol = ~ sd(.x) * sqrt(ann_factor),
    sharpe_0rf = ~ ((prod(1 + .x)^(ann_factor / length(.x)) - 1) / (sd(.x) * sqrt(ann_factor))) #rf assumed 0
  ), .names = "{.col}__{.fn}"))

# Tidy the performance  table
perf_tidy <- perf %>%
  pivot_longer(cols = everything(),
               names_to = c("series", "metric"),
               names_sep = "__",
               values_to = "value") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(match(series, c("FTSE", "EqualWeight", "MinVariance")))

# Save output
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Save weights
weights_tbl <- tibble(
  symbol = stock_names,
  w_equal = w_eq,
  w_minvar = w_minvar
) %>%
  arrange(desc(w_minvar))

write_csv(weights_tbl, "outputs/tables/portfolio_weights.csv")
write_csv(port_rets, "outputs/tables/portfolio_returns_monthly.csv")
write_csv(perf_tidy, "outputs/tables/performance_summary.csv")

message("Saved: outputs/tables/portfolio_weights.csv")
message("Saved: outputs/tables/portfolio_returns_monthly.csv")
message("Saved: outputs/tables/performance_summary.csv")
