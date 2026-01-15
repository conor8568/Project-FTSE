# scripts/04_make_charts.R

# Create charts:
# 1) Cumulative returns (FTSE vs EqualWeight vs MinVariance)
# 2) Risk-return scatter (annualised, monthly)
# 3) Correlation heatmap (daily returns, stocks only)
# 4) Rolling volatility (daily returns, 63 trading days ~ 3 months)

library(tidyverse)
library(scales)
library(zoo)

# Ensure output folder exists again
dir.create("outputs/charts", showWarnings = FALSE, recursive = TRUE)

# 1) Cumulative returns chart (Monthly portfolios)
port <- read_csv("outputs/tables/portfolio_returns_monthly.csv", show_col_types = FALSE) %>%
  mutate(month = as.Date(month))

stopifnot(all(c("month", "FTSE", "EqualWeight", "MinVariance") %in% names(port)))

cum <- port %>%
  arrange(month) %>%
  mutate(
    FTSE_cum = exp(cumsum(FTSE)),
    EqualWeight_cum = exp(cumsum(EqualWeight)),
    MinVariance_cum = exp(cumsum(MinVariance))
  ) %>%
  select(month, FTSE_cum, EqualWeight_cum, MinVariance_cum) %>%
  pivot_longer(-month, names_to = "series", values_to = "cum_value") %>%
  mutate(series = recode(series,
                         "FTSE_cum" = "FTSE (^FTSE)",
                         "EqualWeight_cum" = "Equal weight",
                         "MinVariance_cum" = "Min variance (long only)"))

p1 <- ggplot(cum, aes(x = month, y = cum_value, color = series)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title = "Cumulative Growth of £1 (Monthly)",
    subtitle = "Benchmark vs constructed portfolios",
    x = NULL,
    y = "Growth of £1",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("outputs/charts/01_cumulative_returns.png", p1, width = 10, height = 6, dpi = 300)

# 2) Risk and return scatter (Annualised, monthly)
ann_factor <- 12

perf_tbl <- port %>%
  select(-month) %>%
  pivot_longer(cols = everything(), names_to = "series", values_to = "r") %>%
  group_by(series) %>%
  summarise(
    ann_return = exp(mean(r) * ann_factor) - 1,
    ann_vol = sd(r) * sqrt(ann_factor),
    sharpe_0rf = (mean(r) * ann_factor) / (sd(r) * sqrt(ann_factor)),
    .groups = "drop"
  ) %>%
  mutate(series = recode(series,
                         "FTSE" = "FTSE (^FTSE)",
                         "EqualWeight" = "Equal-weight",
                         "MinVariance" = "Min-variance (long-only)"))

p2 <- ggplot(perf_tbl, aes(x = ann_vol, y = ann_return, label = series)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.002, nudge_x = 0.002, show.legend = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Risk vs Return (Annualised, Monthly Data)",
    subtitle = "Volatility on x-axis, return on y-axis (Sharpe shown in table output)",
    x = "Annualised Volatility",
    y = "Annualised Return"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/charts/02_risk_return_scatter.png", p2, width = 10, height = 6, dpi = 300)

# 3) Correlation heatmap (Daily returns, stocks only)
rets_d <- read_csv("outputs/tables/returns_daily.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

stopifnot(all(c("symbol", "date", "return") %in% names(rets_d)))

benchmark_symbol <- "^FTSE"

rets_wide_d <- rets_d %>%
  filter(symbol != benchmark_symbol) %>%         # stocks only
  select(date, symbol, return) %>%
  pivot_wider(names_from = symbol, values_from = return) %>%
  arrange(date)

# Pairwise correlations handle missing data better than drop_na()
ret_mat <- rets_wide_d %>%
  select(-date) %>%
  as.matrix()

corr <- cor(ret_mat, use = "pairwise.complete.obs")

# Force dimnames (prevents NA axes)
colnames(corr) <- colnames(ret_mat)
rownames(corr) <- colnames(ret_mat)

corr_long <- as.data.frame(corr) %>%
  rownames_to_column("asset_x") %>%
  pivot_longer(-asset_x, names_to = "asset_y", values_to = "corr")

p3 <- ggplot(corr_long, aes(x = asset_x, y = asset_y, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(labels = number_format(accuracy = 0.1), limits = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap (Daily Returns)",
    subtitle = "Stocks only (excludes ^FTSE)",
    x = NULL,
    y = NULL,
    fill = "Corr"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave("outputs/charts/03_correlation_heatmap.png", p3, width = 10, height = 8, dpi = 300)

# 4) Rolling volatility (Daily returns)
# 63 trading days ~ 3 months. can swap to 252 for 1Y.
window <- 63

roll_vol <- rets_d %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    roll_vol = rollapply(
      data = return,
      width = window,
      FUN = sd,
      fill = NA,
      align = "right"
    ) * sqrt(252)  # annualise daily vol
  ) %>%
  ungroup()

symbols_to_plot <- roll_vol %>%
  distinct(symbol) %>%
  pull() %>%
  unique()

# Put FTSE first
symbols_to_plot <- c(benchmark_symbol, setdiff(symbols_to_plot, benchmark_symbol))
symbols_to_plot <- symbols_to_plot[!is.na(symbols_to_plot)]

#FTSE + 6 assets for readability
symbols_to_plot <- head(symbols_to_plot, 7)

p4_data <- roll_vol %>%
  filter(symbol %in% symbols_to_plot) %>%
  filter(!is.na(roll_vol))

p4 <- ggplot(p4_data, aes(x = date, y = roll_vol, color = symbol)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = paste0("Rolling Volatility (", window, "-day, annualised)"),
    subtitle = "Includes ^FTSE and a subset of assets for readability",
    x = NULL,
    y = "Annualised Volatility",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("outputs/charts/04_rolling_volatility.png", p4, width = 10, height = 6, dpi = 300)

message("Saved charts to outputs/charts/:")
message(" - 01_cumulative_returns.png")
message(" - 02_risk_return_scatter.png")
message(" - 03_correlation_heatmap.png")
message(" - 04_rolling_volatility.png")
