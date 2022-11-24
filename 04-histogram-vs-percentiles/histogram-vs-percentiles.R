library(tidyverse)
library(DBI)
library(RClickhouse)
library(magrittr)

# Clickhouse на localhost
conn <- DBI::dbConnect(RClickhouse::clickhouse())

hist_data <- dbGetQuery(conn, "
  WITH
    histogram(100)(duration) AS histogram
  SELECT
    arrayMap(x -> x.1, histogram) AS interval_begin,
    arrayMap(x -> x.2, histogram) AS interval_end,
    arrayMap(x -> x.3, histogram) AS height
  FROM (
    SELECT
      randLogNormal(10, 1) AS duration
    FROM numbers(100000)
  )
")

percentiles_data <- dbGetQuery(conn, "
  WITH
    quantiles(
      0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2,
      0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.4,
      0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5, 0.51, 0.52, 0.53, 0.54, 0.55, 0.56, 0.57, 0.58, 0.59, 0.6,
      0.61, 0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 0.68, 0.69, 0.7, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79, 0.8,
      0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1
    )(duration) AS percentiles,
    arrayPopBack(percentiles) AS interval_begin,
    arrayPopFront(percentiles) AS interval_end,
    arrayMap((begin, end) -> 0.01 / (end - begin), interval_begin, interval_end) AS height
  SELECT
    interval_begin,
    interval_end,
    height
  FROM (
    SELECT
      randLogNormal(10, 1) AS duration
    FROM numbers(100000)
  )
")

hist_df <- hist_data %$%
  tibble(interval_begin, interval_end, height) %>%
  unnest(everything()) %>%
  mutate(
    type = "histogram",
    height = height / sum(height * (interval_end - interval_begin))
  )

percentile_df <- percentiles_data %$%
  tibble(interval_begin, interval_end, height) %>%
  unnest(everything()) %>%
  ungroup() %>%
  mutate(
    type = "percentiles",
    height = height / sum(height * (interval_end - interval_begin))
  )

hist_df %>%
  bind_rows(percentile_df) %>%
  ggplot(aes(xmin = interval_begin, xmax = interval_end, ymin = 0, ymax = height)) +
  geom_rect(fill = "white", colour = "black") +
  # scale_x_continuous(limits = c(0, 250000)) +
  facet_wrap(~type)
