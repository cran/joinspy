## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(joinspy)

## -----------------------------------------------------------------------------
# Base R data frames: auto-detects "base"
orders_df <- data.frame(
  id = c(1, 2, 3),
  amount = c(100, 250, 75),
  stringsAsFactors = FALSE
)

customers_df <- data.frame(
  id = c(1, 2, 4),
  name = c("Alice", "Bob", "Diana"),
  stringsAsFactors = FALSE
)

result_base <- left_join_spy(orders_df, customers_df, by = "id", .quiet = TRUE)
class(result_base)

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
# Tibbles: auto-detects "dplyr"
orders_tbl <- dplyr::tibble(
  id = c(1, 2, 3),
  amount = c(100, 250, 75)
)

customers_tbl <- dplyr::tibble(
  id = c(1, 2, 4),
  name = c("Alice", "Bob", "Diana")
)

result_dplyr <- left_join_spy(orders_tbl, customers_tbl, by = "id", .quiet = TRUE)
class(result_dplyr)

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
# data.tables: auto-detects "data.table"
orders_dt <- data.table::data.table(
  id = c(1, 2, 3),
  amount = c(100, 250, 75)
)

customers_dt <- data.table::data.table(
  id = c(1, 2, 4),
  name = c("Alice", "Bob", "Diana")
)

result_dt <- left_join_spy(orders_dt, customers_dt, by = "id", .quiet = TRUE)
class(result_dt)

## ----eval = requireNamespace("data.table", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)----
# data.table + tibble: data.table wins
mixed_result <- left_join_spy(orders_dt, customers_tbl, by = "id", .quiet = TRUE)
class(mixed_result)

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
result <- left_join_spy(orders_df, customers_df, by = "id",
                        backend = "dplyr", .quiet = TRUE)
class(result)

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
# These have a legitimate many-to-many relationship
tags <- dplyr::tibble(
  item_id = c(1, 1, 2),
  tag = c("red", "large", "small")
)

prices <- dplyr::tibble(
  item_id = c(1, 2, 2),
  currency = c("USD", "USD", "EUR")
)

# Force base R to avoid dplyr's many-to-many warning
result <- left_join_spy(tags, prices, by = "item_id",
                        backend = "base", .quiet = TRUE)
nrow(result)

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
result <- left_join_spy(orders_df, customers_df, by = "id",
                        backend = "data.table", .quiet = TRUE)
class(result)

## -----------------------------------------------------------------------------
messy_df <- data.frame(
  code = c("A-1 ", "B-2", " C-3"),
  value = c(10, 20, 30),
  stringsAsFactors = FALSE
)

lookup_df <- data.frame(
  code = c("A-1", "B-2", "C-3"),
  label = c("Alpha", "Beta", "Gamma"),
  stringsAsFactors = FALSE
)

# 1. Diagnose
report <- join_spy(messy_df, lookup_df, by = "code")

# 2. Repair
repaired_df <- join_repair(messy_df, by = "code")
class(repaired_df)  # still data.frame

# 3. Join
joined_df <- left_join_spy(repaired_df, lookup_df, by = "code", .quiet = TRUE)
class(joined_df)  # still data.frame
joined_df

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
messy_tbl <- dplyr::tibble(
  code = c("A-1 ", "B-2", " C-3"),
  value = c(10, 20, 30)
)

lookup_tbl <- dplyr::tibble(
  code = c("A-1", "B-2", "C-3"),
  label = c("Alpha", "Beta", "Gamma")
)

repaired_tbl <- join_repair(messy_tbl, by = "code")
class(repaired_tbl)  # still tbl_df

joined_tbl <- left_join_spy(repaired_tbl, lookup_tbl, by = "code", .quiet = TRUE)
class(joined_tbl)  # still tbl_df
joined_tbl

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
messy_dt <- data.table::data.table(
  code = c("A-1 ", "B-2", " C-3"),
  value = c(10, 20, 30)
)

lookup_dt <- data.table::data.table(
  code = c("A-1", "B-2", "C-3"),
  label = c("Alpha", "Beta", "Gamma")
)

repaired_dt <- join_repair(messy_dt, by = "code")
class(repaired_dt)  # still data.table

joined_dt <- left_join_spy(repaired_dt, lookup_dt, by = "code", .quiet = TRUE)
class(joined_dt)  # still data.table
joined_dt

## ----eval = requireNamespace("data.table", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)----
# Diagnose on data.tables
orders_dt <- data.table::data.table(
  id = c(1, 2, 3),
  amount = c(100, 250, 75)
)

customers_dt <- data.table::data.table(
  id = c(1, 2, 4),
  name = c("Alice", "Bob", "Diana")
)

report <- join_spy(orders_dt, customers_dt, by = "id")

# Join with dplyr (convert first)
orders_tbl <- dplyr::as_tibble(orders_dt)
customers_tbl <- dplyr::as_tibble(customers_dt)
result <- left_join_spy(orders_tbl, customers_tbl, by = "id", .quiet = TRUE)
class(result)

