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

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
accounts <- data.frame(id = c(1, 2), balance = c(50, 80))
holders <- data.frame(account_key = c(1, 2), holder = c("Ana", "Ben"))

names(left_join_spy(accounts, holders, by = c("id" = "account_key"),
                    backend = "data.table", .quiet = TRUE))

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

result <- left_join_spy(tags, prices, by = "item_id", .quiet = TRUE)
nrow(result)

## ----error = TRUE, eval = requireNamespace("dplyr", quietly = TRUE)-----------
try({
left_join_spy(tags, prices, by = "item_id", .quiet = TRUE,
              relationship = "one-to-one")
})

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

## -----------------------------------------------------------------------------
dup_left <- data.frame(id = c(1, 2), status = c("open", "closed"))
dup_right <- data.frame(id = c(1, 2), status = c("new", "old"))

names(left_join_spy(dup_left, dup_right, by = "id", .quiet = TRUE))

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
names(left_join_spy(dup_left, dup_right, by = "id",
                    backend = "dplyr", .quiet = TRUE))

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
names(left_join_spy(dup_left, dup_right, by = "id",
                    backend = "data.table", .quiet = TRUE))

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
names(left_join_spy(dup_left, dup_right, by = "id", backend = "dplyr",
                    .quiet = TRUE, suffix = c("_current", "_incoming")))

## -----------------------------------------------------------------------------
lhs <- data.frame(id = c(3, 1, 2), amount = c(30, 10, 20))
rhs <- data.frame(id = c(1, 2, 3), name = c("a", "b", "c"))

left_join_spy(lhs, rhs, by = "id", .quiet = TRUE)$id

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
left_join_spy(lhs, rhs, by = "id", backend = "dplyr", .quiet = TRUE)$id

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
left_join_spy(lhs, rhs, by = "id", backend = "data.table", .quiet = TRUE)$id

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
left_join_spy(lhs, rhs, by = "id", backend = "data.table",
              .quiet = TRUE, sort = FALSE)$id

## -----------------------------------------------------------------------------
na_left <- data.frame(code = c("A", NA), v = c(1, 2))
na_right <- data.frame(code = c("A", NA), label = c("alpha", "missing"))

nrow(inner_join_spy(na_left, na_right, by = "code", .quiet = TRUE))

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
nrow(inner_join_spy(na_left, na_right, by = "code",
                    backend = "dplyr", .quiet = TRUE))

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
inner_join_spy(na_left, na_right, by = "code",
               backend = "data.table", .quiet = TRUE)

## -----------------------------------------------------------------------------
report <- join_spy(na_left, na_right, by = "code")
report$expected_rows$inner

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
nrow(inner_join_spy(na_left, na_right, by = "code", backend = "dplyr",
                    .quiet = TRUE, na_matches = "never"))

## -----------------------------------------------------------------------------
nrow(inner_join_spy(na_left, na_right, by = "code", backend = "base",
                    .quiet = TRUE, incomparables = NA))

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
invisible(left_join_spy(orders_df, customers_df, by = "id",
                        backend = "base", .quiet = TRUE))
report_base <- last_report()

invisible(left_join_spy(orders_df, customers_df, by = "id",
                        backend = "dplyr", .quiet = TRUE))
report_dplyr <- last_report()

identical(report_base$expected_rows, report_dplyr$expected_rows)

## -----------------------------------------------------------------------------
result <- left_join_spy(orders_df, customers_df, by = "id", .quiet = TRUE)
is_join_report(attr(result, "join_report"))

## ----error = TRUE-------------------------------------------------------------
try({
products <- data.frame(id = 1:3, product = c("A", "B", "C"))
suppliers_dup <- data.frame(id = c(1, 1, 2), name = c("S1", "S2", "S3"))

join_strict(products, suppliers_dup, by = "id", expect = "1:1",
            backend = "data.table")
})

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
products_dt <- data.table::data.table(id = 1:3, product = c("A", "B", "C"))
suppliers_dt <- data.table::data.table(id = 1:3, name = c("S1", "S2", "S3"))

class(join_strict(products_dt, suppliers_dt, by = "id", expect = "1:1"))

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

## ----eval = requireNamespace("dplyr", quietly = TRUE) && requireNamespace("data.table", quietly = TRUE)----
set.seed(42)
n <- 5000
big_x <- data.frame(id = sample(n), x = rnorm(n))
big_y <- data.frame(id = sample(n), y = rnorm(n))

t_base <- system.time(left_join_spy(big_x, big_y, by = "id",
                                    backend = "base", .quiet = TRUE))
t_dplyr <- system.time(left_join_spy(big_x, big_y, by = "id",
                                     backend = "dplyr", .quiet = TRUE))
t_dt <- system.time(left_join_spy(big_x, big_y, by = "id",
                                  backend = "data.table", .quiet = TRUE))
t_spy <- system.time(report <- join_spy(big_x, big_y, by = "id"))

round(c(base = t_base[["elapsed"]], dplyr = t_dplyr[["elapsed"]],
        data.table = t_dt[["elapsed"]], diagnostics = t_spy[["elapsed"]]), 3)

## ----eval = requireNamespace("data.table", quietly = TRUE)--------------------
class(left_join_spy(orders_dt, customers_dt, by = "id",
                    backend = "base", .quiet = TRUE))

