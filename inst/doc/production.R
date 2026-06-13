## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(joinspy)

## -----------------------------------------------------------------------------
orders <- data.frame(
  customer_id = c("C01", "C02", "C03", "C04"),
  amount = c(100, 200, 150, 300),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  customer_id = c("C01", "C02", "C03", "C04"),
  region = c("East", "West", "East", "North"),
  stringsAsFactors = FALSE
)

# This passes -- keys are clean
stopifnot(key_check(orders, customers, by = "customer_id", warn = FALSE))

## ----error = TRUE-------------------------------------------------------------
try({
orders_dirty <- data.frame(
  customer_id = c("C01", "C02 ", "C03 ", "C04"),
  amount = c(100, 200, 150, 300),
  stringsAsFactors = FALSE
)

stopifnot(key_check(orders_dirty, customers, by = "customer_id", warn = FALSE))
})

## ----error = TRUE-------------------------------------------------------------
try({
if (!key_check(orders_dirty, customers, by = "customer_id", warn = FALSE)) {
  stop("Key quality check failed for orders-customers join. ",
       "Run join_spy() interactively for details.", call. = FALSE)
}
})

## -----------------------------------------------------------------------------
ok <- key_check(orders_dirty, customers, by = "customer_id", warn = FALSE)

if (!ok) {
  repaired <- join_repair(
    orders_dirty, customers,
    by = "customer_id",
    trim_whitespace = TRUE,
    remove_invisible = TRUE
  )
  orders_clean <- repaired$x
  customers_clean <- repaired$y

  # Re-check after repair
  stopifnot(key_check(orders_clean, customers_clean,
                       by = "customer_id", warn = FALSE))
}

## -----------------------------------------------------------------------------
sensors <- data.frame(
  sensor_id = c("S01", "S02", "S03", "S04"),
  location = c("Roof", "Basement", "Lobby", "Garage"),
  stringsAsFactors = FALSE
)

readings <- data.frame(
  sensor_id = c("S01", "S02", "S03", "S05"),
  temperature = c(22.1, 18.5, 21.0, 19.3),
  stringsAsFactors = FALSE
)

# Nothing printed
result <- left_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)

## -----------------------------------------------------------------------------
rpt <- last_report()
rpt$match_analysis$match_rate

## -----------------------------------------------------------------------------
rpt <- last_report()
if (rpt$match_analysis$match_rate < 0.95) {
  warning(sprintf(
    "Low match rate (%.1f%%) in sensor join -- check for missing sensor IDs",
    rpt$match_analysis$match_rate * 100
  ))
}

## -----------------------------------------------------------------------------
result1 <- left_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)
report1 <- last_report()

# ... later ...
result2 <- inner_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)
report2 <- last_report()

## -----------------------------------------------------------------------------
identical(attr(result1, "join_report"), report1)

## -----------------------------------------------------------------------------
rpt <- join_spy(orders_dirty, customers, by = "customer_id")
names(rpt)

## -----------------------------------------------------------------------------
rpt$x_summary$n_duplicates
rpt$match_analysis$match_rate
rpt$expected_rows$left

## -----------------------------------------------------------------------------
length(rpt$issues)
vapply(rpt$issues, function(i) i$type, character(1))

## -----------------------------------------------------------------------------
severities <- vapply(rpt$issues, function(i) i$severity, character(1))
severities
sum(severities == "warning")

## -----------------------------------------------------------------------------
report_gate <- function(rpt, min_match = 0.95) {
  stopifnot(is_join_report(rpt))
  problems <- character(0)
  if (rpt$match_analysis$match_rate < min_match) {
    problems <- c(problems, sprintf(
      "match rate %.0f%% below %.0f%%",
      100 * rpt$match_analysis$match_rate, 100 * min_match
    ))
  }
  sev <- vapply(rpt$issues, function(i) i$severity, character(1))
  if (any(sev == "warning")) {
    problems <- c(problems, sprintf("%d warning-level issue(s)",
                                    sum(sev == "warning")))
  }
  problems
}

report_gate(rpt)

## -----------------------------------------------------------------------------
summary(rpt)

## -----------------------------------------------------------------------------
products <- data.frame(
  product_id = c("P1", "P2", "P3"),
  name = c("Widget", "Gadget", "Gizmo"),
  stringsAsFactors = FALSE
)

line_items <- data.frame(
  product_id = c("P1", "P1", "P2", "P3", "P3"),
  order_id = c(101, 102, 103, 104, 105),
  stringsAsFactors = FALSE
)

detect_cardinality(products, line_items, by = "product_id")

## -----------------------------------------------------------------------------
result <- join_strict(
  products, line_items,
  by = "product_id",
  type = "left",
  expect = "1:n"
)
nrow(result)

## ----error = TRUE-------------------------------------------------------------
try({
products_bad <- data.frame(
  product_id = c("P1", "P1", "P2", "P3"),
  name = c("Widget", "Widget v2", "Gadget", "Gizmo"),
  stringsAsFactors = FALSE
)

join_strict(
  products_bad, line_items,
  by = "product_id",
  type = "left",
  expect = "1:n"
)
})

## -----------------------------------------------------------------------------
events_a <- data.frame(id = rep(c("E1", "E2"), each = 20), src = "a",
                       stringsAsFactors = FALSE)
events_b <- data.frame(id = rep(c("E1", "E2"), each = 20), src = "b",
                       stringsAsFactors = FALSE)

chk <- check_cartesian(events_a, events_b, by = "id")
chk$expansion_factor

## ----eval = requireNamespace("testthat", quietly = TRUE)----------------------
library(testthat)

test_that("orders join customers cleanly on customer_id", {
  expect_true(key_check(orders, customers, by = "customer_id", warn = FALSE))
})

## ----eval = requireNamespace("testthat", quietly = TRUE)----------------------
test_that("products to line_items is one-to-many", {
  expect_identical(
    detect_cardinality(products, line_items, by = "product_id"),
    "1:n"
  )
})

## ----eval = requireNamespace("testthat", quietly = TRUE)----------------------
test_that("left join is predicted to preserve order rows", {
  rpt <- join_spy(orders, customers, by = "customer_id")
  expect_equal(rpt$expected_rows$left, nrow(orders))
})

## -----------------------------------------------------------------------------
report <- join_spy(sensors, readings, by = "sensor_id")

# Text format -- human-readable
txt_log <- tempfile(fileext = ".log")
log_report(report, txt_log)
cat(readLines(txt_log), sep = "\n")
unlink(txt_log)

## -----------------------------------------------------------------------------
# JSON format -- machine-readable
json_log <- tempfile(fileext = ".json")
log_report(report, json_log)
cat(readLines(json_log), sep = "\n")
unlink(json_log)

## -----------------------------------------------------------------------------
auto_log <- tempfile(fileext = ".log")
set_log_file(auto_log, format = "text")

# These joins are automatically logged
result1 <- left_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)
result2 <- inner_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)

# Check what got logged
cat(readLines(auto_log), sep = "\n")

# Clean up
set_log_file(NULL)
unlink(auto_log)

## -----------------------------------------------------------------------------
fn_log <- tempfile(fileext = ".log")
previous <- set_log_file(fn_log)

result <- left_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)

set_log_file(previous)
unlink(fn_log)

## -----------------------------------------------------------------------------
# Only log if logging is configured
if (!is.null(get_log_file())) {
  message("Logging is active at: ", get_log_file())
}

## -----------------------------------------------------------------------------
json_log <- tempfile(fileext = ".json")
set_log_file(json_log, format = "json")

r1 <- left_join_spy(sensors, readings, by = "sensor_id", .quiet = TRUE)
r2 <- inner_join_spy(orders, customers, by = "customer_id", .quiet = TRUE)

set_log_file(NULL)

## -----------------------------------------------------------------------------
log_lines <- readLines(json_log)
rate_lines <- grep('"match_rate"', log_lines, value = TRUE)
rate_lines

## -----------------------------------------------------------------------------
rates <- as.numeric(sub('.*"match_rate": *([0-9.]+).*', "\\1", rate_lines))
rates
which(rates < 0.95)
unlink(json_log)

## -----------------------------------------------------------------------------
orders_chain <- data.frame(
  order_id = 1:6,
  customer_id = c("C1", "C2", "C2", "C3", "C4", "C4"),
  stringsAsFactors = FALSE
)

customers_chain <- data.frame(
  customer_id = c("C1", "C2", "C3", "C4"),
  region_id = c("R1", "R1", "R2", "R3"),
  stringsAsFactors = FALSE
)

regions <- data.frame(
  region_id = c("R1", "R2"),
  region_name = c("North", "South"),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
chain <- analyze_join_chain(
  tables = list(orders = orders_chain, customers = customers_chain,
                regions = regions),
  joins = list(
    list(left = "orders", right = "customers", by = "customer_id"),
    list(left = "result", right = "regions", by = "region_id")
  )
)

## -----------------------------------------------------------------------------
chain[[2]]$report$match_analysis$match_rate
chain[[2]]$report$match_analysis$left_only_keys

## -----------------------------------------------------------------------------
vapply(chain, function(s) length(s$report$issues), integer(1))

## -----------------------------------------------------------------------------
# Simulate a large dataset
set.seed(42)
big_orders <- data.frame(
  customer_id = sample(paste0("C", sprintf("%04d", 1:5000)), 50000, replace = TRUE),
  amount = round(runif(50000, 10, 500), 2),
  stringsAsFactors = FALSE
)

big_customers <- data.frame(
  customer_id = paste0("C", sprintf("%04d", 1:6000)),
  region = sample(c("North", "South", "East", "West"), 6000, replace = TRUE),
  stringsAsFactors = FALSE
)

# Full analysis
system.time(report_full <- join_spy(big_orders, big_customers, by = "customer_id"))

# Sampled analysis
system.time(report_sampled <- join_spy(big_orders, big_customers,
                                        by = "customer_id", sample = 5000))

## -----------------------------------------------------------------------------
report_sampled$sampling

## -----------------------------------------------------------------------------
# ============================================================
# Nightly order enrichment pipeline
# ============================================================

# --- Setup logging ---
pipeline_log <- tempfile(fileext = ".log")
set_log_file(pipeline_log, format = "text")

# --- Load data (simulated) ---
orders <- data.frame(
  order_id = 1:6,
  customer_id = c("C001", "C002 ", "C003", "C003", "C004", "C005"),
  amount = c(150, 230, 89, 410, 320, 175),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  customer_id = c("C001", "C002", "C003", "C004", "C005", "C006"),
  name = c("Acme Corp", "Globex", "Initech", "Umbrella", "Soylent", "Wonka"),
  tier = c("gold", "silver", "gold", "bronze", "silver", "gold"),
  stringsAsFactors = FALSE
)

# --- Gate 1: key quality assertion ---
keys_ok <- key_check(orders, customers, by = "customer_id", warn = FALSE)

if (!keys_ok) {
  message("Key issues detected -- attempting repair")
  repaired <- join_repair(
    orders, customers,
    by = "customer_id",
    trim_whitespace = TRUE,
    remove_invisible = TRUE
  )
  orders <- repaired$x
  customers <- repaired$y
}

# --- Gate 2: cardinality check ---
card <- detect_cardinality(orders, customers, by = "customer_id")
if (card == "n:m") {
  set_log_file(NULL)
  unlink(pipeline_log)
  stop("Unexpected n:m cardinality in orders-customers join", call. = FALSE)
}

# --- Join (with auto-logging via *_join_spy) ---
enriched <- left_join_spy(orders, customers, by = "customer_id", .quiet = TRUE)

# --- Gate 3: row count sanity check ---
# A left join should never lose rows from the left table
if (nrow(enriched) < nrow(orders)) {
  set_log_file(NULL)
  unlink(pipeline_log)
  stop("Row count decreased after left join -- possible data corruption",
       call. = FALSE)
}

# --- Output ---
message(sprintf("Pipeline complete: %d enriched orders", nrow(enriched)))
head(enriched)

# --- Review the log ---
if (file.exists(pipeline_log)) {
  cat(readLines(pipeline_log), sep = "\n")
}

# --- Cleanup ---
set_log_file(NULL)
unlink(pipeline_log)

## -----------------------------------------------------------------------------
t_merge <- system.time(
  merge(big_orders, big_customers, by = "customer_id", all.x = TRUE)
)
t_spy <- system.time(
  left_join_spy(big_orders, big_customers, by = "customer_id", .quiet = TRUE)
)
t_check <- system.time(
  key_check(big_orders, big_customers, by = "customer_id", warn = FALSE)
)

rbind(merge = t_merge, left_join_spy = t_spy, key_check = t_check)[, 1:3]

