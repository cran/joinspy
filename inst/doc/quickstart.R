## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(joinspy)

## -----------------------------------------------------------------------------
sales <- data.frame(
  city = c("London", "Paris ", " Berlin", "Tokyo"),
  revenue = c(500, 300, 450, 600),
  stringsAsFactors = FALSE
)

cities <- data.frame(
  city = c("London", "Paris", "Berlin", "Tokyo", "Madrid"),
  country = c("UK", "France", "Germany", "Japan", "Spain"),
  stringsAsFactors = FALSE
)

report <- join_spy(sales, cities, by = "city")

## -----------------------------------------------------------------------------
sales2 <- data.frame(
  city = c("London", "Paris ", "Berlin"),
  district = c("West ", "Central", " Mitte"),
  revenue = c(500, 300, 450),
  stringsAsFactors = FALSE
)

districts <- data.frame(
  city = c("London", "Paris", "Berlin"),
  district = c("West", "Central", "Mitte"),
  pop = c(200000, 350000, 180000),
  stringsAsFactors = FALSE
)

report <- join_spy(sales2, districts, by = c("city", "district"))

## -----------------------------------------------------------------------------
key_check(sales, cities, by = "city")

## -----------------------------------------------------------------------------
invoices <- data.frame(
  company = c("ACME", "globex", "Initech", "UMBRELLA"),
  amount = c(1200, 800, 950, 1500),
  stringsAsFactors = FALSE
)

vendors <- data.frame(
  company = c("Acme", "Globex", "Initech", "Umbrella"),
  sector = c("Manufacturing", "Logistics", "Software", "Biotech"),
  stringsAsFactors = FALSE
)

report <- join_spy(invoices, vendors, by = "company")

## -----------------------------------------------------------------------------
# Simulate invisible character contamination
raw_ids <- data.frame(
  product_id = c("SKU-001", "SKU-002", paste0("SKU-003", "\u200B")),
  batch = c("A", "B", "C"),
  stringsAsFactors = FALSE
)

clean_ids <- data.frame(
  product_id = c("SKU-001", "SKU-002", "SKU-003"),
  warehouse = c("East", "West", "North"),
  stringsAsFactors = FALSE
)

report <- join_spy(raw_ids, clean_ids, by = "product_id")

## -----------------------------------------------------------------------------
employees <- data.frame(
  dept = c("Sales ", "ENGINEERING", "marketing", paste0("HR", "\u00A0")),
  name = c("Alice", "Bob", "Carol", "David"),
  stringsAsFactors = FALSE
)

departments <- data.frame(
  dept = c("Sales", "Engineering", "Marketing", "HR"),
  budget = c(50000, 80000, 45000, 30000),
  stringsAsFactors = FALSE
)

report <- join_spy(employees, departments, by = "dept")

## -----------------------------------------------------------------------------
transactions <- data.frame(
  store_id = c("S1", "S1", "S2", "S3", "S3", "S3"),
  day = c("Mon", "Tue", "Mon", "Mon", "Tue", "Wed"),
  amount = c(100, 200, 150, 300, 250, 400),
  stringsAsFactors = FALSE
)

key_duplicates(transactions, by = "store_id")

## -----------------------------------------------------------------------------
messy_left <- data.frame(
  id = c(" A-101", "B-202 ", "c-303", paste0("D-404", "\u200B")),
  score = c(88, 92, 76, 95),
  stringsAsFactors = FALSE
)

messy_right <- data.frame(
  id = c("A-101", "B-202", "C-303", "D-404"),
  label = c("Alpha", "Beta", "Gamma", "Delta"),
  stringsAsFactors = FALSE
)

join_repair(messy_left, messy_right, by = "id", dry_run = TRUE)

## -----------------------------------------------------------------------------
repaired <- join_repair(
  messy_left, messy_right,
  by = "id",
  trim_whitespace = TRUE,
  standardize_case = "upper",
  remove_invisible = TRUE
)

repaired$x$id
repaired$y$id

## -----------------------------------------------------------------------------
key_check(repaired$x, repaired$y, by = "id")

## -----------------------------------------------------------------------------
report <- join_spy(messy_left, messy_right, by = "id")
suggest_repairs(report)

## -----------------------------------------------------------------------------
orders <- data.frame(
  product_id = c("P1", "P1", "P2", "P3", "P4"),
  quantity = c(10, 5, 20, 15, 8),
  stringsAsFactors = FALSE
)

products <- data.frame(
  product_id = c("P1", "P2", "P3", "P5"),
  name = c("Widget", "Gadget", "Gizmo", "Doohickey"),
  stringsAsFactors = FALSE
)

report <- join_spy(orders, products, by = "product_id")
report$expected_rows

## -----------------------------------------------------------------------------
summary(report)

## -----------------------------------------------------------------------------
tickets <- data.frame(
  event_id = c(1, 2, 2, 3),
  seat = c("A1", "B2", "B3", "C1"),
  stringsAsFactors = FALSE
)

events <- data.frame(
  event_id = c(1, 2, 4),
  name = c("Concert", "Play", "Opera"),
  stringsAsFactors = FALSE
)

result <- merge(tickets, events, by = "event_id")
join_explain(result, tickets, events, by = "event_id", type = "inner")

## -----------------------------------------------------------------------------
before <- data.frame(
  id = 1:4,
  value = c("a", "b", "c", "d"),
  stringsAsFactors = FALSE
)

lookup <- data.frame(
  id = c(2, 3, 4, 5),
  extra = c("X", "Y", "Z", "W"),
  stringsAsFactors = FALSE
)

after <- merge(before, lookup, by = "id", all = TRUE)
join_diff(before, after, by = "id")

## -----------------------------------------------------------------------------
before2 <- data.frame(
  id = c(1, 2, 3),
  value = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

dup_lookup <- data.frame(
  id = c(1, 1, 2, 3),
  tag = c("X", "Y", "Z", "W"),
  stringsAsFactors = FALSE
)

after2 <- merge(before2, dup_lookup, by = "id")
join_diff(before2, after2, by = "id")

## -----------------------------------------------------------------------------
patients <- data.frame(
  patient_id = c("P01", "P02", "P03"),
  age = c(34, 28, 45),
  stringsAsFactors = FALSE
)

labs <- data.frame(
  patient_id = c("P01", "P02", "P04"),
  result = c(5.2, 3.8, 6.1),
  stringsAsFactors = FALSE
)

result <- left_join_spy(patients, labs, by = "patient_id")
head(result)

## -----------------------------------------------------------------------------
result <- inner_join_spy(patients, labs, by = "patient_id", .quiet = TRUE)

# Later, inspect what happened
rpt <- last_report()
rpt$match_analysis$match_rate

## -----------------------------------------------------------------------------
sensors <- data.frame(
  sensor_id = c("T1", "T2", "T3"),
  location = c("Roof", "Basement", "Lobby"),
  stringsAsFactors = FALSE
)

readings <- data.frame(
  sensor_id = c("T1", "T2", "T3"),
  value = c(22.1, 18.5, 21.0),
  stringsAsFactors = FALSE
)

# Succeeds: one reading per sensor
join_strict(sensors, readings, by = "sensor_id", expect = "1:1")

## ----error = TRUE-------------------------------------------------------------
try({
# Fails: T1 has two readings, violating 1:1
readings_dup <- data.frame(
  sensor_id = c("T1", "T1", "T2", "T3"),
  value = c(22.1, 23.4, 18.5, 21.0),
  stringsAsFactors = FALSE
)

join_strict(sensors, readings_dup, by = "sensor_id", expect = "1:1")
})

## -----------------------------------------------------------------------------
detect_cardinality(sensors, readings_dup, by = "sensor_id")

## -----------------------------------------------------------------------------
left <- data.frame(
  group = c("A", "A", "A", "B"),
  x = 1:4,
  stringsAsFactors = FALSE
)

right <- data.frame(
  group = c("A", "A", "B", "B"),
  y = 5:8,
  stringsAsFactors = FALSE
)

check_cartesian(left, right, by = "group")

## -----------------------------------------------------------------------------
orders <- data.frame(
  order_id = 1:4,
  customer_id = c(1, 2, 2, 3),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  customer_id = 1:3,
  region_id = c(1, 1, 2),
  stringsAsFactors = FALSE
)

regions <- data.frame(
  region_id = 1:2,
  name = c("North", "South"),
  stringsAsFactors = FALSE
)

analyze_join_chain(
  tables = list(orders = orders, customers = customers, regions = regions),
  joins = list(
    list(left = "orders", right = "customers", by = "customer_id"),
    list(left = "result", right = "regions", by = "region_id")
  )
)

## ----eval = FALSE-------------------------------------------------------------
# # Force dplyr backend even for plain data frames
# left_join_spy(x, y, by = "id", backend = "dplyr")
# 
# # Force base R for tibbles
# left_join_spy(x, y, by = "id", backend = "base")

