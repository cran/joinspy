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
  product = c("Widget", "Gadget ", " Gizmo"),
  units = c(10, 20, 30),
  stringsAsFactors = FALSE
)

inventory <- data.frame(
  product = c("Widget", "Gadget", "Gizmo"),
  stock = c(100, 200, 300),
  stringsAsFactors = FALSE
)

join_spy(sales, inventory, by = "product")

## -----------------------------------------------------------------------------
sales_clean <- join_repair(sales, by = "product")
key_check(sales_clean, inventory, by = "product")

## -----------------------------------------------------------------------------
shipments <- data.frame(
  warehouse = c("East ", "West", "East "),
  product   = c("Widget", "Gadget ", "Gizmo"),
  shipped   = c(50, 80, 35),
  stringsAsFactors = FALSE
)

stock <- data.frame(
  warehouse = c("East", "West", "East"),
  product   = c("Widget", "Gadget", "Gizmo"),
  on_hand   = c(200, 150, 90),
  stringsAsFactors = FALSE
)

join_spy(shipments, stock, by = c("warehouse", "product"))

## -----------------------------------------------------------------------------
shipments_clean <- join_repair(shipments, by = c("warehouse", "product"))
key_check(shipments_clean, stock, by = c("warehouse", "product"))

## -----------------------------------------------------------------------------
sensors <- data.frame(
  station = c("AWS-01", "aws-02", "Aws-03"),
  temp = c(22.1, 18.4, 25.7),
  stringsAsFactors = FALSE
)

metadata <- data.frame(
  station = c("aws-01", "AWS-02", "AWS-03"),
  region = c("North", "South", "East"),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
join_spy(sensors, metadata, by = "station")

## -----------------------------------------------------------------------------
repaired <- join_repair(sensors, metadata, by = "station", standardize_case = "lower")
key_check(repaired$x, repaired$y, by = "station")

## -----------------------------------------------------------------------------
# Simulate a non-breaking space in one key
left <- data.frame(
  city = c("New York", "Los\u00a0Angeles", "Chicago"),
  pop = c(8.3, 3.9, 2.7),
  stringsAsFactors = FALSE
)

right <- data.frame(
  city = c("New York", "Los Angeles", "Chicago"),
  area = c(302, 469, 227),
  stringsAsFactors = FALSE
)

join_spy(left, right, by = "city")

## -----------------------------------------------------------------------------
left_fixed <- join_repair(left, by = "city")
key_check(left_fixed, right, by = "city")

## -----------------------------------------------------------------------------
patients <- data.frame(
  mrn = c("P001", "", "P003"),
  age = c(34, 56, 29),
  stringsAsFactors = FALSE
)

visits <- data.frame(
  mrn = c("P001", "P002", ""),
  date = c("2024-01-10", "2024-02-15", "2024-03-20"),
  stringsAsFactors = FALSE
)

join_spy(patients, visits, by = "mrn")

## -----------------------------------------------------------------------------
patients_fixed <- join_repair(patients, by = "mrn", empty_to_na = TRUE)
patients_fixed$mrn

## -----------------------------------------------------------------------------
orders <- data.frame(
  sku = c("WDG-100", "GDG-200", "GZM-300"),
  qty = c(5, 12, 8),
  stringsAsFactors = FALSE
)

catalog <- data.frame(
  sku = c("WDG-100", "GDG-200", "GZM-301"),
  price = c(9.99, 14.99, 7.50),
  stringsAsFactors = FALSE
)

report <- join_spy(orders, catalog, by = "sku")

## -----------------------------------------------------------------------------
employees <- data.frame(
  name = c("Johnson", "Smithe", "O'Brian", "Williams"),
  dept = c("Sales", "R&D", "Ops", "HR"),
  stringsAsFactors = FALSE
)

payroll <- data.frame(
  name = c("Jonhson", "Smith", "O'Brien", "Williams"),
  salary = c(55000, 62000, 48000, 71000),
  stringsAsFactors = FALSE
)

report <- join_spy(employees, payroll, by = "name")

## -----------------------------------------------------------------------------
orders <- data.frame(
  customer_id = c(1, 2, 3),
  amount = c(100, 250, 75)
)

addresses <- data.frame(
  customer_id = c(1, 2, 2, 3),
  address = c("NYC", "LA", "SF", "Chicago"),
  stringsAsFactors = FALSE
)

join_spy(orders, addresses, by = "customer_id")

## -----------------------------------------------------------------------------
key_duplicates(addresses, by = "customer_id")

## -----------------------------------------------------------------------------
orders_dup <- data.frame(
  product = c("A", "A", "B", "B"),
  qty     = c(10, 20, 5, 15)
)

prices_dup <- data.frame(
  product = c("A", "A", "A", "B", "B"),
  price   = c(1.0, 1.1, 1.2, 2.0, 2.5)
)

join_spy(orders_dup, prices_dup, by = "product")

## -----------------------------------------------------------------------------
check_cartesian(orders_dup, prices_dup, by = "product")

## -----------------------------------------------------------------------------
orders <- data.frame(
  customer_id = c(1, NA, 3, NA),
  amount = c(100, 200, 300, 400)
)

customers <- data.frame(
  customer_id = c(1, 2, 3, NA),
  name = c("Alice", "Bob", "Carol", "Unknown"),
  stringsAsFactors = FALSE
)

join_spy(orders, customers, by = "customer_id")

## -----------------------------------------------------------------------------
# Remove
orders_clean <- orders[!is.na(orders$customer_id), ]
key_check(orders_clean, customers, by = "customer_id")

## -----------------------------------------------------------------------------
invoices <- data.frame(
  product_id = c(1, 2, 3),
  total = c(500, 300, 150)
)

products <- data.frame(
  product_id = c("1", "2", "3"),
  name = c("Widget", "Gadget", "Gizmo"),
  stringsAsFactors = FALSE
)

join_spy(invoices, products, by = "product_id")

## -----------------------------------------------------------------------------
invoices$product_id <- as.character(invoices$product_id)
key_check(invoices, products, by = "product_id")

## -----------------------------------------------------------------------------
items <- data.frame(
  order_id = c(1, 1, 2, 2, 2),
  item = c("A", "B", "C", "D", "E"),
  stringsAsFactors = FALSE
)

payments <- data.frame(
  order_id = c(1, 1, 2, 2),
  method = c("Card", "Cash", "Card", "Wire"),
  stringsAsFactors = FALSE
)

check_cartesian(items, payments, by = "order_id")

## -----------------------------------------------------------------------------
detect_cardinality(items, payments, by = "order_id")

## ----error = TRUE-------------------------------------------------------------
try({
join_strict(items, payments, by = "order_id", type = "left", expect = "1:m")
})

## -----------------------------------------------------------------------------
system_a <- data.frame(
  user_id = c("USR-001", "USR-002", "USR-003"),
  score = c(85, 90, 78),
  stringsAsFactors = FALSE
)

system_b <- data.frame(
  user_id = c("1", "2", "3"),
  dept = c("Sales", "R&D", "Ops"),
  stringsAsFactors = FALSE
)

join_spy(system_a, system_b, by = "user_id")

## -----------------------------------------------------------------------------
system_a$user_num <- gsub("^USR-0*", "", system_a$user_id)
key_check(system_a, system_b, by = c("user_num" = "user_id"))

## ----eval = FALSE-------------------------------------------------------------
# report <- join_spy(x, y, by = "key_col")

## ----eval = FALSE-------------------------------------------------------------
# x_clean <- join_repair(x, by = "key_col")
# # or both sides:
# repaired <- join_repair(x, y, by = "key_col", standardize_case = "lower")

## ----eval = FALSE-------------------------------------------------------------
# key_duplicates(y, by = "key_col")

## ----eval = FALSE-------------------------------------------------------------
# result <- join_strict(x_clean, y_clean, by = "key_col",
#                       type = "left", expect = "1:1")

## ----eval = FALSE-------------------------------------------------------------
# result <- left_join_spy(x_clean, y_clean, by = "key_col")
# join_explain(result, x_clean, y_clean, by = "key_col")

## ----eval = FALSE-------------------------------------------------------------
# set_log_file("logs/join_diagnostics.log")
# # All join_spy() / join_explain() calls now append to this file

