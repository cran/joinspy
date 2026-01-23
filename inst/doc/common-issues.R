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
  customer_id = c(1, 2, 2, 3),
  amount = c(100, 50, 75, 200)
)

addresses <- data.frame(
  customer_id = c(1, 2, 2, 3),
  address = c("NYC", "LA", "SF", "Chicago")
)

join_spy(orders, addresses, by = "customer_id")

## -----------------------------------------------------------------------------
key_duplicates(orders, by = "customer_id")
key_duplicates(addresses, by = "customer_id")

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
sales_fixed <- join_repair(sales, by = "product")
key_check(sales_fixed, inventory, by = "product")

## -----------------------------------------------------------------------------
left <- data.frame(
  code = c("ABC", "def", "GHI"),
  value = 1:3,
  stringsAsFactors = FALSE
)

right <- data.frame(
  code = c("abc", "DEF", "ghi"),
  label = c("A", "D", "G"),
  stringsAsFactors = FALSE
)

join_spy(left, right, by = "code")

## -----------------------------------------------------------------------------
repaired <- join_repair(left, right, by = "code", standardize_case = "upper")
key_check(repaired$x, repaired$y, by = "code")

## -----------------------------------------------------------------------------
orders <- data.frame(
  customer_id = c(1, NA, 3, NA),
  amount = c(100, 200, 300, 400)
)

customers <- data.frame(
  customer_id = c(1, 2, 3, NA),
  name = c("Alice", "Bob", "Carol", "Unknown")
)

join_spy(orders, customers, by = "customer_id")

## -----------------------------------------------------------------------------
# Option 1: Remove rows with NA keys
orders_clean <- orders[!is.na(orders$customer_id), ]

# Option 2: Replace NA with placeholder
orders$customer_id[is.na(orders$customer_id)] <- -999

## -----------------------------------------------------------------------------
system_a <- data.frame(
  user_id = c("USR001", "USR002", "USR003"),
  score = c(85, 90, 78),
  stringsAsFactors = FALSE
)

system_b <- data.frame(
  user_id = c("1", "2", "3"),
  department = c("Sales", "Marketing", "Engineering"),
  stringsAsFactors = FALSE
)

report <- join_spy(system_a, system_b, by = "user_id")

## -----------------------------------------------------------------------------
# Extract numeric part
system_a$user_num <- gsub("USR0*", "", system_a$user_id)
key_check(system_a, system_b, by = c("user_num" = "user_id"))

## -----------------------------------------------------------------------------
order_items <- data.frame(
  order_id = c(1, 1, 2, 2, 2),
  item = c("A", "B", "C", "D", "E")
)

order_payments <- data.frame(
  order_id = c(1, 1, 2, 2),
  payment = c("CC1", "CC2", "Cash", "Check")
)

report <- join_spy(order_items, order_payments, by = "order_id")

## -----------------------------------------------------------------------------
check_cartesian(order_items, order_payments, by = "order_id")

## ----error = TRUE-------------------------------------------------------------
try({
# Enforce cardinality to catch this
join_strict(order_items, order_payments, by = "order_id", expect = "1:m")
})

## -----------------------------------------------------------------------------
orders <- data.frame(
  product_id = c(1, 2, 3),
  quantity = c(10, 20, 30)
)

products <- data.frame(
  product_id = c("1", "2", "3"),
  name = c("Widget", "Gadget", "Gizmo"),
  stringsAsFactors = FALSE
)

join_spy(orders, products, by = "product_id")

## -----------------------------------------------------------------------------
orders$product_id <- as.character(orders$product_id)
key_check(orders, products, by = "product_id")

## -----------------------------------------------------------------------------
left <- data.frame(
  id = c("A", "", "C"),
  value = 1:3,
  stringsAsFactors = FALSE
)

right <- data.frame(
  id = c("A", "B", ""),
  label = c("Alpha", "Beta", "Empty"),
  stringsAsFactors = FALSE
)

join_spy(left, right, by = "id")

## -----------------------------------------------------------------------------
left_fixed <- join_repair(left, by = "id", empty_to_na = TRUE)
left_fixed$id

## ----error = TRUE-------------------------------------------------------------
try({
products <- data.frame(id = 1:3, name = c("A", "B", "C"))
prices <- data.frame(id = c(1, 2, 2, 3), price = c(10, 20, 25, 30))

join_strict(products, prices, by = "id", expect = "1:1")
})

## -----------------------------------------------------------------------------
orders <- data.frame(id = c(1, 1, 2, 3), item = c("A", "B", "C", "D"))
customers <- data.frame(id = 1:3, name = c("Alice", "Bob", "Carol"))

detect_cardinality(orders, customers, by = "id")

## -----------------------------------------------------------------------------
sessionInfo()

