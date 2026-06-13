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
join_repair(sales, by = "product", dry_run = TRUE)

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
report <- join_spy(left, right, by = "city")
suggest_repairs(report)

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
both <- join_repair(patients, visits, by = "mrn", empty_to_na = TRUE)
both$y$mrn

## -----------------------------------------------------------------------------
surveys <- data.frame(
  site = factor(c("North", "South", "East")),
  count = c(12, 7, 30)
)

habitats <- data.frame(
  site = c("North", "South", "East"),
  habitat = c("bog", "meadow", "forest"),
  stringsAsFactors = FALSE
)

join_spy(surveys, habitats, by = "site")

## -----------------------------------------------------------------------------
plots <- data.frame(site = factor(c("North ", "South")), richness = c(14, 9))
join_spy(plots, habitats, by = "site")

## -----------------------------------------------------------------------------
plots$site <- as.character(plots$site)
join_spy(plots, habitats, by = "site")

## -----------------------------------------------------------------------------
plots_clean <- join_repair(plots, by = "site")
key_check(plots_clean, habitats, by = "site")

## -----------------------------------------------------------------------------
surveys_f <- data.frame(
  site = factor(c("North", "South"), levels = c("North", "South", "West"))
)
habitats_f <- data.frame(site = factor(c("North", "South", "East")))

join_spy(surveys_f, habitats_f, by = "site")

## -----------------------------------------------------------------------------
plot_ids <- factor(c("10", "20", "30"))
as.numeric(plot_ids)
as.numeric(as.character(plot_ids))

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
key_duplicates(addresses, by = "customer_id", keep = "first")

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
orders_s <- orders
customers_s <- customers
orders_s$customer_id[is.na(orders_s$customer_id)] <- -1
customers_s$customer_id[is.na(customers_s$customer_id)] <- -1

join_spy(orders_s, customers_s, by = "customer_id")

## -----------------------------------------------------------------------------
merge(orders_s, customers_s, by = "customer_id", all.x = TRUE)

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
ids <- c("007", "042")
as.character(as.numeric(ids))

## -----------------------------------------------------------------------------
readings <- data.frame(depth = cumsum(rep(0.1, 3)), oxygen = c(8.1, 7.4, 6.9))
layers <- data.frame(
  depth = c(0.1, 0.2, 0.3),
  layer = c("surface", "mid", "bottom")
)

print(readings$depth, digits = 17)
readings$depth == layers$depth

## -----------------------------------------------------------------------------
join_spy(readings, layers, by = "depth")

## -----------------------------------------------------------------------------
readings$depth <- round(readings$depth, 6)
all(readings$depth %in% layers$depth)

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
join_strict(items, payments, by = "order_id", type = "left", expect = "1:n")
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

## -----------------------------------------------------------------------------
admissions <- data.frame(
  patient_id = c("P-01 ", "P-02", "P-03"),
  ward = c("A", "B", "B"),
  stringsAsFactors = FALSE
)

registry <- data.frame(
  mrn = c("P-01", "P-02", "P-04"),
  dob = c("1980-03-02", "1975-11-19", "1990-07-30"),
  stringsAsFactors = FALSE
)

join_spy(admissions, registry, by = c("patient_id" = "mrn"))

## -----------------------------------------------------------------------------
fixed <- join_repair(admissions, registry, by = c("patient_id" = "mrn"))
left_join_spy(fixed$x, fixed$y, by = c("patient_id" = "mrn"), verbose = FALSE)

## -----------------------------------------------------------------------------
shipments <- data.frame(
  order_ref = c("ORD-1 ", "ORD-2", "ORD-2", "ORD-3", NA),
  qty = c(10, 25, 5, 12, 7),
  stringsAsFactors = FALSE
)

invoices <- data.frame(
  order_ref = c("ORD-1", "ORD-2", "ORD-4"),
  total = c(99, 250, 80),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
report <- join_spy(shipments, invoices, by = "order_ref")
report

## -----------------------------------------------------------------------------
shipments_repaired <- join_repair(shipments, by = "order_ref")

## -----------------------------------------------------------------------------
key_duplicates(shipments_repaired, by = "order_ref")
detect_cardinality(shipments_repaired, invoices, by = "order_ref")

## -----------------------------------------------------------------------------
shipments_repaired <- shipments_repaired[!is.na(shipments_repaired$order_ref), ]

## -----------------------------------------------------------------------------
result <- join_strict(shipments_repaired, invoices, by = "order_ref",
                      type = "left", expect = "n:1")
result

## -----------------------------------------------------------------------------
join_explain(result, shipments_repaired, invoices,
             by = "order_ref", type = "left")

## -----------------------------------------------------------------------------
log_file <- tempfile(fileext = ".log")
set_log_file(log_file)
audited <- left_join_spy(shipments_repaired, invoices,
                         by = "order_ref", .quiet = TRUE)
set_log_file(NULL)
readLines(log_file)[2:8]

## -----------------------------------------------------------------------------
orders <- data.frame(order_id = 1:3, customer_id = c(1, 2, 2))
customers <- data.frame(customer_id = 1:3, region_id = c(1, 1, 2))
regions <- data.frame(region_id = 1:2, name = c("North", "South"))

analyze_join_chain(
  tables = list(orders = orders, customers = customers, regions = regions),
  joins = list(
    list(left = "orders", right = "customers", by = "customer_id"),
    list(left = "result", right = "regions", by = "region_id")
  )
)

