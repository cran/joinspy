## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(joinspy)

# Transparent backgrounds for pkgdown light/dark mode
old_par <- par(no.readonly = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# # Install development version from GitHub
# # install.packages("pak")
# pak::pak("gcol33/joinspy")

## -----------------------------------------------------------------------------
# Sample data with issues
orders <- data.frame(

  customer_id = c("A", "B", "B", "C", "D "),
  amount = c(100, 200, 150, 300, 50),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  customer_id = c("A", "B", "C", "D", "E"),
  name = c("Alice", "Bob", "Carol", "David", "Eve"),
  stringsAsFactors = FALSE
)

# Get diagnostic report
report <- join_spy(orders, customers, by = "customer_id")

## -----------------------------------------------------------------------------
summary(report)

## -----------------------------------------------------------------------------
key_check(orders, customers, by = "customer_id")

## -----------------------------------------------------------------------------
key_duplicates(orders, by = "customer_id")

## -----------------------------------------------------------------------------
orders_clean <- data.frame(
  customer_id = c("A", "B", "C"),
  amount = c(100, 200, 300),
  stringsAsFactors = FALSE
)

# Silent mode for pipelines
result <- left_join_spy(orders_clean, customers, by = "customer_id", .quiet = TRUE)
head(result)

# Access diagnostics afterward
last_report()$match_analysis$match_rate

## -----------------------------------------------------------------------------
products <- data.frame(id = 1:3, name = c("Widget", "Gadget", "Gizmo"))
prices <- data.frame(id = 1:3, price = c(10, 20, 30))

# Succeeds - 1:1 relationship
join_strict(products, prices, by = "id", expect = "1:1")

## ----error = TRUE-------------------------------------------------------------
try({
# Fails - duplicates violate 1:1
prices_dup <- data.frame(id = c(1, 1, 2, 3), price = c(10, 15, 20, 30))
join_strict(products, prices_dup, by = "id", expect = "1:1")
})

## -----------------------------------------------------------------------------
messy <- data.frame(
  id = c(" A", "B ", "  C  "),
  value = 1:3,
  stringsAsFactors = FALSE
)

# Preview repairs
join_repair(messy, by = "id", dry_run = TRUE)

# Apply repairs
fixed <- join_repair(messy, by = "id")
fixed$id

## -----------------------------------------------------------------------------
orders_dup <- data.frame(id = 1:3, product = c("A", "B", "C"))
inventory <- data.frame(id = c(1, 1, 2, 3), location = c("NY", "LA", "NY", "LA"))

result <- merge(orders_dup, inventory, by = "id")
join_explain(result, orders_dup, inventory, by = "id", type = "inner")

## -----------------------------------------------------------------------------
before <- data.frame(id = 1:3, val = c("a", "b", "c"))
after <- merge(before, data.frame(id = 2:4, name = c("B", "C", "D")), by = "id", all = TRUE)

join_diff(before, after, by = "id")

## ----fig.width = 5, fig.height = 4--------------------------------------------
orders <- data.frame(id = 1:5, val = 1:5)
customers <- data.frame(id = 3:7, name = letters[3:7])

report <- join_spy(orders, customers, by = "id")
plot(report)  # Venn diagram

## -----------------------------------------------------------------------------
sessionInfo()

