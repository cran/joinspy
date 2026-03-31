## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(joinspy)

## -----------------------------------------------------------------------------
partner_sales <- data.frame(
  customer_id = c("CUST-1001", "CUST-1002 ", "CUST-1003",
                  " CUST-1004", "CUST-1005 ", "CUST-1006"),
  amount = c(2500, 1800, 3200, 950, 4100, 1600),
  stringsAsFactors = FALSE
)

internal_db <- data.frame(
  customer_id = c("CUST-1001", "CUST-1002", "CUST-1003",
                  "CUST-1004", "CUST-1005", "CUST-1006", "CUST-1007"),
  region = c("West", "East", "West", "South", "East", "North", "West"),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
report <- join_spy(partner_sales, internal_db, by = "customer_id")

## -----------------------------------------------------------------------------
repaired <- join_repair(partner_sales, internal_db, by = "customer_id")
partner_fixed <- repaired$x
internal_fixed <- repaired$y

## -----------------------------------------------------------------------------
key_check(partner_fixed, internal_fixed, by = "customer_id")

## -----------------------------------------------------------------------------
result <- merge(partner_fixed, internal_fixed, by = "customer_id")
nrow(result)

## -----------------------------------------------------------------------------
crm_profiles <- data.frame(
  email = c("ALICE@ACME.COM", "BOB@ACME.COM", "CAROL@ACME.COM",
            "DAVE@ACME.COM", "EVE@ACME.COM"),
  plan = c("enterprise", "starter", "pro", "enterprise", "starter"),
  stringsAsFactors = FALSE
)

click_events <- data.frame(
  email = c("alice@acme.com", "bob@acme.com", "carol@acme.com",
            "dave@acme.com", "frank@acme.com"),
  page_views = c(47, 12, 89, 33, 5),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
report <- join_spy(crm_profiles, click_events, by = "email")

## -----------------------------------------------------------------------------
suggest_repairs(report)

## -----------------------------------------------------------------------------
repaired <- join_repair(
  crm_profiles, click_events,
  by = "email",
  standardize_case = "lower"
)

## -----------------------------------------------------------------------------
result <- merge(repaired$x, repaired$y, by = "email")
nrow(result)
result

## -----------------------------------------------------------------------------
# Simulating PDF copy-paste artifacts:
# \u00A0 is non-breaking space, \u200B is zero-width space
pdf_data <- data.frame(
  country = c("Brazil", "India\u200B", "Germany",
              "Japan\u00A0", "Canada", "France\u200B"),
  prevalence = c(12.3, 8.7, 5.1, 3.9, 6.2, 4.8),
  stringsAsFactors = FALSE
)

reference <- data.frame(
  country = c("Brazil", "India", "Germany", "Japan",
              "Canada", "France", "Italy"),
  population_m = c(214, 1408, 84, 125, 38, 68, 59),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
pdf_data$country

## -----------------------------------------------------------------------------
nrow(merge(pdf_data, reference, by = "country"))

## -----------------------------------------------------------------------------
report <- join_spy(pdf_data, reference, by = "country")

## -----------------------------------------------------------------------------
repaired <- join_repair(pdf_data, reference, by = "country")
nrow(merge(repaired$x, repaired$y, by = "country"))

## -----------------------------------------------------------------------------
# Product catalogue (canonical format)
catalogue <- data.frame(
  product_code = c("WDG-100", "WDG-101", "WDG-102",
                   "WDG-103", "WDG-104", "WDG-105"),
  product_name = c("Widget A", "Widget B", "Widget C",
                   "Widget D", "Widget E", "Widget F"),
  margin = c(0.35, 0.28, 0.42, 0.31, 0.39, 0.25),
  stringsAsFactors = FALSE
)

# Recent transactions (mix of old and new clerk entries)
transactions <- data.frame(
  product_code = c("WDG-100", "WDG-101", "WDG102",
                   "wdg-103", "WDG-104", "wdg105",
                   "WDG-100", "WDG103"),
  quantity = c(5, 3, 7, 2, 4, 6, 1, 8),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
report <- join_spy(transactions, catalogue, by = "product_code")

## -----------------------------------------------------------------------------
join_repair(transactions, catalogue,
            by = "product_code",
            standardize_case = "upper",
            dry_run = TRUE)

## -----------------------------------------------------------------------------
repaired <- join_repair(transactions, catalogue,
                        by = "product_code",
                        standardize_case = "upper")

## -----------------------------------------------------------------------------
# Manual fix: insert dash if missing in product codes matching the pattern
fix_codes <- function(codes) {
  gsub("^([A-Z]{3})(\\d)", "\\1-\\2", codes)
}
repaired$x$product_code <- fix_codes(repaired$x$product_code)

## -----------------------------------------------------------------------------
result <- merge(repaired$x, repaired$y, by = "product_code")
nrow(result)

## -----------------------------------------------------------------------------
economics <- data.frame(
  region = c("North America", "Europe", "Asia Pacific ",
             "North America", "Europe", "Asia Pacific "),
  year = c(2022, 2022, 2022, 2023, 2023, 2023),
  gdp_growth = c(2.1, 1.8, 4.2, 1.9, 0.9, 3.8),
  stringsAsFactors = FALSE
)

population <- data.frame(
  region = c("North America", "Europe", "Asia Pacific",
             "North America", "Europe", "Asia Pacific"),
  year = c(2022, 2022, 2022, 2023, 2023, 2023),
  pop_millions = c(580, 450, 4300, 585, 448, 4350),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
merged <- merge(economics, population, by = c("region", "year"))
nrow(merged)

## -----------------------------------------------------------------------------
report <- join_spy(economics, population, by = c("region", "year"))

## -----------------------------------------------------------------------------
repaired <- join_repair(economics, population, by = c("region", "year"))
result <- merge(repaired$x, repaired$y, by = c("region", "year"))
nrow(result)

