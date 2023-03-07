# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

# test_that("Test check_duplicates",{
#   library(dplyr)
#   library(testthat)
#
#   # Generate sample data
#   data_df <- data.frame(
#     id = c(1, 2, 3, 4, 5, 5, 6, 7, 8, 8),
#     name = c("Alice", "Alice", "Charlie", "David", "Eve", "Eve", "Frank", "Grace", "Henry", "Henry")
#   )
#
#   # Test 1: Check for exact duplicates with default options
#   expect_equal(
#     check_duplicates(data_df = data_df,
#                      print_dups = FALSE),
#     data.frame(
#       id = c(5, 8),
#       name = c("Eve", "Henry")
#     ),
#     info = "Exact duplicates should be identified"
#   )
#
#   # Test 2: Check for duplicates within groups
#   expect_equal(
#     check_duplicates(data_df, group_by_vars = "name"),
#     data.frame(
#       id = c(5, 8, 1, 2),
#       name = c("Eve", "Henry", 'Alice', 'Alice')
#     ),
#     info = "Duplicates within groups should be identified"
#   )
#
#   # Test 3: Check for duplicates with print_dups = FALSE
#   expect_equal(
#     check_duplicates(data_df, print_dups = FALSE),
#     data.frame(
#       id = c(5, 8),
#       name = c("Eve", "Henry")
#     ),
#     info = "Exact duplicates should still be identified when print_dups is set to FALSE"
#   )
#
#   # Test 4: Check for duplicates with non-default grouping variables and print_dups = FALSE
#   expect_equal(
#     check_duplicates(data_df, group_by_vars = "id", print_dups = FALSE),
#     data.frame(
#       id = c(5, 8),
#       name = c("Eve", "Henry")
#     ),
#     info = "Duplicates within groups should still be identified when group_by_vars and print_dups are set to non-default values"
#   )
#
# })

# Generate sample data
data_df <- data.frame(
  id = c(1, 2, 3, 4, 5, 5, 6, 7, 8, 8),
  name = c("Alice", "Alice", "Charlie", "David", "Eve", "Eve", "Frank", "Grace", "Henry", "Henry"),
  income = c(60000, 60000, 70000, 80000, 90000, 90000, 100000, 110000, 120000, 120000)
)

# Test 1: Check for exact duplicates with default options
expect_equal(
  check_duplicates(data_df = data_df,
                   print_dups = FALSE),
  data.frame(
    id = c(5, 8),
    name = c("Eve", "Henry"),
    income = c(90000, 120000)
  ),
  info = "Exact duplicates should be identified"
)

# Test 2: Check for duplicates within groups
expect_equal(
  check_duplicates(data_df, group_by_vars = "name"),
  data.frame(
    id = c(5, 8, 1, 2),
    name = c("Eve", "Henry", 'Alice', 'Alice'),
    income = c(90000, 120000, 60000, 60000)
  ),
  info = "Duplicates within groups should be identified"
)

# Test 3: Check for duplicates with print_dups = FALSE
expect_equal(
  check_duplicates(data_df, print_dups = FALSE),
  data.frame(
    id = c(5, 8),
    name = c("Eve", "Henry"),
    income = c(90000, 120000)
  ),
  info = "Exact duplicates should still be identified when print_dups is set to FALSE"
)

# Test 4: Check for duplicates with non-default grouping variables and print_dups = FALSE
expect_equal(
  check_duplicates(data_df, group_by_vars = "id", print_dups = FALSE),
  data.frame(
    id = c(5, 8),
    name = c("Eve", "Henry"),
    income = c(90000, 120000)
  ),
  info = "Duplicates within groups should still be identified when group_by_vars and print_dups are set to non-default values"
)


