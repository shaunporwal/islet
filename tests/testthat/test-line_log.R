test_that("multiplication works", {


  # check is null
  expect_error(
    line_log(type = NULL,
             item = NULL),
    "argument is of length zero"
  )

  expect_error(
    line_log(type = NULL,
             item = "adfal"),
    "argument is of length zero"
  )

  expect_error(
    line_log(type = "COMMENT",
             item = NULL),
    "Type or Item cannot be NULL"
  )
})
