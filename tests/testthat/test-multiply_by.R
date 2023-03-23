test_that("multiply_by works", {
  expect_equal(multiply_by(2,2), 4)
  expect_error(multiply_by(2, a))
})
