
# Write test cases using expect_equal() function
test_that("community_evenness calculates Shannon index correctly", {
  abundances <- c(5, 2, 3, 1, 4)
  expected_H <- 1.48975
  calculated_H <- community_evenness(abundances, index = "Shannon")
  expect_true(all.equal(calculated_H, expected_H, tolerance = 0.00001))
})

test_that("community_evenness calculates Simpson index correctly", {
  abundances <- c(40,27,5,6,9)
  expected_D <- 0.6735368
  calculated_D <- community_evenness(abundances, index = "Simpson")
  expect_true(all.equal(calculated_D, expected_D, tolerance = 0.00001))
})

test_that("community_evenness calculates Pielou index correctly", {
  abundances <- c(50, 2, 35, 10, 4)
  expected_E <- 0.7144215
  calculated_E <- community_evenness(abundances, index = "Pielou")
  expect_true(all.equal(calculated_E, expected_E, tolerance = 0.00001))
})

test_that("community_evenness throws an error with invalid index option", {
  abundances <- c(5, 2, 3, 1, 4)
  expect_error(community_evenness(abundances, index = "Invalid"))
})
