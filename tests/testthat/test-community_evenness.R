
# Write test cases using expect_equal() function
test_that("community_evenness calculates Shannon index correctly", {
  abundances <- c(5, 2, 3, 1, 4)
  expected_E <- 0.9256339
  calculated_E <- community_evenness(abundances, index = "Shannon")
  expect_true(all.equal(calculated_E, expected_E, tolerance = 0.00001))
})

test_that("community_evenness calculates Simpson index correctly", {
  abundances <- c(40,27,5,6,9)
  expected_E <- 3.0631323
  calculated_E <- community_evenness(abundances, index = "Simpson")
  expect_true(all.equal(calculated_E, expected_E, tolerance = 0.00001))
})

test_that("community_evenness calculates Pielou index correctly", {
  abundances <- c(50, 2, 35, 10, 4)
  expected_E <- 1.149817
  calculated_E <- community_evenness(abundances, index = "Pielou")
  expect_true(all.equal(calculated_E, expected_E, tolerance = 0.00001))
})

test_that("community_evenness throws an error with invalid index option", {
  abundances <- c(5, 2, 3, 1, 4)
  expect_error(community_evenness(abundances, index = "Invalid"), "Invalid index option. Please choose among 'Shannon', 'Simpson' or 'Pielou'. Be aware of capitalization")
})
