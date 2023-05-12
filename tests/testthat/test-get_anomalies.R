test_that("get_anomalies works", {

  abund <- c(20, 20, 20, 30, 25, 25, 20, 200, 180, 15, 20, 25, 30, 25, 250, 20, 15)

  anom <- get_anomalies(abund, time_lag = 4, cutoff = 1.96, negative = FALSE)

  expect_true(anom$anomaly)
  expect_equal(anom$anomaly.position, c(8, 15))
  expect_equal(anom$z, c(NA, NA, NA, NA, 0.5, 0.261, -1.225, 42.866, 1.273, -0.94, -0.838,
                         -0.788, -0.375, 0.387, 55.114, -0.56, -0.589))

  })
