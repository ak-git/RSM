test_that("layer1RelError", {
  expect_equal(round(layer1RelError(10, 30, 0.001), 3), 0.006)
  expect_equal(round(layer1RelError(30, 10, 0.001), 3), 0.006)
  expect_equal(round(layer1RelError(10, 20, 0.001), 3), 0.006)
  expect_equal(round(layer1RelError(40, 20, 0.001), 3), 0.006)
})

test_that("relErrorFactor", {
  plotLayer1RelError()
  expect_equal(round(relErrorFactor(0.5), 1), 6)
  expect_equal(round(relErrorFactor(sqrt(2) - 1), 1), 5.8)
  expect_equal(round(relErrorFactor(sqrt(2) + 1), 1), 5.8)
})
