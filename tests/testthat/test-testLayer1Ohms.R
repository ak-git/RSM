test_that("layer1Ohms", {
  expect_equal(round(layer1Ohms(0.7, 30, 60), 3), 9.903)
  expect_equal(round(layer1Ohms(0.7, 60, 30), 3), 9.903)
  expect_equal(round(layer1Ohms(0.7, c(10, 20, 120), 60), 3), c(2.546, 5.570, 4.951))
  expect_equal(round(as.double(layer1Ohms(0.7, 10)), 3), c(11.141, 16.711, 7.427, 16.340, 14.854, 17.825))
  expect_equal(names(layer1Ohms(0.7, 10)), c("10 x 30 mm", "30 x 50 mm", "[I - x - U - U - I - x]", "[I - U - x - x - I - U]", "20 x 40 mm", "40 x 60 mm"))
})

test_that("mmToSI", {
  expect_equal(round(mmToSI(1), 3), 0.001)
  expect_equal(round(mmToSI(20), 3), 0.02)
  expect_equal(round(mmToSI(1000), 3), 1.00)
})

test_that("layer1Inverse", {
  expect_equal(round(layer1Inverse(30, 60, 9.903), 3), 0.7)
  expect_equal(round(layer1Inverse(60, 30, 9.903), 3), 0.7)
})
