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


test_that("layer2Ohms", {
  expect_equal(round(layer2Ohms(8.0, 1.0, 10.0, 20.0, 10.0), 3), 309.342)
  expect_equal(round(layer2Ohms(8.0, 1.0, 10.0, 30.0, 90.0), 3), 8.815)
  expect_equal(round(layer2Ohms(8.0, 1.0, 50.0, 10.0, 20.0), 3), 339.173)
  expect_equal(round(layer2Ohms(8.0, 1.0, 50.0, 30.0, 90.0), 3), 38.858)

  expect_equal(round(layer2Ohms(1.0, 1.0, 0.0, 20.0, 40.0), 3), 21.221)
  expect_equal(round(layer2Ohms(0.7, Inf, 5.0, 10.0, 30.0), 3), 30.971)
})
