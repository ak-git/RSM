test_that("layer1RelativeRhoErrorIfLayer2Model", {
  rho1 <- 1
  rho2 <- 0.1
  hmm <- 1
  smm <- 2
  lmm <- 3
  rho1Real <- layer1Inverse(smm, lmm, layer2Ohms(rho1, rho2, hmm, smm, lmm))
  relError <- abs(rho1Real - rho1) / rho1

  expect_equal(relError, layer1RelativeRhoErrorIfLayer2Model(10, 1, 1, 2, 3))
  expect_equal(relError, layer1RelativeRhoErrorIfLayer2Model(1, 0.1, 2, 4, 6))
})