test_that("dRdHLR", {
  0.0001 -> dhmm
  20 -> lmm
  layer2Ohms(rho1 = 9, rho2 = 1, hmm = lmm * 0.2, smm = lmm * 0.5, lmm = lmm) -> R1
  layer2Ohms(rho1 = 9, rho2 = 1, hmm = lmm * 0.2 + dhmm, smm = lmm * 0.5, lmm = lmm) -> R2
  ((R2 - R1) / dhmm) * (lmm / R1) -> der
  expect_equal(round(dRdHLR(-0.8, 0.2, 0.5), 3), round(der, 3))

  0.00001 -> dhmm
  30 -> lmm
  layer2Ohms(rho1 = 1, rho2 = 9, hmm = lmm * 0.1, smm = lmm * 0.3, lmm = lmm) -> R1
  layer2Ohms(rho1 = 1, rho2 = 9, hmm = lmm * 0.1 + dhmm, smm = lmm * 0.3, lmm = lmm) -> R2
  ((R2 - R1) / dhmm) * (lmm / R1) -> der
  expect_equal(round(dRdHLR(0.8, 0.1, 0.3), 3), round(der, 3))
})