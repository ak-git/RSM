layer1RelError <- function(smm, lmm, relError) {
  abs(smm) -> smm
  abs(lmm) -> lmm
  max(smm, lmm) * relError * sign(lmm - smm) -> err
  layer1Inverse(smm - err, lmm + err, layer1Ohms(1.0, smm, lmm)) -> inverse
  return(abs(inverse - 1.0))
}

relErrorFactor <- function(x) {
  if (x > 1) {
    1 / x -> x
  }
  return((1 + x) / (x * (1 - x)))
}

plotLayer1RelError <- function() {
  seq(0.2, 5, 0.01) -> sToL
  sapply(sToL, relErrorFactor) -> y
  par(mar = c(5, 6, 1, 1))
  plot(sToL, y, type = "l", lwd = 2, ylim = c(5, 20), log = 'xy',
       xlab = expression(bold(frac(s, L))),
       ylab = expression(bold(frac(delta~rho, delta~L)))
  )
  sqrt(2) - 1 -> xmin1
  sqrt(2) + 1 -> xmin2
  points(xmin1, relErrorFactor(xmin1))
  points(xmin2, relErrorFactor(xmin2))
  abline(h = c(5, 10, 15, 20), col = 'black', lty = 1, lwd = 0.5)
  abline(v = c(0.5, 1, 2), col = 'black', lty = 1, lwd = 0.5)
  abline(h = relErrorFactor(xmin1), col = 'black', lty = 2, lwd = 1.5)
  text(xmin1, relErrorFactor(xmin1), expression(sqrt(2) - 1), pos = 1)
  text(xmin2, relErrorFactor(xmin2), expression(sqrt(2) + 1), pos = 1)
  text(0.18, relErrorFactor(xmin2), expression(~3 + 2 * sqrt(2)), pos = 4, srt = 90)
}
