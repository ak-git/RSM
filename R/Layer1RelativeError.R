layer1RelError <- function(smm, lmm, relError) {
  abs(smm) -> smm
  abs(lmm) -> lmm
  max(smm, lmm) * relError * sign(lmm - smm) -> err
  layer1Inverse(smm - err, lmm + err, layer1Ohms(1.0, smm, lmm)) -> inverse
  return(abs(inverse - 1.0))
}

relErrorFactor <- function(x) {
  0.001 -> relError
  return(layer1RelError(x, 1.0, relError) / relError)
}

source('~/Documents/Source Code/RSM/R/rsm.R', echo = TRUE)

seq(0.2, 5, 0.01) -> sToL
sapply(sToL, relErrorFactor) -> y
plot(sToL, y, type = "l", lwd = 2, ylim = c(5, 20), log = 'xy', xlab = "s / L", ylab = "Error Factor")
sqrt(2) - 1 -> xmin1
1 / xmin1 -> xmin2
points(xmin1, relErrorFactor(xmin1))
points(xmin2, relErrorFactor(xmin2))
abline(h = c(5, 10, 15, 20), col = 'black', lty = 1, lwd = 0.5)
abline(v = c(0.2, 0.5, 1, 2, 5), col = 'black', lty = 1, lwd = 0.5)
abline(h = relErrorFactor(xmin1), col = 'black', lty = 2, lwd = 1.5)
text(0.4, 5.3, expression(sqrt(2) - 1))
