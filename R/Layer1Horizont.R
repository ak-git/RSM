source("R/rsm.R")

layer1RelativeRhoErrorIfLayer2Model <- function(rho1, rho2, hmm, smm, lmm) {
  mmToSI(hmm) -> h
  mmToSI(smm) -> s
  mmToSI(lmm) -> l
  k <- rhoToK(rho1, rho2)

  err <- function(expected) {
    sum(sapply(1:MAX_SUM, function(x) ((k^x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> result
    actual <- rho1 * (1.0 + 2.0 / (mp(l - s) - mp(l + s)) * result)
    return(abs(actual - expected) / expected)
  }

  return(err(rho1))
}

rho1ToRho2 <- 0.001
sToL <- 1 / 3
exp(seq(log(0.1), log(1), length.out = 100)) -> hToL
sapply(hToL, function(x) (layer1RelativeRhoErrorIfLayer2Model(rho1ToRho2, 1.0, x, sToL, 1.0))) -> y1
par(mar = c(5, 6, 1, 1))
plot(hToL, y1, type = "l", lwd = 2, log = "xy",
     xlab = expression(bold(frac(h, L))),
     ylab = expression(bold(delta ~ rho))
)

layer1RelativeRhoErrorIfLayer2Model2 <- function(hToL, sToL) {

  invHypot <- function(x, y) {
    result <- sqrt(x^2 + y^2)
    return(1 / result)
  }

  sum(sapply(1:MAX_SUM, function(x) (invHypot(1 - sToL, 4 * x * hToL) - invHypot(1 + sToL, 4 * x * hToL)))) -> result
  actual <- (1 - sToL^2) / sToL * result
  return(abs(actual))
}

library("VGAM")

layer1RelativeRhoErrorIfLayer2Model3 <- function(hToL, sToL) {
  return((1 - sToL^2) * zeta(3) / (32 * hToL^3))
}

sapply(hToL, function(x) (layer1RelativeRhoErrorIfLayer2Model2(x, sToL))) -> y2
lines(hToL, y2, type = "l", lwd = 2, lty = 5)
sapply(hToL, function(x) (layer1RelativeRhoErrorIfLayer2Model3(x, sToL))) -> y3
lines(hToL, y3, type = "l", lwd = 2, lty = 6)