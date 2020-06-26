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

rho1ToRho2 <- 0.1
sToL <- 1 / 3
exp(seq(log(0.1), log(2), length.out = 200)) -> hToL
sapply(hToL, function(x) (layer1RelativeRhoErrorIfLayer2Model(rho1ToRho2, 1.0, x, sToL, 1.0))) -> y1
par(mar = c(5, 6, 1, 1))
plot(hToL, y1, type = "l", lwd = 2, log = "xy",
     xlab = expression(bold(frac(h, L))),
     ylab = expression(bold(delta ~ rho))
)
layer1RelativeRhoErrorIfLayer2Model3 <- function(hToL, sToL) {
  k <- (1 - rho1ToRho2) / (1 + rho1ToRho2)
  sum(sapply(1:1024, function(n) (k^n / n^3))) -> result
  return((1 - sToL^2) * result / (32 * hToL^3))
}

sapply(hToL, function(x) (layer1RelativeRhoErrorIfLayer2Model3(x, sToL))) -> y3
lines(hToL, y3, type = "l", lwd = 2, lty = 6)
abline(h = 0.02)

legend("top", box.col = 'black',
       legend = c(
         expression(bold(frac(s, L) == frac(1, 3))),
         expression(bold(frac(rho ~ 1, rho ~ 2) == frac(10, 1)))
       ),
       lty = c(1, 5), lwd = c(1, 1), horiz = F
)