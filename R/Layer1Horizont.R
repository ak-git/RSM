source("R/rsm.R")

layer1RelativeRhoErrorIfLayer2Model <- function (rho1, rho2, hmm, smm, lmm) {
  mmToSI(hmm) -> h
  mmToSI(smm) -> s
  mmToSI(lmm) -> l
  k <- rhoToK(rho1, rho2)

  sum(sapply(1:MAX_SUM, function(x) ((k ^ x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> result
  return(abs(2.0 / (mp(l - s) - mp(l + s)) * result))
}

rho1ToRho2 <- 10

sToL <- 0.5
exp(seq(log(0.1), log(1), length.out = 100)) -> hToL
sapply(hToL, function (x) (layer1RelativeRhoErrorIfLayer2Model(rho1ToRho2, 1.0, x, sToL, 1.0))) -> y1
par(mar = c(5, 6, 1, 1))
plot(hToL, y1, type = "l", lwd = 2, ylim = c(0, 1),
     xlab = expression(bold(frac(h, L))),
     ylab = expression(bold(delta~rho))
)

sToL <- 0.1
sapply(hToL, function (x) (layer1RelativeRhoErrorIfLayer2Model(rho1ToRho2, 1.0, x, sToL, 1.0))) -> y2
lines(hToL, y2, type = "l", lwd = 2, lty = 5)
abline(h = 0.0, col = 'black', lty = 1, lwd = 0.5)
legend("topright", box.col = 'white', inset = 0.2,
       title = expression(bold(frac(rho[1], rho[2]) == frac(10, 1))),
       legend = c(
         expression(bold(frac(s, L) == 0.5)),
         expression(bold(frac(s, L) == 0.1))
       ),
       lty = c(1, 5), lwd = c(2, 2), horiz = F, y.intersp = 10
)
