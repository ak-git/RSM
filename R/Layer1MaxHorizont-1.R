library("VGAM")

hToL_Horizont <- function (sToL, deltaL) {
  return((sToL * (1 - sToL) ^ 2 * zeta(3) / (32 * deltaL)) ^ (1 / 3))
}

hToL_Horizont2 <- function (sToL, deltaL) {
  rho1ToRho2 <- 0.5
  k <- (1 - rho1ToRho2) / (1 + rho1ToRho2)
  sum(sapply(1:1024, function(n) (k^n / n^3))) -> result
  return((abs(sToL * (1 - sToL) ^ 2 * result / (32 * deltaL))) ^ (1 / 3))
}

seq(0, 1, length.out = 100) -> sToL
deltaL <- 0.001
sapply(sToL, function (x) (hToL_Horizont(x, deltaL))) -> y
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(sToL, y, type = "l", lwd = 2, ylim = c(0.4, 1.8), log = "y",
     xlab = expression(bold(frac(s, L))),
     ylab = expression(bold(frac(h, L))),
     main = expression(bold(delta ~ L == '0,001'))
)
abline(h = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8), col = 'black', lty = 1, lwd = 0.5)
abline(v = seq(0, 1, 0.2), col = 'black', lty = 1, lwd = 0.5)
abline(v = 1 / 3, col = 'black', lty = 2, lwd = 1.5)

sapply(sToL, function (x) (hToL_Horizont2(x, deltaL))) -> y3
lines(sToL, y3, type = "l", lwd = 2, lty = 6)
text(1 / 3, 0.45, expression(bold(frac(s, L) == frac(1, 3))), pos = 2, srt = 0)

legend("topright", box.col = 'black',
       legend = c(
         expression(bold(rho[2] == infinity)),
         expression(bold(frac(rho[1], rho[2]) == '2'))
       ),
       lty = c(1, 5), lwd = c(1, 1), horiz = F
)