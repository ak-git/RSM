library("VGAM")

hToL_Horizont <- function (sToL, deltaL) {
  return((sToL * (1 - sToL) ^ 2 * zeta(3) / (32 * deltaL)) ^ (1 / 3))
}

seq(0, 1, length.out = 100) -> sToL
deltaL <- 0.001
sapply(sToL, function (x) (hToL_Horizont(x, deltaL))) -> y
par(mar = c(5, 6, 1, 1), mfrow = c(1, 2))
plot(sToL, y, type = "l", lwd = 2, ylim = c(0.4, 1.8), log = "y",
     xlab = expression(bold(frac(s, L))),
     ylab = expression(bold(frac(h, L)))
)
abline(h = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8), col = 'black', lty = 1, lwd = 0.5)
abline(v = seq(0, 1, 0.2), col = 'black', lty = 1, lwd = 0.5)
abline(v = 1 / 3, col = 'black', lty = 2, lwd = 1.5)

deltaL <- 0.01
sapply(sToL, function (x) (hToL_Horizont(x, deltaL))) -> y2
lines(sToL, y2, type = "l", lwd = 2, lty = 5)
text(1 / 3, 0.45, expression(bold(frac(s, L) == frac(1, 3))), pos = 2, srt = 0)

legend("top", box.col = 'black',
       legend = c(
         expression(bold(delta ~ L == '0,001')),
         expression(bold(delta ~ L == '0,01'))
       ),
       lty = c(1, 5), lwd = c(1, 1), horiz = F
)

exp(seq(log(0.001), log(0.1), length.out = 100)) -> deltaL
sapply(deltaL, function (x) (hToL_Horizont(1 / 3, x))) -> y
plot(deltaL, y, type = "l", lwd = 2, ylim = c(0.4, 1.8), log = "xy",
     xlab = expression(bold(delta ~ L)),
     ylab = expression(bold(frac(h, L)))
)
abline(v = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1), col = 'black', lty = 1, lwd = 0.5)
abline(h = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8), col = 'black', lty = 1, lwd = 0.5)

legend("top", box.col = 'black',
       legend = c(
         expression(bold(frac(s, L) == frac(1, 3)))
       ),
       lty = c(1), lwd = c(1), horiz = F
)