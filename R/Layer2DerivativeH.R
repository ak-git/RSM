dRdHLR <- function(k, hToL, sToL) {
  sum(sapply(1:MAX_SUM, function(x) {
    x ^ 2 * k ^ x * (-MP(1 - sToL, x, hToL) ^ 3 + MP(1 + sToL, x, hToL) ^ 3) -> result
    return(result)
  })) -> nom

  sum(sapply(1:MAX_SUM, function(x) {
    k ^ x * (MP(1 - sToL, x, hToL) - MP(1 + sToL, x, hToL)) -> result
    return(result)
  })) -> den

  (mp(1 - sToL) - mp(1 + sToL)) + 2 * den -> den

  8 * hToL * nom / den -> R
  return(R)
}

source('R/rsm.R')
exp(seq(log(0.01), log(1), length.out = 100)) -> hToL
sapply(hToL, function(x) dRdHLR(rhoToK(2, 1), x, 1 / 2)) -> y1
sapply(hToL, function(x) dRdHLR(rhoToK(1, 2), x, 1 / 2)) -> y2
par(mar = c(5, 6, 1, 1))
plot(hToL, y1, type = "l", lwd = 2, log = "x", ylim = c(-3.5, 3.5), lty = 1,
     xlab = expression(bold(frac(h, L))),
     ylab = expression(bold(frac(partialdiff ~ R, partialdiff ~ h) %.% frac(L, R))),
)
lines(hToL, y2, type = "l", lwd = 2, lty = 5)
abline(h = 0, col = 'black', lty = 1, lwd = 0.5)
legend("topright", inset = 0.01, box.col = 'white',
       title = expression(bold(frac(s, L) == frac(1, 2))),
       legend = c(
         expression(bold(frac(rho[1], rho[2]) == 2)),
         expression(bold(frac(rho[1], rho[2]) == frac(1, 2)))
       ),
       lty = c(1, 5), lwd = c(2, 2), horiz = F, y.intersp = 2
)
