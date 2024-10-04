dRdrho2N <- function(k, hToL, sToL) {
  sum(sapply(1:MAX_SUM, function(x) {
    x *
      k^(x - 1) *
      (MP(1 - sToL, x, hToL) - MP(1 + sToL, x, hToL)) -> result
    return(result)
  })) -> nom

  sum(sapply(1:MAX_SUM, function(x) {
    k^x * (MP(1 - sToL, x, hToL) - MP(1 + sToL, x, hToL)) -> result
    return(result)
  })) -> den

  (mp(1 - sToL) - mp(1 + sToL)) + 2 * den -> den

  (1 - k^2) * nom / den -> R
  return(R)
}

dRdrho1N <- function(k, hToL, sToL) {
  return(1 - dRdrho2N(k, hToL, sToL))
}

source('R/rsm.R')
exp(seq(log(0.01), log(1), length.out = 100)) -> hToL
sapply(hToL, function(x) dRdrho1N(rhoToK(2, 1), x, 1 / 2)) -> y1
sapply(hToL, function(x) dRdrho2N(rhoToK(2, 1), x, 1 / 2)) -> y2
par(mar = c(5, 6, 1, 1))
plot(hToL, y1, type = "l", lwd = 2, log = "x", ylim = c(0, 1), lty = 1,
     xlab = expression(bold(frac(h, L))),
     ylab = expression(bold(frac(partialdiff ~ R, partialdiff ~ rho[i]) %.% frac(rho[i], R))),
)
lines(hToL, y2, type = "l", lwd = 2, lty = 5)
abline(h = c(0:3), col = 'black', lty = 1, lwd = 0.5)
legend("right", inset = 0.01, box.col = 'white',
       title = expression(bold(list(frac(s, L) == frac(1, 2), phantom(0) ~ frac(rho[1], rho[2]) == 2))),
       legend = c(
         expression(bold(frac(partialdiff ~ R, partialdiff ~ rho[1]) %.% frac(rho[1], R))),
         expression(bold(frac(partialdiff ~ R, partialdiff ~ rho[2]) %.% frac(rho[2], R)))
       ),
       lty = c(1, 5), lwd = c(2, 2), horiz = F, y.intersp = 2
)