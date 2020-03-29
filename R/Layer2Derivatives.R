dRdHLR <- function(k, hToL, sToL) {
  mp <- function(ls) {
    return(1 / abs(ls))
  }

  MP <- function(ls, n) {
    ls ^ 2 + (4 * n * hToL) ^ 2 -> result
    return(1 / sqrt(result))
  }

  sum(sapply(1:4096, function(x) {
    x ^ 2 * k ^ x * (-MP(1 - sToL, x) ^ 3 + MP(1 + sToL, x) ^ 3) -> result
    return(result)
  })) -> nom

  sum(sapply(1:4096, function(x) {
    k ^ x * (MP(1 - sToL, x) - MP(1 + sToL, x)) -> result
    return(result)
  })) -> den

  (mp(1 - sToL) - mp(1 + sToL)) + 2 * den -> den

  32 * hToL * nom / den -> R
  return(R)
}

source('R/rsm.R')
exp(seq(log(0.001), log(1), length.out = 100)) -> hToL
sapply(hToL, function(x) dRdHLR(rhoToK(10, 1), x, 1 / 2)) -> y
sapply(hToL, function(x) dRdHLR(rhoToK(2, 1), x, 1 / 2)) -> y2
par(mar = c(5, 6, 1, 1))
plot(hToL, y, type = "l", lwd = 2, log = "xy",
    xlab = expression(bold(frac(h, L))),
    ylab = expression(bold(frac(partialdiff~R, partialdiff~h)))
)
lines(hToL, y2, type = "l", lwd = 2, lty = 2)
legend("topleft", box.col = 'white', inset = 0.01,
    title = expression(bold(frac(s, L) == frac(1, 2))),
    legend = c(expression(bold(frac(rho[1], rho[2]) == 10)), expression(bold(frac(rho[1], rho[2]) == 2))),
    lty=c(1, 2), lwd = c(2, 2), horiz = F, y.intersp = 2
)
