source("R/rsm.R")

hToL_HorizontMax <- function(sToL = 1 / 3, deltaL, k = 1) {
  sum(sapply(1:MAX_SUM, function(n) (k^n / n^3))) -> result
  return((abs(sToL * (1 - sToL)^2 * result / (32 * deltaL)))^(1 / 3))
}

hToL_HorizontMin <- function(sToL = 1 / 3, deltaL, k = 1) {
  x <- sToL
  sum(sapply(1:MAX_SUM, function(n) (k^n * n^2))) -> r
  r <- deltaL * (1 + k) / (1 - k) / r
  r <- r * (1 - x) * (1 + x)^3 / 16 / x / (x^2 + 3)
  return(sqrt(abs(r)))
}

L <- 10
deltaL <- 0.1 / L
sToL <- 1 / 3
exp(seq(log(0.01), log(0.99), length.out = 100)) -> k
sapply(k, function(x) (L * hToL_HorizontMax(sToL = sToL, deltaL = deltaL, k = x))) -> h1
sapply(k, function(x) (L * hToL_HorizontMin(sToL = sToL, deltaL = deltaL, k = x))) -> h2
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(k, h1, type = "l", lwd = 2, log = "xy", ylim = c(min(h2), max(h1)))
lines(k, h2, type = "l", lwd = 2, lty = 5)

seq(-0.99, 0.99, length.out = 100) -> k
sapply(k, function(x) (L * hToL_HorizontMax(sToL = sToL, deltaL = deltaL, k = x))) -> h1
sapply(k, function(x) (L * hToL_HorizontMin(sToL = sToL, deltaL = deltaL, k = x))) -> h2
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(k, h1, type = "l", lwd = 2, ylim = c(min(h2), max(h1)))
lines(k, h2, type = "l", lwd = 2, lty = 5)


library('pracma')
hToLHorizontEqual <- function(sToL = 1 / 3, deltaL = 0.01, k) {
  return(hToL_HorizontMax(sToL = sToL, deltaL = deltaL, k = k) - hToL_HorizontMin(sToL = sToL, deltaL = deltaL, k = k))
}

hToLFindPlus <- function(sToL = 1 / 3, deltaL = 0.01) {
  k <- uniroot(function(x) (hToLHorizontEqual(sToL = sToL, deltaL = deltaL, k = x)), lower = 0.00001, upper = 0.99)$root
  return(hToL_HorizontMax(sToL = sToL, deltaL = deltaL, k = k))
}
hToLFindMinus <- function(sToL = 1 / 3, deltaL = 0.01) {
  k <- uniroot(function(x) (hToLHorizontEqual(sToL = sToL, deltaL = deltaL, k = x)), upper = -0.00001, lower = -0.99)$root
  return(hToL_HorizontMax(sToL = sToL, deltaL = deltaL, k = k))
}

sToL <- 1 / 3
seq(0.01, 0.1, length.out = 100) -> deltaL
sapply(deltaL, function(x) (hToLFindPlus(sToL = sToL, deltaL = x))) -> sL1
sapply(deltaL, function(x) (hToLFindMinus(sToL = sToL, deltaL = x))) -> sL2
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(deltaL, sL1, type = "l", lwd = 2, log = "y", ylim = c(min(sL1, sL2), max(sL1, sL2)), ylab = expression(bold(frac(h, L))))
lines(deltaL, sL2, type = "l", lwd = 2, lty = 5)
legend("topright", box.col = 'black',
       title = expression(bold(frac(s, L) == frac(1, 3))),
       legend = c(
         expression(bold(k > 0)),
         expression(bold(k < 0))
       ),
       lty = c(1, 5), lwd = c(2, 2), horiz = T
)


seq(0.1, 0.9, length.out = 100) -> sToL
sapply(sToL, function(x) ((hToLFindPlus(sToL = x, deltaL = 0.001) + hToLFindMinus(sToL = x, deltaL = 0.001)) / 2.0)) -> hToL
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(sToL, hToL, type = "l", lwd = 2, ylab = expression(bold(frac(h, L))))

write.csv(as.data.frame(sToL), file = 'sToL.csv', row.names = FALSE)
write.csv(as.data.frame(hToL), file = 'hToL.csv', row.names = FALSE)