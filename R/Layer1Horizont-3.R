source("R/rsm.R")

apparentRho <- function(rho1 = 1, rho2 = Inf, hToL, sToL = 1 / 3) {
  l <- 1
  s <- sToL
  h <- hToL
  k <- rhoToK(rho1, rho2)

  sum(sapply(1:MAX_SUM, function(x) ((k^x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> result
  rhoApparent <- rho1 * (1.0 + 2.0 / (mp(l - s) - mp(l + s)) * result)
  return(rhoApparent)
}

hToL_Horizont <- function(deltaL, k = 1) {
  sum(sapply(1:MAX_SUM, function(n) (k^n / n^3))) -> result
  return((abs(result / deltaL))^(1 / 3) / 6)
}

rho1 <- 1
rho2 <- 2
k <- rhoToK(rho1 = rho1, rho2 = rho2)

exp(seq(log(0.01), log(2), length.out = 100)) -> hToL
sapply(hToL, function(x) (apparentRho(hToL = x))) -> y1
par(mar = c(5, 6, 1, 1), mfrow = c(1, 1))
plot(hToL, y1, type = "l", lwd = 2, log = "xy", ylim = c(rho1, rho2),
     xlab = expression(bold(frac(h, L))),
     ylab = expression(bold(frac(rho, rho[1])))
)

sapply(hToL, function(x) (apparentRho(hToL = x, rho2 = rho2))) -> y2
lines(hToL, y2, type = "l", lwd = 2, lty = 5)
abline(h = c(1, rho2))

dL <- 0.1
L <- 30
deltaL <- dL / L

abline(h = 6 * deltaL + 1, lty = 3, lwd = 2)
text(x = 0.1, y = 6 * deltaL + 1, expression(delta ~ rho[1] == "2 %"),
     pos = 3, srt = 0)

abline(v = hToL_Horizont(deltaL = deltaL), lty = 3, lwd = 2)
text(x = hToL_Horizont(deltaL = deltaL, k = rhoToK(1, 2)) - 0.05, y = 1.2,
     expression('0,785'),
     pos = 3, srt = 90)

abline(v = hToL_Horizont(deltaL = deltaL, k = k), lty = 3, lwd = 2)
text(x = hToL_Horizont(deltaL = deltaL) - 0.05, y = 1.2, expression('1,185'), pos = 3, srt = 90)

hToL_HorizontMin <- function(deltaL, k = 1) {
  x <- 1 / 3
  sum(sapply(1:MAX_SUM, function(n) (k^n * n^2))) -> r
  r <- deltaL * (1 + k) / (1 - k) / r
  r <- r * (1 - x) * (1 + x)^3 / 16 / x / (x^2 + 3)
  return(sqrt(abs(r)))
}

abline(h = rho2 * (1 - 6 * deltaL), lty = 3, lwd = 2)
text(x = 0.1, y = rho2 * (1 - 6 * deltaL), expression(delta ~ rho[2] == "2 %"),
     pos = 1, srt = 0)

abline(v = hToL_HorizontMin(deltaL = deltaL, k = k), lty = 3, lwd = 2)
text(x = hToL_HorizontMin(deltaL = deltaL, k = k) - 0.001, y = 1.2, expression('0,02'), pos = 3, srt = 90)
legend("topright", box.col = 'black',
       title = expression(bold(frac(s, L) == frac(1, 3))),
       legend = c(
         expression(bold(rho[2] == infinity)),
         expression(bold(frac(rho[1], rho[2]) == frac(1, 2)))
       ),
       lty = c(1, 5), lwd = c(2, 2), horiz = T
)