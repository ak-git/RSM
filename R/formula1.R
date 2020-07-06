k <- 0.1
a <- 0.1

f <- function(x) {

  invHypot <- function(x, y) {
    result <- sqrt(x^2 + y^2)
    return(1 / result)
  }

  sum(sapply(1:1024, function(n) (k^n * (invHypot(1 - a, n * x) - invHypot(1 + a, n * x))))) -> result
  return(abs(result))
}

gHigh <- function(x) {
  sum(sapply(1:1024, function(n) (k^n / n^3))) -> result
  return(abs(result * 2 * a / x^3))
}

gLow <- function(x) {
  sum(sapply(1:1024, function(n) (k^n))) -> result
  sum(sapply(1:1024, function(n) ((k^n) * (n^2)))) -> r2
  r2 * x^2 / 2 * (1 / (1 - a)^3 - 1 / (1 + a)^3) -> r2
  return(abs(2 * a * result / (1 - a^2) - r2))
}

exp(seq(log(0.01), log(10), length.out = 100)) -> x
sapply(x, f) -> y
sapply(x, gHigh) -> y2
sapply(x, gLow) -> y3

plot(x, y, type = "l", lwd = 2, log = "xy")
lines(x, y2, type = "l", lwd = 2, lty = 5)
lines(x, y3, type = "l", lwd = 2, lty = 6)