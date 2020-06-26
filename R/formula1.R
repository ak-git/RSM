k <- 0.8
a <- 0.5

f <- function(x) {

  invHypot <- function(x, y) {
    result <- sqrt(x^2 + y^2)
    return(1 / result)
  }

  sum(sapply(1:1024, function(n) (k^n * (invHypot(1 - a, n * x) - invHypot(1 + a, n * x))))) -> result
  return(abs(result))
}

g <- function(x) {
  sum(sapply(1:1024, function(n) (k^n / n^3))) -> result
  return(abs(result * 2 * a / x^3))
}

exp(seq(log(0.01), log(100), length.out = 100)) -> x
sapply(x, f) -> y
sapply(x, g) -> y2

plot(x, y, type = "l", lwd = 2, log = "xy")
lines(x, y2, type = "l", lwd = 2, lty = 5)