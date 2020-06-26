a <- 1
k <- 1

f <- function(x) {
  sum(sapply(1:10240, function(n) (k^n / n^3))) -> result
  return(abs(1 + result * a / x^3))
}

g <- function(k) {
  sum(sapply(1:1024, function(n) (k^n / n^3))) -> result
  return(abs(result))
}

exp(seq(log(0.01), log(1), length.out = 100)) -> x
sapply(x, g) -> y
plot(x, y, type = "l", lwd = 2, log = "xy")

g(0.5)