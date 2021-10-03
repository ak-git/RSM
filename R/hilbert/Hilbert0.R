library('fBasics')

from <- 0
to <- 2
n <- 1000
ylim <- c(-2, 2)

par(mfrow = c(4, 1), mar = c(2, 3, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
f0 <- function(x) sin(2 * pi * x)
curve(f0, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'red', font = 2, ylim = ylim)
legend("topleft", legend = expression(sin(2 * pi * x)), lty = 'solid', lwd = 2, col = 'red', y.intersp = 2)

f1 <- function(x) x * f0(x)
curve(exp = f1, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'green', font = 2, ylim = ylim)
legend("topleft", legend = expression(x %.% sin(2 * pi * x)), lty = 'solid', lwd = 2, col = 'green', y.intersp = 2)
abline(a = 0, b = 1, lty = 'dashed', lwd = 2, col = 'black', font = 2)

f2 <- function(x) sin(2 * pi * x * x)
curve(exp = f2, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'blue', font = 2, ylim = ylim)
legend("topleft", legend = expression(sin(2 * pi * x %.% x)), lty = 'solid', lwd = 2, col = 'blue', y.intersp = 2)

f3 <- function(x) x * f2(x)
curve(exp = f3, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'orange', font = 2, ylim = ylim)
legend("topleft", legend = expression(x %.% sin(2 * pi * x %.% x)), lty = 'solid', lwd = 2, col = 'orange', y.intersp = 2)
abline(a = 0, b = 1, lty = 'dashed', lwd = 2, col = 'black', font = 2)
