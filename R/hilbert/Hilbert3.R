library('fBasics')

from <- 0
to <- 2
n <- 1000
ylim <- c(-pi / 2, pi / 2)

par(mfrow = c(2, 1), mar = c(2, 3, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
f0 <- function(x) sin(2 * pi * x * x)
f1 <- function(x) sin(2 * pi * x * x - pi / 2)
curve(exp = f0, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'red', font = 2, ylim = ylim)
curve(exp = f1, from = from, to = to, n = n, lty = 'dashed', lwd = 2, col = 'blue', font = 2, ylim = ylim, add = TRUE)
legend("topleft", y.intersp = 2,
       legend = c(expression(sin(2 * pi * x %.% x)), expression(-cos(2 * pi * x %.% x))),
       lty = c('solid', 'dashed'), lwd = 2,
       col = c('red', 'blue'))

f3 <- function(x) x
curve(exp = f3, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'orange', font = 2)
legend("topleft", y.intersp = 2, legend = expression(x), lty = 'solid', lwd = 2, col = 'orange')


