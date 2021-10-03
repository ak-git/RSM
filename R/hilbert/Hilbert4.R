library('fBasics')

from <- 0
to <- 2
n <- 1000
ylim <- c(-2, 2)

par(mfrow = c(1, 1), mar = c(2, 3, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
f0 <- function(x) x * sin(2 * pi * x * x)
f1 <- function(x) x * sin(2 * pi * x * x - pi / 2)
curve(exp = f0, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = 'red', font = 2, ylim = ylim)
curve(exp = f1, from = from, to = to, n = n, lty = 'dashed', lwd = 2, col = 'blue', font = 2, ylim = ylim, add = TRUE)
legend("topleft", y.intersp = 2,
       legend = c(expression(x %.% sin(2 * pi * x %.% x)), expression(-x %.% cos(2 * pi * x %.% x))),
       lty = c('solid', 'dashed'), lwd = 2,
       col = c('red', 'blue'))
abline(a = 0, b = 1, lty = 'dashed', lwd = 2, col = 'black', font = 2)