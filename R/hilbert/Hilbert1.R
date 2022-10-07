library('fBasics')
library('scales')

from <- 0
to <- 2
n <- 1000
ylim <- c(-pi / 2, pi / 2)

par(mfrow = c(2, 1), mar = c(2, 3, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1, font.axis = 2)
f0 <- function(x) cos(2 * pi * x)
f1 <- function(x) cos(2 * pi * x - pi / 2)
curve(f0, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = hue_pal()(2)[1], ylim = ylim)
curve(f1, from = from, to = to, n = n, lty = 'dashed', lwd = 2, col = hue_pal()(2)[2], ylim = ylim, add = TRUE)
legend("topleft", y.intersp = 2,
       legend = c(expression(cos(2 * pi * x)), expression(sin(2 * pi * x))),
       lty = c('solid', 'dashed'), lwd = 2,
       col = hue_pal()(2))

f2 <- function(x) sin(2 * pi * x)
f3 <- function(x) sin(2 * pi * x - pi / 2)
curve(f2, from = from, to = to, n = n, lty = 'solid', lwd = 2, col = hue_pal(h.start = 90)(2)[1], ylim = ylim)
curve(f3, from = from, to = to, n = n, lty = 'dashed', lwd = 2, col = hue_pal(h.start = 90)(2)[2], ylim = ylim, add = TRUE)
legend("topleft", y.intersp = 2,
       legend = c(expression(sin(2 * pi * x)), expression(-cos(2 * pi * x))),
       lty = c('solid', 'dashed'), lwd = 2,
       col = hue_pal(h.start = 90)(2))

