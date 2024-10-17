library('fBasics')
library('scales')

par(mfrow = c(1, 1), mar = c(5, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1, font.axis = 2)
col <- hue_pal()(2)
lwd <- 3

y <- c(0, 1, 2, 2, 2, 2, 1.5, 1.0, 0.5, 0)
z <- c(0, 1/3, 1, 5/3, 2, 2, 11/6, 1.5, 1.0, 0.5)
x <- seq(0, length(y) - 1, by = 1)

plot(x = x, y = y, lty = 'solid', type = 'b', lwd = lwd, col = col[1], ylab = expression(y), ylim = c(min(y, z), max(y, z)))
lines(x = x, y = z, lty = 'solid', type = 'b', lwd = lwd, col = col[2], ylab = expression(z))