library('fBasics')
library('scales')

time <- 4.0
step <- 0.001
x <- seq(0, time, by = step)
y1 <- sapply(x, function(x) sin(2 * pi * x))
fft1 <- Mod(fft(y1)) * step / time * 2
y2 <- sapply(x, function(x) x * sin(2 * pi * x))
fft2 <- Mod(fft(y2)) * step / time * 2
y3 <- sapply(x, function(x) sin(2 * pi * x * x))
fft3 <- abs(fft(y3)) * step / time * 2
y4 <- sapply(x, function(x) x * sin(2 * pi * x * x))
fft4 <- Mod(fft(y4)) * step / time * 2

par(mfrow = c(4, 2), mar = c(4, 4, 1, 1), cex = 1.0, family = 'mono', las = 1, tck = 1, font.axis = 2)
col <- hue_pal()(8)
plot(x = x / time / step, y = fft1, lty = 'blank', type = 'b', lwd = 2, col = col[1], xlab = expression(f), xlim = c(0, 5))
plot(x = x, y = y1, lty = 'solid', type = 'l', lwd = 2, col = col[2], ylab = expression(sin(2 * pi * x)))

plot(x = x / time / step, y = fft2, lty = 'blank', type = 'b', lwd = 2, col = col[3], xlab = expression(f), xlim = c(0, 5))
plot(x = x, y = y2, lty = 'solid', type = 'l', lwd = 2, col = col[4], ylab = expression(x %.% sin(2 * pi * x)))

plot(x = x / time / step, y = fft3, lty = 'blank', type = 'b', lwd = 2, col = col[5], xlab = expression(f), xlim = c(0, 5))
plot(x = x, y = y3, lty = 'solid', type = 'l', lwd = 2, col = col[6], ylab = expression(sin(2 * pi %.% x %.% x)))

plot(x = x / time / step, y = fft4, lty = 'blank', type = 'b', lwd = 2, col = col[7], xlab = expression(f), xlim = c(0, 10))
plot(x = x, y = y4, lty = 'solid', type = 'l', lwd = 2, col = col[8], ylab = expression(x %.% sin(2 * pi %.% x %.% x)))



plot(x = x / time / step, y = fft1, lty = 'blank', type = 'b', lwd = 2, col = col[1], xlab = expression(f), xlim = c(0, 5))
plot(x = x, y = y1, lty = 'solid', type = 'l', lwd = 2, col = col[2], ylab = expression(sin(2 * pi * x)))
plot(x = x / time / step, y = fft2, lty = 'blank', type = 'b', lwd = 2, col = col[3], xlab = expression(f), xlim = c(0, 5))
plot(x = x, y = y4, lty = 'solid', type = 'l', lwd = 2, col = col[4], ylab = expression(x %.% sin(2 * pi %.% x %.% x)))
plot(x = x / time / step, y = fft3, lty = 'blank', type = 'b', lwd = 2, col = col[5], xlab = expression(f), xlim = c(0, 5))
plot(x = x, y = y2, lty = 'solid', type = 'l', lwd = 2, col = col[6], ylab = expression(x %.% sin(2 * pi * x)))
plot(x = x / time / step, y = fft4, lty = 'blank', type = 'b', lwd = 2, col = col[7], xlab = expression(f), xlim = c(0, 10))
plot(x = x, y = y3, lty = 'solid', type = 'l', lwd = 2, col = col[8], ylab = expression(sin(2 * pi %.% x %.% x)))

