library('fBasics')
library('scales')

par(mfrow = c(2, 1), mar = c(5, 4, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1, font.axis = 2)
time <- 3
step <- 0.001
x <- seq(0, time, by = step)
y <- sapply(x, function(x) x * sin(2 * pi * x * x))
plot(x = x, y = y, lty = 'solid', type = 'l', lwd = 2, col = hue_pal()(2)[1], ylab = expression(x %.% sin(2 * pi %.% x %.% x)))

fft <- Mod(fft(y)) * step / time * 2
plot(x = x / time / step, y = fft, lty = 'blank', type = 'b', lwd = 2, col = hue_pal()(2)[2], xlab = expression(f), xlim = c(0, 10))
