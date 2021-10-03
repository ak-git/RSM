library('fBasics')

par(mfrow = c(2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
time <- 3
step <- 0.001
x <- seq(0, time, by = step)
f0 <- function(x) x * sin(2 * pi * x * x)
y <- sapply(x, f0)
plot(x = x, y = y, lty = 'solid', type = 'l', lwd = 2, col = 'red', font = 2)

fft <- abs(fft(y)) * step / time * 2
plot(x = x / time / step, y = fft, lty = 'blank', type = 'b', lwd = 2, col = 'blue', font = 2, xlab = expression(f), xlim = c(0, 10))
