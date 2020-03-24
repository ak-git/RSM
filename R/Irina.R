library(signal)
Fd <- 100
n <- 100
#bf <- remez(n = n, c(0.05, 0.95), c(1, 1), ftype = 'hilbert')
bf <- read.table(file = "hilbert.csv")
bf <- Arma(b = as.numeric(bf$V1), a = c(1))
freqz(bf)

t <- seq(0, 1, by = 1 / Fd)
x <- cos(2 * pi * (t) * 10 * t)
z <- filter(bf, x)

lenT <- length(t)
t <- t[1:(lenT - n / 2)]
x <- x[1:(lenT - n / 2)]
z <- z[(1 + n / 2):lenT]

envelope <- sqrt(x^2 + z^2)
freq <- diff(atan2(z, x) * Fd / (2.0 * pi))
freq[length(freq) + 1] <- freq[length(freq)]
freq[freq < 0] = Fd + freq[freq < 0]

par(mfrow = c(2, 1))
plot(t, x, type = "l")
lines(t, z, col = "red")
lines(t, envelope, col = 'green')
plot(t, freq, type = 'l', col = 'blue', ylim = range(0, 25))
