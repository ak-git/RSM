library(signal)
Fd <- 2.5
n <- 20
bf <- remez(n = n, c(0.05, 0.95), c(1, 1), ftype = 'hilbert')
write.table(as.numeric(bf), file = 'hilbert.csv', row.names = FALSE, col.names = FALSE)
freqz(bf)

ohms <- read.csv2(file = 'ak.txt', header = FALSE)$V1 / 1000.0
t <- seq(0, (length(ohms) - 1) / Fd, by = 1 / Fd)
x <- ohms
plot(t, x, type = 'l')

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
plot(t, freq, type = 'l', col = 'blue', ylim = range(0, 2))
