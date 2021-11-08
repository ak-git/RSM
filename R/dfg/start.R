outFile <- "R/dfg/2021-10-25 17-23-43.csv"
aper <- read.csv("R/dfg/2021-10-25 17-23-43 246 aper.csv");
plog <- read.csv("R/dfg/2021-10-25 17-23-43 256 PureLogic.csv")

df <- merge(aper, plog)[(220 * 1000 + 1):(380 * 1000), c('TIME', 'R1', 'R2', 'POSITION')]

step <- 500
out <- sapply(1:(length(df$TIME) / step - 1), function(x) {
  center <- x * step - step / 2 + 1
  interval <- (x * step - 4 * step / 10 + 1):(x * step - step / 10 + 1)
  intervalNext <- interval + step
  c(df$TIME[center],
    mean(df$R1[interval]), mean(df$R1[intervalNext]),
    mean(df$R2[interval]), mean(df$R2[intervalNext]),
    df$POSITION[center],
    df$POSITION[center + step] - df$POSITION[center])
})

out <- as.data.frame(t(out))
colnames(out) <- c('TIME', 'R1', 'R1`', 'R2', 'R2`', 'POSITION', 'dh')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = 'R1')
lines(out$TIME, out$R1, type = 'b', lty = 'blank', col = 'red')

plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', ylab = 'R2')
lines(out$TIME, out$R2, type = 'b', lty = 'blank', col = 'orange')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank', col = 'blue')

write.csv(out, file = outFile, row.names = FALSE)