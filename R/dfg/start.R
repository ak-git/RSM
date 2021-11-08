outFile <- "R/dfg/2021-10-25 17-23-43.csv"
aper <- read.csv("R/dfg/2021-10-25 17-23-43 246 aper.csv");
plog <- read.csv("R/dfg/2021-10-25 17-23-43 256 PureLogic.csv")

df <- merge(aper, plog)[(105 * 1000 + 1):(120 * 1000), c('TIME', 'R1', 'R2', 'POSITION')]

step <- 500
out <- sapply(1:(length(df$TIME) / step), function(x) {
  center <- x * step - step / 2 + 1
  interval <- (x * step - 3 * step / 7 + 1):(x * step - step / 7 + 1)
  c(df$TIME[center], mean(df$R1[interval]), mean(df$R2[interval]), df$POSITION[center])
})

out <- as.data.frame(t(out))
colnames(out) <- names(df)

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = 'R1')
lines(out$TIME, out$R1, type = 'b', lty = 'blank')

plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', ylab = 'R2')
lines(out$TIME, out$R2, type = 'b', lty = 'blank')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank')

write.csv(out, file = outFile, row.names = FALSE)