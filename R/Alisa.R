rm(list = ls())

c(75, 87) -> interval
1000 -> freq

read.csv2("1.txt", header = TRUE) -> x
x$R1..mΩ[(min(interval) * freq):(max(interval) * freq)] / 1000.0 -> s
plot(seq(min(interval), max(interval), 1.0 / freq), s, type = 'l',
     main ="Сигнал электрического импеданса с собаки",
     xlab ="Время, с", ylab = "Электрический импеданс, Ом")

library(hht)
seq(0, max(interval) - min(interval), 1.0 / freq) -> t
ceemd.result <- CEEMD(s, t, noise.amp = 1e-09, trials = 1)
PlotIMFs(ceemd.result, imf.list = 1:6, time.span = c(0, max(t)))
