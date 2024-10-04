library('scales')

list.filesResult <- list.files(path = paste0(getwd(), "/R/plot_E_9322"), pattern = "force.csv")
df <- read.csv(paste0(getwd(), "/R/plot_E_9322/", list.filesResult))
area <- 0.015 * 0.045

par(mfcol = c(1, 1), mar = c(5, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(2)
lwd <- 3

main <- expression(Pressure ~ Z)
xlab <- expression(Z ~ mm)
ylab <- expression(mm ~ Hg)
PaToHg <- 0.00750062
plot(df$Coordinate.Z..mm, (df$Force.Down..N + 0.7121421012411667) / area * PaToHg,
     type = 'l', xlab = xlab, ylab = ylab, col = col[1], lwd = lwd, main = main)
lines(df$Coordinate.Z..mm, (df$Force.Up..N + 0.9643385268375497) / area * PaToHg,
     type = 'l', col = col[2], lwd = lwd)
