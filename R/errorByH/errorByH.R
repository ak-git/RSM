read.csv2("R/errorByH/x.txt", dec = ',', sep = "\t", header = FALSE) -> rho1ToRho2
as.numeric(rho1ToRho2) -> rho1ToRho2

read.csv2("R/errorByH/y.txt", dec = ',', header = FALSE) -> hL
hL$V1 -> hL

read.table("R/errorByH/eRho1ByEl.txt", dec = ',', header = FALSE, col.names = rho1ToRho2) -> eRho1
read.table("R/errorByH/eRho2ByEl.txt", dec = ',', header = FALSE, col.names = rho1ToRho2) -> eRho2
read.table("R/errorByH/eHByEl.txt", dec = ',', header = FALSE, col.names = rho1ToRho2) -> eH

plot(hL, eRho1$X10, type = 'l', lty = 1)
lines(hL, eRho1$X2, lty = 1)
