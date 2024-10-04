111 -> xMax

read.table("R/errorByErrorL/y.txt", dec = ",") -> x
x$V1[1:xMax] -> x

read.table("R/errorByErrorL/eH.txt", dec = ",") -> eH
eH$V1[1:xMax] -> eHMinusL
eH$V2[1:xMax] -> eHPlusL

read.table("R/errorByErrorL/eRho1.txt", dec = ",") -> eRho1
eRho1$V1[1:xMax] -> eRho1MinusL
eRho1$V2[1:xMax] -> eRho1PlusL

read.table("R/errorByErrorL/eRho2.txt", dec = ",") -> eRho2
eRho2$V1[1:xMax] -> eRho2MinusL
eRho2$V2[1:xMax] -> eRho2PlusL

plot(x, eHPlusL, type = "l", lty = 1)
lines(x, eHMinusL, lty = 2)
lines(x, eRho1PlusL, lty = 3)
lines(x, eRho1MinusL, lty = 4)
lines(x, eRho2PlusL, lty = 5)
lines(x, eRho2MinusL, lty = 6)
