source("R/rsm.R")

layer1RelativeRhoErrorIfLayer2Model <- function (rho1, rho2, hmm, smm, lmm) {
  mmToSI(hmm) -> h
  mmToSI(smm) -> s
  mmToSI(lmm) -> l
  k <- rhoToK(rho1, rho2)

  sum(sapply(1:MAX_SUM, function(x) ((k ^ x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> result
  return(abs(2.0 / (mp(l - s) - mp(l + s)) * result))
}