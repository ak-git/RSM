mmToSI <- function(mm) {
  return(mm / 1000.0)
}

rhoToK <- function(rho1, rho2) {
  if (is.infinite(rho2)) {
    1 -> k
  }
  else {
    (rho2 - rho1) / (rho2 + rho1) -> k
  }
  return(k)
}

mp <- function(ls) {
  return(2 / abs(ls))
}

layer1Ohms <- function(rho, smm, lmm = NULL) {
  R <- function(rho, s, l) {
    return((rho / pi) * (mp(l - s) - mp(l + s)))
  }

  mmToSI(smm) -> s
  if (is.null(lmm) || is.na(lmm) || is.nan(lmm)) {
    list(
      R(rho, s * 1, s * 3),
      R(rho, s * 3, s * 5),
      R(rho, s * 2, s * 4) / 2,
      R(rho, s * 2, s * 4) / 2 + R(rho, s * 4, s * 6) / 2,
      R(rho, s * 2, s * 4),
      R(rho, s * 4, s * 6)
    ) -> listResult
    c(
      paste(smm * 1, 'x', smm * 3, 'mm', collapse = '; '),
      paste(smm * 3, 'x', smm * 5, 'mm', collapse = '; '),
      paste('[I - x - U - U - I - x]'),
      paste('[I - U - x - x - I - U]'),
      paste(smm * 2, 'x', smm * 4, 'mm', collapse = '; '),
      paste(smm * 4, 'x', smm * 6, 'mm', collapse = '; ')
    ) -> names(listResult)
    return(listResult)
  }
  else {
    mmToSI(lmm) -> l
    return(R(rho, s, l))
  }
}

layer1Inverse <- function(smm, lmm, ohms) {
  mmToSI(smm) -> s
  mmToSI(lmm) -> l

  (ohms * pi) / (mp(l - s) - mp(l + s)) -> rho
  return(rho)
}

MP <- function(ls, n, h) {
  ls^2 + (4 * n * h)^2 -> result
  return(2 / sqrt(result))
}

MAX_SUM <- 4096

layer2Ohms <- function(rho1, rho2, hmm, smm, lmm) {
  rhoToK(rho1, rho2) -> k
  mmToSI(hmm) -> h
  mmToSI(smm) -> s
  mmToSI(lmm) -> l

  sum(sapply(1:MAX_SUM, function(x) ((k^x) * (MP(l - s, x, h) - MP(l + s, x, h))))) -> R

  mp(l - s) - mp(l + s) + 2 * R -> R

  (rho1 / pi) * R -> R
  return(R)
}