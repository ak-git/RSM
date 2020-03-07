mmToSI <- function(mm) {
  return(mm / 1000.0);
}

layer1Ohms <- function(rho, smm, lmm = NULL) {
  R <- function(rho, s, l) {
    return((rho / pi) * (2.0 / abs(l - s) - 2.0 / (l + s)))
  }

  mmToSI(smm) -> s;
  if (is.null(lmm) || is.na(lmm) || is.nan(lmm)) {
    list(
      R(rho, s * 1, s * 3),
      R(rho, s * 3, s * 5),
      R(rho, s * 2, s * 4) / 2,
      R(rho, s * 2, s * 4) / 2 + R(rho, s * 4, s * 6) / 2,
      R(rho, s * 2, s * 4),
      R(rho, s * 4, s * 6)
    ) -> listResult;
    c(
      paste(smm * 1, 'x', smm * 3, 'mm', collapse = '; '),
      paste(smm * 3, 'x', smm * 5, 'mm', collapse = '; '),
      paste('[I - x - U - U - I - x]'),
      paste('[I - U - x - x - I - U]'),
      paste(smm * 2, 'x', smm * 4, 'mm', collapse = '; '),
      paste(smm * 4, 'x', smm * 6, 'mm', collapse = '; ')
    ) -> names(listResult);
    return(listResult);
  }
  else {
    mmToSI(lmm) -> l;
    return(R(rho, s, l));
  }
}

layer1Inverse <- function(smm, lmm, ohms) {
  mmToSI(smm) -> s;
  mmToSI(lmm) -> l;

  (ohms * pi) / (2.0 / abs(l - s) - 2.0 / (l + s)) -> rho;
  return(rho);
}
