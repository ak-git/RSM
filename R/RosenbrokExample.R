func <- function(x) {
  rho1 <- x[1]
  rho2 <- x[2]

  R1Measured <- 100
  R2Measured <- 10

  R1Predicted <- rho1 * (rho1 + 2 * rho2)
  R2Predicted <- rho1 * (rho1 + rho2)

  error <- (R1Measured - R1Predicted) ^ 2 + (R2Measured - R2Predicted) ^ 2

  return(error)
}

optim(par = c(0, 0), fn = func, gr = NULL,
      method = c("Nelder-Mead"), lower = -Inf, upper = +Inf,
      control = list(), hessian = TRUE) -> out

out$par[1] -> x
out$par[2] -> y
