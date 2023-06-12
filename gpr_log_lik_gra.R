gpr_log_lik_gra <- function(m, s2, t2, phi, y, D){
  # Auxiliary Variables
  C       <- exp(- phi * D)
  K       <- t2 * C
  Sig     <- K + s2 * diag(N)
  Sig_Inv <- solve(Sig)
  r       <- Sig_Inv %*%  (y - m * rep(1, N))
  N       <- length(y)
  # Derivative of s2
  g_s2 <- - sum(diag(Sig_Inv)) / 2
  g_s2 <-   g_s2 + sum(r^2) / 2
  # Derivative of t2
  g_t2 <- - sum(Sig_Inv * C) / 2
  g_t2 <-   g_t2 + t(r) %*% C %*% r / 2
  # Derivative of phi
  g_phi <- sum(diag(Sig_Inv * K * D)) / 2
  g_phi <- g_phi - t(r) %*% (K * D) %*% r / 2
  return(c(g_s2, g_t2, g_phi))
}