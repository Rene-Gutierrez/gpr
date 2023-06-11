gpr_log_lik_gra <- function(m, s2, t2, phi, y, D){
  # Auxiliary Variables
  Sig     <- t2 * exp(- phi * D) + s2 * diag(N)
  Sig_Inv <- solve(Sig)
  r       <- Sig_Inv %*%  (y - m * rep(1, N))
  N       <- length(y)
  # Derivative of s2
  g_s2 <- - sum(diag(Sig_Inv)) / 2 
  g_s2 <- g_s2 - sum(r^2) / 2
  return(g_s2)
}