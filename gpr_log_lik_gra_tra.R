gpr_log_lik_gra_tra <- function(m, a, b, c, y, D){
  # Auxiliary Variables
  s2      <- exp(a)
  t2      <- exp(b)
  phi     <- exp(c)
  C       <- exp(- phi * D)
  K       <- t2 * C
  Sig     <- K + s2 * diag(N)
  Sig_Inv <- solve(Sig)
  r       <- Sig_Inv %*%  (y - m * rep(1, N))
  N       <- length(y)
  # Derivative of s2
  g_a <- - sum(diag(Sig_Inv)) * s2 / 2
  g_a <-   g_a + sum(r^2) * s2 / 2
  # Derivative of t2
  g_b <- - sum(Sig_Inv * K) / 2
  g_b <-   g_b + t(r) %*% K %*% r / 2
  # Derivative of phi
  g_c <- sum(Sig_Inv * (K * D)) * phi / 2
  g_c <- g_c - t(r) %*% (K * D) %*% r * phi / 2
  return(c(g_a, g_b, g_c))
}