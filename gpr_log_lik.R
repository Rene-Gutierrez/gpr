gpr_log_lik <- function(m, s2, t2, phi, y, D){
  N   <- dim(y)[2]
  Sig <- t2 * exp(- phi * D) + s2 * diag(N)
  ll  <- sum(mvtnorm::dmvnorm(x = y, m = rep(1, N) * m, sigma = Sig, log = TRUE))
  return(ll)
}