gpr_log_lik_bac_tra <- function(m,
                                a,
                                b,
                                c,
                                y,
                                D,
                                g,
                                p,
                                cc  = 1 / 2,
                                tau = 1 / 2){
  # Auxiliary Variables
  mm <- crossprod(g, p)
  tt <- cc * mm
  # Initialization
  aa <- tau
  # Computes the objective function
  f   <- gpr_log_lik(m   = m,
                     s2  = exp(a),
                     t2  = exp(b),
                     phi = exp(c),
                     y   = y,
                     D   = D)
  faa <- gpr_log_lik(m   = m,
                     s2  = exp(a + aa * p[1]),
                     t2  = exp(b + aa * p[2]),
                     phi = exp(c + aa * p[3]),
                     y   = y,
                     D   = D)
  while(faa - f < aa * tt){
    # Updates
    aa  <- aa * tau
    faa <- gpr_log_lik(m   = m,
                       s2  = exp(a + aa * p[1]),
                       t2  = exp(b + aa * p[2]),
                       phi = exp(c + aa * p[3]),
                       y   = y,
                       D   = D)
    }
  return(aa)
}