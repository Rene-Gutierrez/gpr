# Test GPR Log-likelihood
N <- 100
M <- 100
S <- matrix(data = NA, nrow = M * M, ncol = 2)
for(i in 1:M){
  for(j in 1:M){
    S[(i - 1) * M + j,] <- c(i, j) - M / 2
  }
}

X1 <- matrix(data = NA, nrow = N, ncol = M * M)

for(i in 1:N){
  X1[i,] <- matrix(data = mvtnorm::dmvnorm(x = S, mean = c(-M/4, M/4), sigma = runif(n = 1, min = 0, max = M) * diag(2)))
  xmax   <- max(X1[i,])
  xmin   <- min(X1[i,])
  X1[i,] <- (X1[i, ] - xmin) / xmax
}

X2 <- matrix(data = NA, nrow = N, ncol = M * M)

for(i in 1:N){
  X2[i,] <- matrix(data = mvtnorm::dmvnorm(x = S, mean = c(M/4, -M/4), sigma = runif(n = 1, min = 0, max = M) * diag(2)))
  xmax   <- max(X2[i,])
  xmin   <- min(X2[i,])
  X2[i,] <- (X2[i, ] - xmin) / xmax
}

X3 <- matrix(data = NA, nrow = N, ncol = M * M)

for(i in 1:N){
  X3[i,] <- matrix(data = mvtnorm::dmvnorm(x = S, mean = c(0, 0), sigma = runif(n = 1, min = 0, max = M) * diag(2)))
  xmax   <- max(X3[i,])
  xmin   <- min(X3[i,])
  X3[i,] <- (X3[i, ] - xmin) / xmax
}

plot(NULL, xlim = c(-M / 2, M / 2), ylim = c(-M / 2, M / 2))
rect(xleft   = S[, 1] - 1,
     ybottom = S[, 2] - 1,
     xright  = S[, 1],
     ytop    = S[, 2],
     border  = NA,
     col     = rgb(1, 0, 0, X3[3,]))

X <- rbind(X1, X2, X3)

xsel <- rbinom(n = M, size = 1, prob = 0.5)
ysel <- rbinom(n = M, size = 1, prob = 0.5)

Y <- matrix(data = NA, nrow = 3 * N, ncol = M * M)
for(i in 1:(3*N)){
  mX     <- matrix(data = X[i,], nrow = M, ncol = M)
  mY     <- kronecker(X = xsel, Y = t(colMeans(mX))) +  kronecker(X = rowMeans(mX), Y = t(ysel))
  Y[i, ] <- c(mY)
}

Ymax <- max(Y)
Ymin <- min(Y)
Y   <- (Y - Ymin) / (Ymax - Ymin)

for(i in 1:(3*N)){
  Y[i, ] <- c(Y[i,]) + rnorm(n = M * M, sd = 0.05)
}

plot(NULL, xlim = c(-M / 2, M / 2), ylim = c(-M / 2, M / 2))
Ymax <- max(Y)
Ymin <- min(Y)
pY   <- (Y - Ymin) / (Ymax - Ymin)
rect(xleft   = S[, 1] - 1,
     ybottom = S[, 2] - 1,
     xright  = S[, 1],
     ytop    = S[, 2],
     border  = NA,
     col     = rgb(1, 0, 0, pY[11,]))

D <- matrix(data = NA, nrow = 3*N, ncol = 3*N)
for(i in 1:(3*N)){
  for(j in 1:(3*N)){
    D[i, j] <- sum((X[i, ] - X[j, ])^2)
  }
}
