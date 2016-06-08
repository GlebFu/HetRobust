n <- 20
p <- 5
const <- c(rep(0, p), 1)
X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))

M <- chol2inv(chol(crossprod(X)))
H <- X %*% M %*% t(X)
h_i <- diag(H)
omega <- 1 / (1 - h_i)
g_i <- as.vector(X %*% M %*% const)
A_vec <- omega * g_i^2

eigen(crossprod(X))$values
eigen(M)$values
eigen(H)$values
eigen(diag(n) - H)$values
eig_set <- eigen(diag(A_vec) %*% (diag(n) - H))
lambda <- eig_set$values
hist(lambda)

eig_set$vectors
