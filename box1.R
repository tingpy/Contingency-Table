rm(list = ls())
library(nnet)
library(tidyverse)
library(rstan)
library(bayestestR)
#
T <- 600
K <- 10
P <- 6
J <- 16
S <- 4
#
Sigma <- matrix(c(1, 0.4, 0.4^2, 0.4^3, 0.4^4,  0.4, 1, 0.4, 0.4^2, 0.4^3,
                  0.4^2, 0.4, 1, 0.4, 0.4^2, 0.4^3, 0.4^2, 0.4, 1, 0.4,
                  0.4^4, 0.4^3, 0.4^2, 0.4, 1), P-1, P-1)
mvn.Choleski <- function(n, mu, Sigma) {
  d <- length(mu)
  Q <- chol(Sigma)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z %*% Q + matrix(mu, n, d, byrow = T)
  X
}
set.seed(530)
X <- mvn.Choleski(T, rep(0, P-1), Sigma)

set.seed(929)
X <- cbind(sample(S, T, replace = T), X)
X <- as.data.frame(X)
colnames(X) <- c("state", "1", "2", "3", "4", "5")
X <- cbind(lead(X$state), X)
colnames(X) <- c("state1", "state", "1", "2", "3", "4", "5")
X <- X %>% subset(., !is.na(.$state1))
# coefficients for each choice
multinomModel <- multinom(state1 ~ ., data = X)
state <- X$state
probs <- c()
for(i in seq(sqrt(J))) {
  X$state <- i
  probs <- cbind(probs, predict (multinomModel, X, "probs"))
}
# theta'_k
set.seed(613)
gamma <- rdirichlet(1, alpha = rep(1, J))
lambda <- c()
set.seed(929)
lambda <- 6 * (1 - runif(J))^(-1/2)
alp <- gamma * lambda
theta_k <- c()
for(i in seq(K)) {
  theta_k <- rbind(theta_k, rdirichlet(1, alpha = alp))
}
#
set.seed(888)
d <- cbind.data.frame(Date = factor(seq(T-1)), Post = c(sample(seq(K)), sample(seq(K), size = T -1 - length(seq(K)), replace = TRUE)), X)
# construct d_y_k
n_last_d <- as.vector(table(d$Post))
d_y_k <- c()
f1 <- function(temp) {
  m = matrix(0, sqrt(J), sqrt(J))
  for(i in 1:length(temp)) {
    m[temp[i]] = m[temp[i]] + 1
  }
  return(t(m))
}

for(i in seq(K)) {
  temp <- t(rmultinom(n = 1,  size = n_last_d[i], prob = theta_k[i, ]))
  d_y_k <- rbind(d_y_k, temp)
}

# theta_t
theta_t <- c()
for(i in seq(T-1)) {
  theta_t <- rbind(theta_t, rdirichlet(1, alpha = exp(theta_k[d$Post[i], ] + probs[i, ])))
}

# construct d_y_t
d_y_t <- c()
for(i in seq(T-1)) {
  temp1 <- t(rmultinom(n = 1,  size = 1, prob = theta_t[i, ]))
  d_y_t <- rbind(d_y_t, temp1)
}