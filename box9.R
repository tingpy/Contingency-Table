# caculate LL1
theta_k_post <- post %>% select(., contains("theta_k"))
p_theta_t <- c();LL <- c();LL1 <- c()
for(i in seq(T-1)) {
  p_theta_k <- theta_k_post[seq(from = i, by = K, length.out = J)]
  for(j in seq(nrow(p_theta_t))) {
    LL <- c(LL, dmultinom(d_y_k[i, ], size = n_last_d[i], prob = as.numeric(
      p_theta_k[j, ]), log = T))
    }
  LL1 <- rbind(LL1, LL)
  LL <- c()
  }
sum(apply(LL1, 1, mean))
theta_t_post <- post %>% select(., contains("theta_t"))
p_theta_t <- c();LL <- c();LL1 <- c()
for(i in seq(T-1)) {
  p_theta_t <- theta_t_post[seq(from = i, by = T-1, length.out = J)]
  for(j in seq(nrow(p_theta_t))) {
    LL <- c(LL, dmultinom(d_y_t[i, ], size = 1, prob = as.numeric(p_theta_t[j, ]), log = T))
    }
  LL1 <- rbind(LL1, LL)
  LL <- c()
  }
sum(apply(LL1, 1, mean))
# caculate LL2
LL2 <- c()
for(i in seq(K)) {
  LL2 <- c(LL2, dmultinom(d_y_k[i, ], size = n_last_d[i], prob = theta_k[i, ], log = T))
  }
sum(LL2)
LL2 <- c()
for(i in seq(T-1)) {
  LL2 <- c(LL2, dmultinom(d_y_t[i, ], size = 1, prob = theta_t[i, ], log = T))
  }
sum(LL2)