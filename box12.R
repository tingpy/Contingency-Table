posterior <- as.data.frame(fit01)
x <- c()
pred.y <- c()
for(i in seq(nrow(d.test))) {
  post_theta_k <- posterior %>% select(., contains("theta_k")) %>% apply(., 2, mean) %>% .[seq(from = d.test$Post[i], by = nrow(y_k), length.out = ncol(y_t))]
  d.train <- rbind(d.train, d.test[i, ])
  df.train <- select(d.train, c("state", "state1", "Price", "台化", "LG", "last7", "last21", "spread1", "spread2"))
  
  multinomModel <- multinom(state1 ~ ., df.train[seq(nrow(df.train)-1), ])
  probs <- c()
  for(k in seq(sqrt(ncol(y_t)))) {
    df.train$state <- k
    probs <- cbind(probs, predict (multinomModel, df.train, "probs"))
  }
  set.seed(689)
  t_m <- exp(post_theta_k + tail(probs, 1)) %>% matrix(., sqrt(ncol(y_t)), sqrt(ncol(y_t)), byrow = T)
  for(j in seq(nrow(t_m))) {
    t_m[j, ] <- t_m[j, ] / sum(t_m[j, ])
  }
  pred.y <- c(pred.y, which.max(t_m[d.test$state[i], ]))
  cat("第", i, "次預測, 加油!", "\n")
  if (pred.y[i] == d.test$state1[i]) {
    x <- c(x, as.integer(pred.y[i] == d.test$state1[i]))
    cat("第", i, "次預測 0", "\n")
    cat("累積正確率 = ", (cumsum(x)[i])/i, "\n")
  }
  else {
    x <- c(x, as.integer(pred.y[i] == d.test$state1[i]))
    cat("第", i, "次預測 X", "\n")
    cat("state 為", d.test$state[i], "\n")
    cat("state1 應為", d.test$state1[i], "預測為", pred.y[i], "\n")
    cat("theta_k = ", post_theta_k , "\n", "phi_t = ", tail(probs, 1), "\n")
    cat("累積正確率 = ", (cumsum(x)[i])/i, "\n")
  }
}