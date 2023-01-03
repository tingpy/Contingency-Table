stan.data <- list(J = ncol(y_t), T = nrow(y_t), N_P = nrow(y_k), y_t = y_t, y_k = y_k, price_tag = d.train$Post, probs = probs)
fit01 <- stan(model_code = m01, data = stan.data, cores = 2, chains = 2, iter = 2000, warmup = 1000, seed = 929)
stan_trace(fit01, pars = c("gamma"), nrow = 4)
stan_trace(fit01, pars = c("lambda"), nrow = 4)
stan_trace(fit01, pars = c("theta_k[1,1]", "theta_k[1,2]", "theta_k[1,3]", "theta_k[1,4]",
                           "theta_k[1,5]", "theta_k[1,6]", "theta_k[1,7]", "theta_k[1,8]",
                           "theta_k[1,9]", "theta_k[1,10]", "theta_k[1,11]", "theta_k[1,12]",
                           "theta_k[1,13]", "theta_k[1,14]", "theta_k[1,15]", "theta_k[1,16]"), nrow = 4)
stan_trace(fit01, pars = c("theta_t[1,1]", "theta_t[1,2]", "theta_t[1,3]", "theta_t[1,4]",
                           "theta_t[1,5]", "theta_t[1,6]", "theta_t[1,7]", "theta_t[1,8]",
                           "theta_t[1,9]", "theta_t[1,10]", "theta_t[1,11]", "theta_t[1,12]",
                           "theta_t[1,13]", "theta_t[1,14]", "theta_t[1,15]", "theta_t[1,16]"), nrow = 4)

stan_ac(fit01, pars = c("gamma"), nrow = 4)
stan_ac(fit01, pars = c("lambda"), nrow = 4)
stan_ac(fit01, pars = c("theta_k[1,1]", "theta_k[1,2]", "theta_k[1,3]", "theta_k[1,4]",
                        "theta_k[1,5]", "theta_k[1,6]", "theta_k[1,7]", "theta_k[1,8]",
                        "theta_k[1,9]", "theta_k[1,10]", "theta_k[1,11]", "theta_k[1,12]",
                        "theta_k[1,13]", "theta_k[1,14]", "theta_k[1,15]", "theta_k[1,16]"), nrow = 4)
stan_ac(fit01, pars = c("theta_t[1,1]", "theta_t[1,2]", "theta_t[1,3]", "theta_t[1,4]",
                        "theta_t[1,5]", "theta_t[1,6]", "theta_t[1,7]", "theta_t[1,8]",
                        "theta_t[1,9]", "theta_t[1,10]", "theta_t[1,11]", "theta_t[1,12]",
                        "theta_t[1,13]", "theta_t[1,14]", "theta_t[1,15]", "theta_t[1,16]"), nrow = 4)

stan_hist(fit01, pars = c("gamma"), nrow = 4)
stan_hist(fit01, pars = c("lambda"), nrow = 4)
stan_hist(fit01, pars = c("theta_k[1,1]", "theta_k[1,2]", "theta_k[1,3]", "theta_k[1,4]",
                          "theta_k[1,5]", "theta_k[1,6]", "theta_k[1,7]", "theta_k[1,8]",
                          "theta_k[1,9]", "theta_k[1,10]", "theta_k[1,11]", "theta_k[1,12]",
                          "theta_k[1,13]", "theta_k[1,14]", "theta_k[1,15]", "theta_k[1,16]"), nrow = 4)
stan_hist(fit01, pars = c("theta_t[1,1]", "theta_t[1,2]", "theta_t[1,3]", "theta_t[1,4]",
                          "theta_t[1,5]", "theta_t[1,6]", "theta_t[1,7]", "theta_t[1,8]",
                          "theta_t[1,9]", "theta_t[1,10]", "theta_t[1,11]", "theta_t[1,12]",
                          "theta_t[1,13]", "theta_t[1,14]", "theta_t[1,15]", "theta_t[1,16]"), nrow = 4)
