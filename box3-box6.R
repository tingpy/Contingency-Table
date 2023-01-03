stan.data <- list(J = J, N = nrow(d), N_P = K, d_m = d_y_t, d_mp = d_y_k, quote = d$Post, probs = probs)
fit01 <- stan(model_code = m01, data = stan.data, cores = 2, chains = 2, iter = 2000, warmup = 1500, seed = 929)
print(fit01, probs = c(.1, .5, .9))
post <- as.data.frame(fit01)
library(gridExtra)
gamma_post <- post %>% select(., contains("gamma"))
p <- list()
for(i in seq(length(gamma))) {
  # Compute HDI and ETI
  ci_hdi <- ci(gamma_post[, i], method = "HDI")
  ci_eti <- ci(gamma_post[, i], method = "ETI")
  
  p[[i]] <- gamma_post[, i] %>% 
    estimate_density(extend=TRUE) %>% 
    ggplot(aes(x=x, y=y)) +
    geom_area(fill="orange") +
    labs(y = "density") +
    ggtitle(paste("Posterior Samples for Gamma[", i, "]", sep=''), subtitle = "(a,b)=(4,1)") +
    theme_classic() +
    # HDI in blue
    geom_vline(xintercept=ci_hdi$CI_low, color="royalblue", size=1.2) +
    geom_vline(xintercept=ci_hdi$CI_high, color="royalblue", size=1.2) +
    # True values
    geom_vline(xintercept=gamma[i], color="dimgray", size=1.8) +
    # Quantile in red
    geom_vline(xintercept=ci_eti$CI_low, color="red", size=1) +
    geom_vline(xintercept=ci_eti$CI_high, color="red", size=1) +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) +
    theme(axis.title.x=element_blank())
}
do.call(grid.arrange,p)

lambda_post <- post %>% select(., contains("lambda"))
p <- list()
for(i in seq(length(lambda))) {
  # Compute HDI and ETI
  ci_hdi <- ci(lambda_post[, i], method = "HDI")
  ci_eti <- ci(lambda_post[, i], method = "ETI")
  
  p[[i]] <- log10(lambda_post[, i]) %>% 
    estimate_density() %>% 
    ggplot(aes(x=x, y=y)) +
    geom_area(fill="orange") +
    labs(y = "density") +
    ggtitle(paste("Posterior Samples for Lambda[", i, "] \n", sep='', subtitle = "(a,b)=(4,1)")) +
    theme_classic() +
    # HDI in blue
    geom_vline(xintercept=log10(ci_hdi$CI_low), color="royalblue", size=0.8) +
    geom_vline(xintercept=log10(ci_hdi$CI_high), color="royalblue", size=0.8) +
    # True values
    geom_vline(xintercept=log10(lambda[i]), color="dimgray", size=1.8) +
    # Quantile in red
    geom_vline(xintercept=log10(ci_eti$CI_low), color="red", size=1) +
    geom_vline(xintercept=log10(ci_eti$CI_high), color="red", size=1) +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) +
    theme(axis.title.x=element_blank())
}
do.call(grid.arrange,p)

theta_k_post <- post %>% select(., contains("theta_k")) %>% .[seq(from = 1, by = 10, length.out = 16)]
p <- list()
for(i in seq(ncol(theta_k_post))) {
  # Compute HDI and ETI
  ci_hdi <- ci(theta_k_post[, i], method = "HDI")
  ci_eti <- ci(theta_k_post[, i], method = "ETI")
  
  p[[i]] <- theta_k_post[, i] %>% 
    estimate_density(extend=TRUE) %>% 
    ggplot(aes(x=x, y=y)) +
    geom_area(fill="orange") +
    labs(y = "density") +
    ggtitle(paste("Posterior Samples for Theta_k[", i, "] \n", sep='', subtitle = "(a,b)=(4,1)")) +
    theme_classic() +
    # HDI in blue
    geom_vline(xintercept=ci_hdi$CI_low, color="royalblue", size=1.2) +
    geom_vline(xintercept=ci_hdi$CI_high, color="royalblue", size=1.2) +
    # True values
    geom_vline(xintercept=theta_k[1, ][i], color="dimgray", size=1.8) +
    # Quantile in red
    geom_vline(xintercept=ci_eti$CI_low, color="red", size=1) +
    geom_vline(xintercept=ci_eti$CI_high, color="red", size=1) +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) +
    theme(axis.title.x=element_blank())
}
do.call(grid.arrange,p)

theta_t_post <- post %>% select(., contains("theta_t")) %>% .[seq(from = 1, by = 599, length.out = 16)]
p <- list()
for(i in seq(ncol(theta_t_post))) {
  # Compute HDI and ETI
  ci_hdi <- ci(theta_t_post[, i], method = "HDI")
  ci_eti <- ci(theta_t_post[, i], method = "ETI")
  
  p[[i]] <- theta_t_post[, i] %>% 
    estimate_density(extend=TRUE) %>% 
    ggplot(aes(x=x, y=y)) +
    geom_area(fill="orange") +
    labs(y = "density") +
    ggtitle(paste("Posterior Samples for Theta_t[", i, "] \n", sep='', subtitle = "(a,b)=(4,1)")) +
    theme_classic() +
    # HDI in blue
    geom_vline(xintercept=ci_hdi$CI_low, color="royalblue", size=1.2) +
    geom_vline(xintercept=ci_hdi$CI_high, color="royalblue", size=1.2) +
    # True values
    geom_vline(xintercept=theta_t[1, ][i], color="dimgray", size=1.8) +
    # Quantile in red
    geom_vline(xintercept=ci_eti$CI_low, color="red", size=1) +
    geom_vline(xintercept=ci_eti$CI_high, color="red", size=1) +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) +
    theme(axis.title.x=element_blank())
}
do.call(grid.arrange,p)
