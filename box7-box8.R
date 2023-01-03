## eti for gamma
gamma_eti_4_1 <- c()
for(i in seq(length(gamma))) {
  gamma_eti_4_1 <- rbind(gamma_eti_4_1, data.frame(ci(gamma_post[i], method= "ETI", ci = 0.89)))
  }
gamma_eti_4_1 <- gamma_eti_4_1 %>% mutate(True_gamma = as.vector(gamma))
gamma_eti_4_1 <- gamma_eti_4_1 %>% mutate(check = True_gamma >= CI_low & True_gamma <= CI_high)
mean(gamma_eti_4_1$check)

## hpdi for gamma
gamma_hpdi_4_1 <- c()
for(i in seq(length(gamma))) {
  gamma_hpdi_4_1 <- rbind(gamma_hpdi_4_1, data.frame(ci(gamma_post[i], method = "HDI", ci = 0.89)))
  }
gamma_hpdi_4_1 <- gamma_hpdi_4_1 %>% mutate(True_gamma = as.vector(gamma))
gamma_hpdi_4_1 <- gamma_hpdi_4_1 %>% mutate(check = True_gamma >= CI_low & True_gamma <= CI_high)
mean(gamma_hpdi_4_1$check)

## eti for lambda
lambda_eti_4_1 <- c()
for(i in seq(length(lambda))) {
  lambda_eti_4_1 <- rbind(lambda_eti_4_1, data.frame(ci(lambda_post[i], method = "ETI", ci = 0.89)))
  }
lambda_eti_4_1 <- lambda_eti_4_1 %>% mutate(True_lambda = as.vector(lambda))
lambda_eti_4_1 <- lambda_eti_4_1 %>% mutate(check = True_lambda >= CI_low & True_lambda <= CI_high)

mean(lambda_eti_4_1$check)

## hpdi for lambda
lambda_hpdi_4_1 <- c()
for(i in seq(length(lambda))) {
  lambda_hpdi_4_1 <- rbind(lambda_hpdi_4_1, data.frame(ci(lambda_post[i], method = "HDI", ci = 0.89)))
  }
lambda_hpdi_4_1 <- lambda_hpdi_4_1 %>% mutate(True_lambda = as.vector(lambda))
lambda_hpdi_4_1 <- lambda_hpdi_4_1 %>% mutate(check = True_lambda >= CI_low& True_lambda <= CI_high)
mean(lambda_hpdi_4_1$check)

## eti for theta_k
theta_k_eti_4_1 <- c();theta_k_hpdi_4_1 <- c();theta_t_eti_4_1 <- c();theta_t_hpdi_4_1 <- c()
for(i in seq(ncol(theta_k_post))) {
  theta_k_eti_4_1 <- rbind(theta_k_eti_4_1, data.frame(ci(theta_k_post[i], method = "ETI", ci = 0.89)))
  }
theta_k_eti_4_1 <- theta_k_eti_4_1 %>% mutate(True_theta_k = as.vector(theta_k))
theta_k_eti_4_1 <- theta_k_eti_4_1 %>% mutate(check = True_theta_k >= CI_low& True_theta_k <= CI_high)

## hpdi for theta_k
for(i in seq(ncol(theta_k_post))) {
  theta_k_hpdi_4_1 <- rbind(theta_k_hpdi_4_1, data.frame(ci(theta_k_post[i], method = "HDI", ci = 0.89)))
  }
theta_k_hpdi_4_1 <- theta_k_hpdi_4_1 %>% mutate(True_theta_k = as.vector(theta_k))
theta_k_hpdi_4_1 <- theta_k_hpdi_4_1 %>% mutate(check = True_theta_k >= CI_low & True_theta_k <= CI_high)

## eti for theta_t
for(i in seq(ncol(theta_t_post))) {
  theta_t_eti_4_1 <- rbind(theta_t_eti_4_1, data.frame(ci(theta_t_post[i], method = "ETI", ci = 0.89)))
  }
theta_t_eti_4_1 <- theta_t_eti_4_1 %>% mutate(True_theta_t = as.vector(theta_t))
theta_t_eti_4_1 <- theta_t_eti_4_1 %>% mutate(check = True_theta_t >= CI_low& True_theta_t <= CI_high)

## hpdi for theta_t
for(i in seq(ncol(theta_t_post))) {
  theta_t_hpdi_4_1 <- rbind(theta_t_hpdi_4_1, data.frame(ci(theta_t_post[i], method = "HDI", ci = 0.89)))
  }
theta_t_hpdi_4_1 <- theta_t_hpdi_4_1 %>% mutate(True_theta_t = as.vector(theta_t))
theta_t_hpdi_4_1 <- theta_t_hpdi_4_1 %>% mutate(check = True_theta_t >= CI_low & True_theta_t <= CI_high)
mean(theta_k_eti_4_1$check);mean(theta_k_hpdi_4_1$check);mean(theta_t_eti_4_1$check);mean(theta_t_hpdi_4_1$check)