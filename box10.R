library(tidyverse)
#
d <- read.csv("appendix_data.csv", head = T, sep = ",")
d <- d %>% mutate(state1 = lead(state))
d <- d %>% subset(., !is.na(.$state1))
d <- d %>% mutate(yt = max(d$state)*(state-1)+state1)
d$Index.of.price.tag <- ntile(d$Index.of.price.tag, 10)
#
S <- 4
J <- S ^ 2
T <- nrow(d)
#
f <- function(d.train) {
  m = matrix(0, S, S)
  for(i in 1:length(d.train$state)) {
    m[d.train$state[i],d.train$state1[i]] = m[d.train$state[i],d.train$state[i]] + 1
    }
  return(m)
}
y_k <- d[order(d$Index.of.price.tag), ] %>% group_by(Index.of.price.tag) %>%
  group_map(~ f(.x)) %>% lapply(., t) %>% lapply(., as.vector) %>% do.call(rbind, .)
d$Date <- as.Date(d$Date)
y_t <- d %>% group_by(Date) %>% group_map(~ f(.x)) %>% lapply(., t) %>% lapply(., as.vector) %>% do.call(rbind, .)
d <- subset(d, select = -Date )

d.train <- select(d, c("state", "state1", "Price", "Competitor1", "Competitor2", "Inventory1", "Inventory2", "Spread.1", "Spread.2"))
multinomModel <- multinom(state1 ~ ., data = d.train)
probs <- c()
for(i in seq(S)) {
  d.train$state <- i
  probs <- cbind(probs, predict (multinomModel, d.train, "probs"))
  }