library(tidyverse)
source("funcs.R")

small <- readr::read_csv("https://raw.githubusercontent.com/sisl/AA228-CS238-Student/master/project2/data/small.csv")

summary(small)


#rand_test <- c(floor(runif(100,1,5)))

#readr::write_lines(rand_test, file = "small.policy")

trans <- small %>%
  group_by(s,a,sp) %>%
  summarise(num = n()) %>%
  group_by(s,a) %>%
  mutate(prob = num/sum(num)) %>%
  select(s,sp,a,prob)

rew <- small %>%
  group_by(s,a,r) %>%
  summarise(num = n()) %>%
  select(s,a,r)

policy <- tibble(s = unique(small$s), a = rep(2,100))

util_init <- rew %>%
  left_join(policy, by = "s") %>%
  filter(`a.x` == `a.y`) %>%
  select(s, u = r)


disc <- 0.95

df <- policy_eval(10, rew, policy, trans, disc)

df_norm <- df %>%
  ungroup() %>%
  #mutate(new = 10*(u/max(u))) %>%
  mutate(x = (s-1)%%10, y = ceiling(s/10))


ggplot(df_norm, aes(x = x, y = y)) +
  geom_tile(aes(fill = u), color = "black") +
  geom_text(aes(label = s))





#bellman(7, df, trans, policy, disc, rew)

test <- value_iter(10,util_init,rew,policy,trans,disc)

df <- policy_eval(10, rew, test, trans, disc)

df_norm <- df %>%
  ungroup() %>%
  #mutate(new = 10*(u/max(u))) %>%
  mutate(x = (s-1)%%10, y = ceiling(s/10))


ggplot(df_norm, aes(x = x, y = y)) +
  geom_tile(aes(fill = u), color = "black") +
  geom_text(aes(label = s))


submit <- test %>%
  arrange(s)

readr::write_lines(submit$a, file = "small.policy")
