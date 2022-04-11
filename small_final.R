library(tidyverse)
source("funcs.R")

small <- readr::read_csv("https://raw.githubusercontent.com/sisl/AA228-CS238-Student/master/project2/data/small.csv")

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

policy <- value_iter(15,util_init,rew,policy,trans,disc)

submit <- policy %>%
  arrange(s)

readr::write_lines(submit$a, file = "small.policy")
