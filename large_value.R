library(tidyverse)
source("funcs.R")

large <- readr::read_csv("https://raw.githubusercontent.com/sisl/AA228-CS238-Student/master/project2/data/large.csv")

summary(large)


#rand_test <- c(floor(runif(100,1,5)))

#readr::write_lines(rand_test, file = "large.policy")

trans <- large %>%
  group_by(s,a,sp) %>%
  summarise(num = n()) %>%
  group_by(s,a) %>%
  mutate(prob = num/sum(num)) %>%
  select(s,sp,a,prob)

rew <- large %>%
  group_by(s,a,r) %>%
  summarise(num = n()) %>%
  select(s,a,r)

policy <- tibble(s = unique(large$s), a = rep(4,length(unique(large$s))))

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

test1 <- value_iter(20,util_init,rew,test,trans,disc)

#df <- policy_eval(10, rew, test, trans, disc)

write_csv(test,"large_iter1.csv")

test1 <- read_csv("large_iter1.csv")
df_norm <- df %>%
  ungroup() %>%
  #mutate(new = 10*(u/max(u))) %>%
  mutate(x = (s-1)%%10, y = ceiling(s/10))


ggplot(df_norm, aes(x = x, y = y)) +
  geom_tile(aes(fill = u), color = "black") +
  geom_text(aes(label = s))


submit <- test %>%
  arrange(s)


seq <- c(00,01,02,03,04,10,11,12,13,14,20)


test2 <- test1 %>%
  mutate(l1 = str_sub(s,1,1),
         l2 = str_sub(s,2,2),
         l3 = str_sub(s,3,3),
         l4 = str_sub(s,4,4),
         l5 = str_sub(s,5,5),
         l6 = str_sub(s,6,6)
         )


test3 <- test2 %>%
  filter(l1 == 15)


policy_knn <- tibble(s=seq(1:312020)) %>%
  mutate(s_update = str_pad(s, 6, pad = "0")) %>%
  mutate(l1 = str_sub(s_update,1,2),
         l2 = str_sub(s_update,3,4),
         l3 = str_sub(s_update,5,6)) %>%
  left_join(test3, by = c("l2","l3")) %>%
  mutate(a = factor(a))



policy_filled <- policy_knn %>%
  filter(is.na(a) == F) %>%
  select(l2,l3,l1=l1.x,a)


model2 <- randomForest(policy_filled[-3:-4],policy_filled[[4]], ntree = 500)

policy_unfilled <- policy_knn %>%
  filter(is.na(a) == T) %>%
  select(l2,l3,l1=l1.x,a)

preds <- predict(model2, newdata = policy_unfilled[-3:-4])


policy_unfilled$a <- preds

submit <- bind_rows(policy_unfilled,policy_filled) %>%
  mutate(s = paste0(l1,l2,l3)) %>%
  arrange(s) %>%
  filter(is.na(a) == FALSE) %>%
  select(s,a)


submit <- policy_knn %>%
  select(s.x,a_new) %>%
  arrange(s.x)


test2a <- test1 %>%
  mutate(l1 = str_sub(s,1,2),
         l2 = str_sub(s,3,4),
         l3 = str_sub(s,5,6)
  ) %>%
  group_by(a,l2) %>%
  summarise(l2_t = n())


df_test <- tibble(s = seq(1:312020)) %>%
  left_join(test1, by = "s") %>%
  rowwise() %>%
  mutate(a = as.numeric(case_when(
    is.na(a) == TRUE ~ 4,
    str_sub(s,3,4) %in% c("01","20") & str_sub(s,5,6) %in% c("01","20") ~ as.numeric(sample(5:9,1)),
    T ~ a
  )))

summary(submit)

submit<- test1 %>%
  arrange(s)

setdiff(large$s,large$sp)
length(unique(large$s))
test4 <- test3 %>%
  rowwise() %>%
  mutate(total = sum(l1,l2,l3,l4,l5,l6), first3 = paste0(l1,l2))
c(150:151)
ggplot(test4) +
  geom_bar(aes(x = l1))
spaces <- c("20","14","13","12","11","10","04","03","02","01")
unique(test4$l2)
test5 <- expand.grid(c(1:621),c(0,3,5,7,9),spaces,spaces)

readr::write_lines(df_test$a, file = "large.policy")
