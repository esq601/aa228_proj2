library(tidyverse)
source("funcs.R")
library(data.table)


df <- expand.grid(c(0:2),c(0:2),c(0:2),c(0:2),c(0:2),c(0:2),c(0:2),c(0:2),c(0:2))


df1 <- read_csv("unique_states.csv", col_names = FALSE)[-1,-1] %>%
  rename(Var1 = X2, Var2 = X3, Var3 = X4,
         Var4 = X5, Var5 = X6, Var6 = X7,
         Var7 = X8, Var8 = X9, Var9 = X10) %>%
  mutate(ct1 = apply(X=.,1,FUN=function(x) length(which(x==1))),
         ct2 = apply(X=.,1,FUN=function(x) length(which(x==2))),
         ct0 = apply(X=.,1,FUN=function(x) length(which(x==0)))) %>%
  mutate(r = case_when(
    Var1 == 1 & Var2 == 1 & Var3 == 1 & ct1 >= ct2 ~ 10,
    Var1 == 1 & Var4 == 1 & Var7 == 1 & ct1 >= ct2  ~ 10,
    Var1 == 1 & Var5 == 1 & Var9 == 1 & ct1 >= ct2  ~ 10,
    Var2 == 1 & Var5 == 1 & Var8 == 1 & ct1 >= ct2  ~ 10,
    Var3 == 1 & Var6 == 1 & Var9 == 1 & ct1 >= ct2  ~ 10,
    Var4 == 1 & Var5 == 1 & Var6 == 1 & ct1 >= ct2  ~ 10,
    Var7 == 1 & Var8 == 1 & Var9 == 1 & ct1 >= ct2  ~ 10,
    Var3 == 2 & Var5 == 2 & Var7 == 2 & ct2 >= ct1 ~ -10,
    Var1 == 2 & Var2 == 2 & Var3 == 2 & ct2 >= ct1 ~ -10,
    Var1 == 2 & Var4 == 2 & Var7 == 2 & ct2 >= ct1 ~ -10,
    Var1 == 2 & Var5 == 2 & Var9 == 2 & ct2 >= ct1 ~ -10,
    Var2 == 2 & Var5 == 2 & Var8 == 2 & ct2 >= ct1 ~ -10,
    Var3 == 2 & Var6 == 2 & Var9 == 2 & ct2 >= ct1 ~ -10,
    Var4 == 2 & Var5 == 2 & Var6 == 2 & ct2 >= ct1 ~ -10,
    Var7 == 2 & Var8 == 2 & Var9 == 2 & ct2 >= ct1 ~ -10,
    Var3 == 2 & Var5 == 2 & Var7 == 2 & ct2 >= ct1 ~ -10,
    T ~ 0)) %>%
  mutate(r = case_when(
    ct0 == 0 & r < 9.5 & r > -9.5 ~ -1,
    T ~ r
  )) %>%
  mutate(s = paste0(Var1,Var2,Var3,Var4,Var5,Var6,Var7,Var8,Var9))


# df1 <- df %>%
#   mutate(ct1 = apply(X=.,1,FUN=function(x) length(which(x==1))),
#          ct2 = apply(X=.,1,FUN=function(x) length(which(x==2))),
#          ct0 = apply(X=.,1,FUN=function(x) length(which(x==0)))) %>%
#   filter(abs(ct1-ct2) <= 1) %>%
#   mutate(r = case_when(
#     Var1 == 1 & Var2 == 1 & Var3 == 1 & ct1 > ct2 ~ 10,
#     Var1 == 1 & Var4 == 1 & Var7 == 1 & ct1 > ct2  ~ 10,
#     Var1 == 1 & Var5 == 1 & Var9 == 1 & ct1 > ct2  ~ 10,
#     Var2 == 1 & Var5 == 1 & Var8 == 1 & ct1 > ct2  ~ 10,
#     Var3 == 1 & Var6 == 1 & Var9 == 1 & ct1 > ct2  ~ 10,
#     Var4 == 1 & Var5 == 1 & Var6 == 1 & ct1 > ct2  ~ 10,
#     Var7 == 1 & Var8 == 1 & Var9 == 1 & ct1 > ct2  ~ 10,
#     Var3 == 2 & Var5 == 2 & Var7 == 2 & ct2 > ct1 ~ -10,
#     Var1 == 2 & Var2 == 2 & Var3 == 2 & ct2 > ct1 ~ -10,
#     Var1 == 2 & Var4 == 2 & Var7 == 2 & ct2 > ct1 ~ -10,
#     Var1 == 2 & Var5 == 2 & Var9 == 2 & ct2 > ct1 ~ -10,
#     Var2 == 2 & Var5 == 2 & Var8 == 2 & ct2 > ct1 ~ -10,
#     Var3 == 2 & Var6 == 2 & Var9 == 2 & ct2 > ct1 ~ -10,
#     Var4 == 2 & Var5 == 2 & Var6 == 2 & ct2 > ct1 ~ -10,
#     Var7 == 2 & Var8 == 2 & Var9 == 2 & ct2 > ct1 ~ -10,
#     Var3 == 2 & Var5 == 2 & Var7 == 2 & ct2 > ct1 ~ -10,
#     T ~ 0)) %>%
#   mutate(r = case_when(
#     ct0 == 0 & r < 9.5 & r > -9.5 ~ -1,
#     T ~ r
#   )) %>%
#   #filter(ct1 == ct2) %>%
#   # filter(!(r == 10 & ct2 == ct1)) %>%
#   # filter(!(r == -10 & ct2 == ct1)) %>%
#   # filter(r == 10 & ct1 > ct2 | r < 10) %>%
#   # filter(r == -10 & ct2 > ct1 | r > -10) %>%
#   mutate(s = paste0(Var1,Var2,Var3,Var4,Var5,Var6,Var7,Var8,Var9)) #%>%
#  select(s,r)

trans <- df1 %>%
  #filter(ct1 <= ct2+1) %>%
  pivot_longer(cols = contains("Var")) %>%
  mutate(a = str_sub(name,4,4)) %>%
  #filter(ct1 <= ct2) %>%
  mutate(sp=as.character(s))

#trans <- bind_rows(trans,trans)
length(unique(trans$s))

sp <- trans$sp
sp_rev <- trans$sp
#substr(x=sp[1],start = trans[1,8],stop = trans[1,8]) <- "1"

for(i in 1:as.integer(length(sp))) {
  substr(sp[i],trans[i,8],trans[i,8]) <- "1"
}

for(i in 1:as.integer(length(sp))) {
  substr(sp_rev[i],trans[i,8],trans[i,8]) <- "2"
}


#### Oppo


small_oppo <- trans %>%
  bind_cols(sp_rev) %>%
  mutate(a = as.numeric(a)+20) %>%
  filter(value == 0 | str_detect(s,"0") == FALSE) %>%
  mutate(`...10` = case_when(
    r == 10 | r == -10 | r == -1 ~ s,
    T ~ `...10`
  ))
#select(s,a,r,sp=`...10`)

trans <- data.table(trans)
small <- data.table(small)


trans_new_oppo <- small_oppo %>%
  filter(ct2 <= ct1+1) %>%
  #mutate(a = as.numeric(a) + 20) %>%
  # filter(!str_detect(str_sub(s,1,3),"222") &
  #          !str_detect(str_sub(s,4,6),"222") &
  #          !str_detect(str_sub(s,7,9),"222") &
  #          !str_detect(paste0(str_sub(s,1,1),str_sub(s,5,5),str_sub(s,9,9)),"222") &
  #          !str_detect(paste0(str_sub(s,2,2),str_sub(s,5,5),str_sub(s,8,8)),"222") &
  #          !str_detect(paste0(str_sub(s,3,3),str_sub(s,5,5),str_sub(s,7,7)),"222") &
  #          !str_detect(paste0(str_sub(s,1,1),str_sub(s,4,4),str_sub(s,7,7)),"222") &
  #          !str_detect(paste0(str_sub(s,2,2),str_sub(s,5,5),str_sub(s,8,8)),"222") &
  #          !str_detect(paste0(str_sub(s,3,3),str_sub(s,6,6),str_sub(s,9,9)),"222")) %>%
  group_by(s,a,sp=`...10`) %>%
  summarise(num = n()) %>%
  group_by(s,a) %>%
  mutate(prob = num/sum(num)) %>%
  select(s,sp,a,prob)



rew_oppo <- small_oppo %>%
  group_by(s,a,r) %>%
  summarise(num = n()) %>%
  select(s,a,r)

policy_oppo <- tibble(s = unique(small_oppo$s), a = rep(1,length(unique(small_oppo$s))))

util_init_oppo <- policy_oppo %>%
  left_join(rew_oppo, by = "s") %>%
  select(s, u = r) %>%
  ungroup() %>%
  #mutate(s = factor(r)) %>%
  distinct() %>%
  #filter(`a.x` == `a.y`) %>%
  
  arrange(desc(s))


disc <- 0.9
trans_total <- data.table(bind_rows(trans_new_oppo,trans_new)) %>%
  mutate(ct1 = str_count(sp,"1"),
         ct2 = str_count(sp,"2"),
         ct0 = str_count(sp,"0")) %>%
  filter(abs(ct1-ct2) <= 1) 

rew_oppo <- data.table(rew_oppo)
trans_total <- data.table(trans_total)
policy_oppo <- data.table(policy_oppo)
util_init_oppo <- data.table(util_init_oppo)

#util_new <- policy_eval(5,rew,policy,trans_new,disc)

#policydf <- policy$policy$policy

trans_total[s == "000102000"]

func_out_oppo <- value_iter(10,"opp",util_init_oppo,rew_oppo,
                            policy_oppo,trans_total,disc)


test1 <- func_out_oppo$util %>%
  mutate(ct1 = str_count(s,"1"),
         ct2 = str_count(s,"2"),
         ct0 = str_count(s,"0"))

hist(func_out_oppo$util$u)
View(func_out_oppo$util)
##### Main

small <- trans %>%
  bind_cols(sp) %>%
  mutate(a = as.numeric(a) + 10) %>%
  filter(value == 0 | str_detect(s,"0") == FALSE) %>%
  mutate(`...10` = case_when(
    r == 10 | r == -10 | r == -1 ~ s,
    T ~ `...10`
  ))
  #select(s,a,r,sp=`...10`)

trans <- data.table(trans)
small <- data.table(small)


trans_new <- small %>%
  filter(ct1 <= ct2+1) %>%
  # filter(!str_detect(str_sub(s,1,3),"222") &
  #          !str_detect(str_sub(s,4,6),"222") &
  #          !str_detect(str_sub(s,7,9),"222") &
  #          !str_detect(paste0(str_sub(s,1,1),str_sub(s,5,5),str_sub(s,9,9)),"222") &
  #          !str_detect(paste0(str_sub(s,2,2),str_sub(s,5,5),str_sub(s,8,8)),"222") &
  #          !str_detect(paste0(str_sub(s,3,3),str_sub(s,5,5),str_sub(s,7,7)),"222") &
  #          !str_detect(paste0(str_sub(s,1,1),str_sub(s,4,4),str_sub(s,7,7)),"222") &
  #          !str_detect(paste0(str_sub(s,2,2),str_sub(s,5,5),str_sub(s,8,8)),"222") &
  #          !str_detect(paste0(str_sub(s,3,3),str_sub(s,6,6),str_sub(s,9,9)),"222")) %>%
  group_by(s,a,sp=`...10`) %>%
  summarise(num = n()) %>%
  group_by(s,a) %>%
  mutate(prob = num/sum(num)) %>%
  select(s,sp,a,prob)



rew <- small %>%
  group_by(s,a,r) %>%
  summarise(num = n()) %>%
  select(s,a,r)

policy <- tibble(s = unique(small$s), a = rep(1,length(unique(small$s))))

util_init <- policy %>%
  left_join(rew, by = "s") %>%
  select(s, u = r) %>%
  ungroup() %>%
  #mutate(s = factor(r)) %>%
  distinct() %>%
  #filter(`a.x` == `a.y`) %>%

  arrange(desc(s))


disc <- 0.9

rew <- data.table(rew)
trans_total <- data.table(trans_total)
policy <- data.table(policy)
util_init <- data.table(util_init)

#util_new <- policy_eval(5,rew,policy,trans_new,disc)

#policydf <- policy$policy$policy
View(func_out_oppo$util)
func_out <- value_iter(10,"main",util_init,rew,policy,trans_total,disc)
func_out1 <- value_iter(5,"base",func_out$util,rew,policy,trans_total,disc)


ggplot(func_out$policy) +
  geom_bar(aes(x = a))
write.csv(func_out$policy,"policy.csv")

hist(func_out$util$u)
View(func_out$util)
View(func_out$policy)


test2 <- func_out$util %>%
  left_join(func_out_oppo$util, by = "s")

test3 <- test2 %>%
  filter(u.x == 0 & u.y == 0)
test4 <- test2 %>%
  filter(u.x != 0 | u.y != 0)

test <- data.table(left_join(func_out$util,func_out$policy, by = "s"))

test[s == "000000000"]
test[s == "100000000"]
test[s == "100020000"]
test[s == "112020100"]
trans_total[s == "112020000"]


func_out1 <- value_iter(5,func_out$util,rew,func_out$policy,trans_new,disc)


for(i in 1:5){
  policy_new <- func_out$policy
  util_new <- func_out$util
  func_out <- value_iter(1,util_new,rew,policy_new,trans_new,disc)
}

View(func_out1$util)
View(func_out$policy)
ggplot(func_out1$policy) +
  geom_bar(aes(x = a))

policy_ttt <- df1 %>%
  select(s) %>%
  mutate(5)

str_sub("1234",-1)


small[`...10` == "020011202"]
small[s == "000011202"]
util_new[s == "020011202"]
util_new[s == "020011202"]
policy_new[s == "020011202"]
trans[s == "020011202"]
