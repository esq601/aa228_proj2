library(tidyverse)
library(data.table)
library(randomForest)
library(caret)
source("funcs.R")

large_full <- readr::read_csv("https://raw.githubusercontent.com/sisl/AA228-CS238-Student/master/project2/data/large.csv")

#large <- readr::read_csv("large1.csv")

known_r <- large_full %>%
  group_by(s,sp,a,r) %>%
  summarise(n()) %>%
  mutate(change = s-sp)

states <- unique(large_full$s)
actions <- unique(large_full$a)

# q <- expand_grid(states,actions) %>%
#   mutate(q = 0)
# 
# q_dt <- as.data.table(q)
# 
# q_dt[states == 25203 & actions == 3, q:=1]
q <- read.csv("q_lrg_new2.csv")
#  select(-X)

q_dt <- as.data.table(q)

#rand_test <- c(floor(runif(100,1,5)))

#readr::write_lines(rand_test, file = "large.policy")

state_space <- tibble(s = seq(1:50000)) %>%
  left_join(large_full, by = "s")


policy <- tibble(s = seq(1:500), a = rep(3,500))


trans <- large_full %>%
  group_by(s,a,sp) %>%
  summarise(num = n()) %>%
  group_by(s,a) %>%
  #mutate(prob = num/sum(num)) %>%
  select(s,sp,a)

rew <- large_full %>%
  group_by(s,a,r) %>%
  summarise(num = n()) %>%
  select(s,a,r)


util_init <- rew %>%
  left_join(policy, by = "s") %>%
  filter(`a.x` == `a.y`) %>%
  select(s, u = r)


disc <- 0.95


lrg_full <- large_full %>%
  left_join(trans, by = c("s","sp","a"))

lrg_full <- as.data.table(lrg_full)

q_learn <- function(q_dt,state,action,next_states,reward,disc,learn) {
  #print(paste0("action",action))
  r <- reward %>%
    filter(s == state, a == action)
  
  q_dt_curr <- q_dt[states == state & actions == action,q]
  
  r <- r$r
  
  #print(next_states)
  #print(paste("reward",r))
  len_next <- nrow(next_states)
  
  
  
  if(len_next > 0){
    max_q_next <- -100000
    
    #for(i in unique(next_states$s)) {
    
    # next_state_temp <- next_states[s == i]
    #print(next_state_temp)
    for(j in 1:nrow(next_states)){
      #print(i)
      next_temp <- next_states[j,]
      q_dt_next <- q_dt[states == next_temp$s & actions == next_temp$a,q] 
      #print(q_dt_next)
      
      #print(q_dt_next)
      if(q_dt_next > max_q_next) {
        r <- max(r)
        max_q_next <- q_dt_next
        
      }
      #}
      
    }
     # print(q_dt_curr)
     # print(learn)
     # print(r)
     # print(disc)
     # print(max_q_next)
    q_update <- q_dt_curr + learn*(r + disc* max_q_next - q_dt_curr)
    
    #print(paste("update",q_update))
    # print(paste0("update",q_update))
    q_dt[states == state & actions == action, q:=q_update]
    #(paste("next",next_val))
    
  }
}

large_full <- as.data.table(large_full)

state_15 <- q_dt %>%
  filter(q > 0)

search_group <- large_full %>%
  filter(sp %in% state_15$s)


for(i in 1:1000000) {
  if(i %% 500 == 0){
    print(i)
    print(q_dt %>%
            mutate(eval = ifelse(q==0,"Zero","Non-Zero")) %>%
            group_by(eval) %>%
            summarise(num = n()))
    write.csv(q_dt,file = "q_lrg_new2.csv")
  }
  
  state_sel <- sample(search_group$s,1)
  
  if(str_sub(state_sel,3,4) %in% c(01,20) & str_sub(state_sel,5,6) %in% c(01,20)) {
    for(i in 5:9){
      
      next_states <-large_full[s %in% state_sel & a == i]
      q_learn(q_dt,state_sel,i,next_states,rew,disc,0.2)
    }
    
    print("transition")
  } else {
    for(i in 1:4){
      next_states <-large_full[s %in% state_sel & a == i]
      q_learn(q_dt,state_sel,i,next_states,rew,disc,0.2)
    }
  }
  
  
}

simulate <- function(data,policy,reward,state,trans,h,eps,alpha){
  
  state_val <- state
  for (i in 1:h) {
    
    if(i %% 50 == 0) {
      print(paste("Step",i))
    }
    #print(state_val)
    rand_num <- runif(1)
    eps <- alpha*eps
    if(eps > rand_num) {
      #print("random")
      action_space <- data %>%
        filter(s==state_val) 
    } else {
      #print("greedy")
      action_space <- data %>%
        filter(s==state_val) %>%
        filter(r == max(r))
    }
    # policy_val <- policy %>%
    #   filter(s == state_val)
    # 
    # action <- policy_val$a
    
    #print(action_space)
    
    for(k in unique(action_space$a)) {
      
      #print(k)
      next_states <- data[s %in% action_space$sp & a == k]
      #print(next_states)
      
      try(
        q_learn(q_dt,state_val,k,next_states,reward,disc,0.2)
      )
    }
    
    action_space <- action_space %>%
      sample_n(1)
    
    action <- action_space$a
    
    
    #print(i)
    new_state <- data %>%
      #   ungroup() %>%
      #   distinct() %>%
      #   arrange(prob) %>%
      filter(a == action, s == state_val) %>%
      ungroup() %>%
      sample_n(1)
    #mutate(prob_dist = cumsum(prob),t_delta = abs(rand_num-prob_dist)) %>%
    #arrange(t_delta)
    
    #new_state <- new_state[1,]
    
    #q_new
    if(new_state$r > 75000){
      q_dt[states == state_val & actions == action, q:=new_state$r]
      break
    }
    
    #print(new_state)
    state_val <- new_state$sp
    #print(state_val)
  }
}



for(j in 1:2000){
  print(paste("Run",j))
  
  start_state <<- sample_n(large_full,1)$s
  
  print(start_state)
  #for(k in 1:5) {
  
  simulate(lrg_full,policy,rew,start_state,trans,5, alpha = .99, eps = .5)
  #}
  
  write.csv(q_dt,file = "q_lrg_new1.csv")
  
  print(q_dt %>%
          mutate(eval = ifelse(q==0,"Zero","Non-Zero")) %>%
          group_by(eval) %>%
          summarise(num = n()))
}




#write.csv(q_dt,file = "q_med.csv")



q_dt_test <- q_dt %>%
  group_by(states) %>%
  summarise(max_a = max(q), min_a = min(q)) %>%
  mutate(delta = max_a - min_a)


u_dt <- q_dt %>%
  group_by(states) %>%
  filter(q == max(q)) %>%
  sample_n(1)



policy_knn <- expand_grid(s=seq(1:312020),a=actions) %>%
  left_join(q_dt, by = c("s" = "states", "a" = "actions")) %>%
  mutate(l1 = str_sub(s,1,2),
         l2 = str_sub(s,3,4),
         l3 = str_sub(s,5,6))

# policy_knn <- tibble(s = seq(1:312020)) %>%
#   mutate(l1 = as.numeric(str_sub(s,1,2))) %>%
#   rowwise() %>%
#   mutate(a = ifelse(l1 == 15,sample(1:4,1),sample(5:9,1)))
# 
# summary(policy_knn)

policy_filled <- policy_knn %>%
  filter(is.na(q) == F)


# control <- trainControl(method='repeatedcv', 
#                         number=10, 
#                         repeats=3)
# 
# 
# mtry <- sqrt(ncol(policy_filled))
# tunegrid <- expand.grid(.mtry=mtry)
# 
# rf_default <- train(q~., 
#                     data=policy_filled, 
#                     method='rf',
#                     tuneGrid=tunegrid, 
#                     trControl=control)

model2 <- randomForest(policy_filled[-3],policy_filled[[3]], ntree = 500)
#model1 <- lm(q ~ s + a, policy_filled)




policy_unfilled <- policy_knn
  
states <- tibble(s = unique(policy_knn$s)) %>%
  filter(!(s %in% policy_filled$s)) %>%
  mutate(l1 = str_sub(s,1,2),
         l2 = str_sub(s,3,4),
         l3 = str_sub(s,5,6)) %>%
  rowwise() %>%
  mutate(a = as.numeric(case_when(
    l1 == 15 ~ sample(1:4,1),
    l2 %in% c(01,20) & l3 %in% c(01,20) & l1 == 20 ~ sample(5,1),
    l2 %in% c(01,20) & l3 %in% c(01,20) ~ sample(5:9,1),
    T ~ sample(1:4,1)
  )))



preds <- predict(model2, newdata = policy_unfilled[-3])

policy_unfilled$q_est <- preds

u_pred <- policy_unfilled %>%
  group_by(s) %>%
  filter(q_est == max(q_est)) %>%
  select(s,a) %>%
  distinct() #%>%
#mutate(a = 7)

policy_test <- u_dt %>%
  select(s=states, a =actions) %>%
  bind_rows(select(states,s,a)) %>%
  arrange(s)


readr::write_lines(policy_test$a, file = "large.policy")
