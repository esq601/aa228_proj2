library(tidyverse)
library(data.table)
library(randomForest)
library(caret)
source("funcs.R")

medium_full <- readr::read_csv("https://raw.githubusercontent.com/sisl/AA228-CS238-Student/master/project2/data/medium.csv")



known_r <- medium_full %>%
  group_by(s,a,r) %>%
  summarise(n())

states <- unique(medium_full$s)
actions <- unique(medium_full$a)

q <- expand_grid(states,actions) %>%
  mutate(q = 0)

q_dt <- as.data.table(q)
# 
# q_dt[states == 25203 & actions == 3, q:=1]
#q <- read.csv("q_med.csv") %>%
#  select(-X)

#q_dt <- as.data.table(q)

#rand_test <- c(floor(runif(100,1,5)))

#readr::write_lines(rand_test, file = "medium.policy")

state_space <- tibble(s = seq(1:50000)) %>%
  left_join(medium_full, by = "s")


policy <- tibble(s = seq(1:50000), a = rep(3,50000))


trans <- medium_full %>%
  group_by(s,a,sp) %>%
  summarise(num = n()) %>%
  group_by(s,a) %>%
  #mutate(prob = num/sum(num)) %>%
  select(s,sp,a)

rew <- medium_full %>%
  group_by(s,a,r) %>%
  summarise(num = n()) %>%
  select(s,a,r)


util_init <- rew %>%
  left_join(policy, by = "s") %>%
  filter(`a.x` == `a.y`) %>%
  select(s, u = r)


disc <- 1


med_full <- medium_full %>%
  left_join(trans, by = c("s","sp","a"))

med_full <- as.data.table(med_full)

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
    
    q_dt[states == state & actions == action, q:=q_update]
    #(paste("next",next_val))
    
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
  
  start_state <<- sample_n(medium_full,1)$s
  
  print(start_state)
  #for(k in 1:5) {
  
    simulate(med_full,policy,rew,start_state,trans,50, alpha = .99, eps = .5)
  #}
  
  write.csv(q_dt,file = "q_med_new1.csv")
  
  print(q_dt %>%
          mutate(eval = ifelse(q==0,"Zero","Non-Zero")) %>%
          group_by(eval) %>%
          summarise(num = n()))
}



  
write.csv(q_dt,file = "q_med.csv")

for(i in 1:1000000) {
  if(i %% 500 == 0){
    print(i)
    print(q_dt %>%
            mutate(eval = ifelse(q==0,"Zero","Non-Zero")) %>%
            group_by(eval) %>%
            summarise(num = n()))
    write.csv(q_dt,file = "q_med_new2.csv")
  }
  state_sel <- sample(states,1)
  action_sel <- sample(actions,1)
  
  next_states <-med_full[s %in% state_sel & a == action_sel]
  q_learn(q_dt,state_sel,action_sel,next_states,rew,disc,0.2)
}



q_dt_test <- q_dt %>%
  group_by(states) %>%
  summarise(max_a = max(q), min_a = min(q)) %>%
  mutate(delta = max_a - min_a)


u_dt <- q_dt %>%
  group_by(states) %>%
  filter(q == max(q)) %>%
  sample_n(1)



policy_knn <- expand_grid(s=seq(1:50000),a=actions) %>%
  left_join(q_dt, by = c("s" = "states", "a" = "actions"))



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

model2 <- randomForest(policy_filled[-3],policy_filled[[3]], ntree = 100)
#model1 <- lm(q ~ s + a, policy_filled)




policy_unfilled <- policy_knn %>%
  filter(is.na(q) == TRUE)



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
  bind_rows(u_pred) %>%
  arrange(s)


readr::write_lines(policy_test$a, file = "medium.policy")
