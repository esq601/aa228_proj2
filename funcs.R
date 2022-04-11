lookahead <- function(util, trans, policy, disc, reward) {
  
  util_update <- util
  
  for(state in util$s) {
    #print(state)
    a <- policy %>%
      filter(s == state)
    a_val <- a[[2]]
    
    rew_new <- reward %>%
      ungroup() %>%
      filter(s == state, a == a_val)
    
    r_val <- rew_new[[3]]
    #print(r_val)
    
    sum_total <- 0
    
    trans_new <- trans %>%
      filter(s == state,a == a_val)
    
    #print(trans_new)
    #print(sum_total)
    for(state_new in trans_new$sp) {
      t <- trans %>%
        filter(s == state, sp == state_new, a == a_val)
      
      t_val = t$prob
      
      util_prev <- util_update %>%
        filter(s == state_new)
      
      util_p_val <- util_prev[[2]]
      
      sum_total <- sum_total + (util_p_val * t_val) 
    }
    
    util_new <- r_val + disc * sum_total
    
    #$print(util_new)
    
    util_update <- util_update %>%
      mutate(u = ifelse(s == state, util_new, u))
    
  }
  
  #print(util_update)
  util_update %>%
    mutate_at(vars(u), ~replace(., is.na(.), 0))
  
}

policy_eval <- function(iters, reward, policy, trans, disc) {
  
  util_init <- reward %>%
    left_join(policy, by = "s") %>%
    filter(`a.x` == `a.y`) %>%
    select(s, u = r)
  
  util <- util_init
  
  for(i in seq(1:iters)) {
    print(i)
    
    util <- lookahead(util,trans,policy,disc,rew)
    
  }
  
  util
}

bellman <- function(state,util_bell, trans, policy_bell, disc, reward) {
  
  #util_update <- util
  u <- util_bell[s==state]
  t <- trans[s == state]
  
  p <- policy_bell[s == state]
  
  u_curr <- u$u
  max_u_temp <- -Inf
  #print(t)
  a_new <- p$a
  
  #print(state)
  
  for(actions in unique(t$a[t$a < 20])) {
    #print(actions)
    rew_new <- reward[s == state & a == actions]
    
    r_val <- rew_new[[3]]
    # print(r_val)
    
    sum_total <- 0
    
    trans_new <- trans[s == state & a == actions]
    #print(trans_new)
    
    for(state_new in trans_new$sp) {
      #print(state_new)

      t_prob <- trans[s == state & sp == state_new & a == actions]
      
      
      t_val <- t_prob$prob
      #print(t_val)
      # print(util_update)
      util_prev <- util_bell[s == state_new]
      #print(util_prev)
      # if(util_prev[[2]] > 0 & util_prev[[2]] < 10){
      #   print(paste("prev",util_prev))
      # }
      
      util_p_val <- util_prev[[2]]
      
      sum_total <- sum_total + (util_p_val * t_val) 
      
      #print(paste("sum",sum_total))
    }
    
    util_new <- r_val + disc * sum_total
    
    # if(util_prev[[2]] == 9.5){
    #   print(paste("new",util_new))
    # }
    #$print(util_new)
    #print(util_new)
    if(length(util_new) > 0 ) {
      if(util_new > max_u_temp) {
        #print(paste("new action:",actions))
        a_new <- actions
        max_u_temp <- util_new
      } 
    }
    
  }
  
  if(max_u_temp > u_curr) {
    new_a <- a_new
    new_u <- max_u_temp
  } else {
    
    #print(sample(unique(t$a)))
    new_a <- unique(t$a)[1]
    new_u <- u_curr
  }
  
  #print(paste0("sent action:",new_a))
   #print(max_a)
  # print("^ util new")
  return(list(a = new_a, u = new_u))
  
}

rev_bellman <- function(state,util_bell, trans, policy_bell, disc, reward) {
  
  #util_update <- util
  u <- util_bell[s==state]
  t <- trans[s == state]
  
  p <- policy_bell[s == state]
  
  u_curr <- u$u
  max_u_temp <- Inf
  #print(t)
  a_new <- p$a
  
  #print(state)
  
  for(actions in unique(t$a[t$a > 20])) {
    #print(actions)
    rew_new <- reward[s == state & a == actions]
    
    r_val <- rew_new[[3]]
    # print(r_val)
    
    sum_total <- 0
    
    trans_new <- trans[s == state & a == actions]
    #print(trans_new)
    
    for(state_new in trans_new$sp) {
      #print(state_new)
      
      t_prob <- trans[s == state & sp == state_new & a == actions]
      
      
      t_val <- t_prob$prob
      #print(t_val)
      # print(util_update)
      util_prev <- util_bell[s == state_new]
      #print(util_prev)
      # if(util_prev[[2]] > 0 & util_prev[[2]] < 10){
      #   print(paste("prev",util_prev))
      # }
      
      util_p_val <- util_prev[[2]]
      
      sum_total <- sum_total + (util_p_val * t_val) 
      
      #print(paste("sum",sum_total))
    }
    
    util_new <- r_val + disc * sum_total
    
    # if(util_prev[[2]] == 9.5){
    #   print(paste("new",util_new))
    # }
    #$print(util_new)
    #print(util_new)
    if(length(util_new) > 0 ) {
      if(util_new < max_u_temp) {
        #print(paste("new action:",actions))
        a_new <- actions
        max_u_temp <- util_new
      } 
    }
    
  }
  
  if(max_u_temp < u_curr) {
    new_a <- a_new
    new_u <- max_u_temp
  } else {
    
    #print(sample(unique(t$a)))
    new_a <- unique(t$a)[1]
    new_u <- u_curr
  }
  
  #print(paste0("sent action:",new_a))
  #print(max_a)
  # print("^ util new")
  return(list(a = new_a, u = new_u))
  
}


base_bellman <- function(state,util_bell, trans, policy_bell, disc, reward) {
  
  #util_update <- util
  u <- util_bell[s==state]
  t <- trans[s == state]
  
  p <- policy_bell[s == state]
  
  u_curr <- u$u
  max_u_temp <- Inf
  #print(t)
  a_new <- p$a
  
  #print(state)
  
  for(actions in unique(t$a)) {
    #print(actions)
    rew_new <- reward[s == state & a == actions]
    
    r_val <- rew_new[[3]]
    # print(r_val)
    
    sum_total <- 0
    
    trans_new <- trans[s == state & a == actions]
    #print(trans_new)
    
    for(state_new in trans_new$sp) {
      #print(state_new)
      
      t_prob <- trans[s == state & sp == state_new & a == actions]
      
      
      t_val <- t_prob$prob
      #print(t_val)
      # print(util_update)
      util_prev <- util_bell[s == state_new]
      #print(util_prev)
      # if(util_prev[[2]] > 0 & util_prev[[2]] < 10){
      #   print(paste("prev",util_prev))
      # }
      
      util_p_val <- util_prev[[2]]
      
      sum_total <- sum_total + (util_p_val * t_val) 
      
      #print(paste("sum",sum_total))
    }
    
    util_new <- r_val + disc * sum_total
    
    # if(util_prev[[2]] == 9.5){
    #   print(paste("new",util_new))
    # }
    #$print(util_new)
    #print(util_new)
    if(length(util_new) > 0 ) {
      if(util_new > max_u_temp) {
        #print(paste("new action:",actions))
        a_new <- actions
        max_u_temp <- util_new
      } 
    }
    
  }
  
  if(max_u_temp < u_curr) {
    new_a <- a_new
    new_u <- max_u_temp
  } else {
    
    #print(sample(unique(t$a)))
    new_a <- unique(t$a)[1]
    new_u <- u_curr
  }
  
  #print(paste0("sent action:",new_a))
  #print(max_a)
  # print("^ util new")
  return(list(a = new_a, u = new_u))
  
}


# backup <- function(util, trans, policy, disc, reward, state) {
#   max_util <- 0
#   
#   t <- trans %>%
#     filter(s == state)
#   
#   pol <- policy %>%
#     filter(s == state)
#   
#   for(actions in unique(t$a)) {
#     util <- lookahead(filter(util,s==state),filter(trans,a==actions),
#                       policy,disc,reward) %>%
#       ungroup()
#     print(paste(actions,util))
#     if(util$u[1] > max_util){
#       #print(actions)
#       max_util <- util
#       a_best <- actions
#     } else {
#       a_best <- pol$a
#     }
#   }
#   
#   a_best
# }


value_iter <- function(iters,side =c("main","opp","base"), util_iter, reward, policy_iter, trans, disc) {
  
  #policy_update <- policy
  
  
  for(i in seq(1:iters)) {
    print(i)
    #util <- policy_eval(10, reward, policy_update, trans, disc)
    time_iter <- Sys.time()
    
    print(time_iter)
    
    for(state in policy_iter$s){
      #state=27
      #print(util_iter[s == state])
      if(side == "main"){
        action <- bellman(state, util_iter, trans, policy_iter, disc, reward)
      } else if (side == "opp") {
        #print('opp')
        action <- rev_bellman(state, util_iter, trans, policy_iter, disc, reward)
      } else {
        action <- base_bellman(state, util_iter, trans, policy_iter, disc, reward)
      }
      
      
     # print(action)
      policy_iter[s == state, a:= action$a]
      util_iter[s==state, u:= action$u]
      
      #print(policy_iter[s == state])
      # <- policy_update %>%
      #   mutate(a = ifelse(s==state,action,a))
    }
    
    
    
  }
  
  return(list(policy = policy_iter, util = util_iter))
  
}
