### Implementation & model exploration of parametric decision model based on prospect theory 
### discussed in "Dopaminergic Modulation of Decision Making and Subjective Well-Being" 
### (Rutledge et al., 2015)

## Aim: use simulation to find out how loss aversion (lambda) and risk aversion 
## (alpha_loss and alpha_gain) behave together

setwd("/home/caroline/Desktop/git/decision_neuroscience_modeling/")

library(reshape2)
library(rgl)
library(plyr)
library(dplyr)
library(tidyr)
library(knitr)
library(stringr)
library(readr)
library(ggplot2)

#### 1. Implement model:

### 1.1 Utility functions

# u_gamble = (0.5 * v_gain ^ alpha_gain) - 0.5 * lambda * (-v_loss) ^ alpha_loss

u_gamble = function(v_gain, v_loss, alpha_gain, alpha_loss, lambda){
  return((0.5 * v_gain ^ alpha_gain) - 0.5 * lambda * ((-1 * v_loss) ^ alpha_loss))
}

# u_certain = (v_certain ^ alpha_gain)               # if v_certain >= 0
# u_certain = - lambda * (- v_certain) ^ alpha_loss  # if v_certain < 0

u_certain = function(v_certain, alpha_gain, alpha_loss, lambda){
  if(v_certain >= 0){
    return(v_certain ^ alpha_gain)
  }
  else{ # i.e., if (v_certain < 0)
    return(-1 * lambda * ((-1 * v_certain) ^ alpha_loss))
  }
}

# Meaning of variables:

# u_gamble = utility of gamble option
# u_certain = utility of certain option
# v_gain = value of potential gain of gamble [0.48 ; 2.2] (IV)
# v_loss = value of potential loss of gamble [-0.48 ; -2.2] (IV)
# v_certain = value of certain option [0.3 ; 0.55], [-0.3 ; -0.55] (IV)
# alpha_gain = risk aversion in gain domain [0.3 ; 1.3] (model param)
# alpha_loss = risk aversion in loss domain [0.3 ; 1.3] (model param)
# lambda = loss aversion [0.5 ; 5] (model param)

### 1.2 Softmax function

# softmax function to determine the probability of selecting a gamble:
# P_gamble = 1 / (1 + e ^ (-mu* (U_gamble-U_certain) ) )

p_gamble = function(mu, u_gamble, u_certain){
  return(1 / (1 + exp(-1 * mu * (u_gamble - u_certain) ) ) )
}

# where mu = inverse temperature parameter (stochasticity) (model param)
# p_gamble: probability of selecting the gamble


#### 2. Simulate contexts: Construct parameter ranges

# simulated IVs:
v_certain_pos_range = seq(30, 55, 0.01)        # v_certain for gain trials 
                                               # (!!unit: pence; if using pounds, utility trend reverses 
                                               # due to exponentiation!)
#v_certain_pos_range = seq(0.3, 0.55, 1)  
v_certain_neg_range = seq(-55, -30, 1)         # v_loss for gain trials
#v_certain_neg_range = seq(-0.55, -0.3, 0.01)
multiplicative_factor = seq(1.6, 4, 0.01)      # multiplicative factor to construct v_gain & v_loss from v_certain

# free params:
alpha_gain_range = seq(0.3, 1.3, 0.2)    # 0.3 0.5 0.7 0.9 1.1 1.3
alpha_loss_range = seq(0.3, 1.3, 0.2)    # 0.3 0.5 0.7 0.9 1.1 1.3
lambda_range = seq(0.5, 5, 0.5)          # 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0


#### 3. Gain trial:

set.seed(1) # set seed for reproducibility

v_certain_pos = sample(v_certain_pos_range, 1)   # this is v_certain_pos, because gain trial
v_gain = sample(multiplicative_factor, 1) * v_certain_pos
v_loss = 0

## 3.1 u_gamble for gain trials

# Make u_gamble array
u_gamble_array = array(NA, dim=c(length(alpha_gain_range),length(alpha_loss_range), length(lambda_range)))

for (alpha_gain_index in seq_len(dim(u_gamble_array)[1])){
  alpha_gain_value = alpha_gain_range[alpha_gain_index]
  for (alpha_loss_index in seq_len(dim(u_gamble_array)[2])){
    alpha_loss_value = alpha_loss_range[alpha_loss_index]
    for (lambda_index in seq_len(dim(u_gamble_array)[3])){
      lambda_value = lambda_range[lambda_index]
      #print(lambda_value)
      u_gamble_array[alpha_gain_index,alpha_loss_index,lambda_index] = u_gamble(v_gain, v_loss, alpha_gain_value, alpha_loss_value, lambda_value)
    }
  }
}

u_gamble_array_melted=melt(u_gamble_array)
#points3d(u_gamble_array_melted$Var1,u_gamble_array_melted$Var2,u_gamble_array_melted$Var3)

u_gamble_array_melted = u_gamble_array_melted %>%
  mutate(
    alpha_gain = alpha_gain_range[Var1],
    alpha_loss = alpha_loss_range[Var2],
    lambda = lambda_range[Var3],
    utility_gamble = value
  )%>%
  select(alpha_gain, alpha_loss, lambda, utility_gamble)

ggplot(u_gamble_array_melted, aes(lambda, utility_gamble, color=lambda)) + 
  geom_point() +
  facet_grid(alpha_loss ~ alpha_gain, labeller = label_both) +
  ggtitle(
    "Utility of the gamble option in gain trials"
  )
ggsave("u_gamble_gain.png",width=10,height=10)

# Observation: 
# lambda (loss aversion) and alpha_loss (risk aversion for loss trials) have no impact on utility_gamble
# (this is the expected behavior b/c 2nd term in utility_gamble function falls away when v_loss=0.)
# utility_gamble increases as as alpha_gain (risk aversion for gain trials) increases.
# This is also expected, because authors (counterintuitively) defined alpha_gain such, that a **higher** alpha_gain
# value corresponds to **less risk aversion** whereas **lower** alpha_gain values corresponds to 
# **more risk aversion** (so we might as well call alpha_gain **riskseekingness** instead) 

## 3.2 u_certain for gain trials

# Make u_certain array
u_certain_array = array(NA, dim=c(length(alpha_gain_range),length(alpha_loss_range), length(lambda_range)))

for (alpha_gain_index in seq_len(dim(u_certain_array)[1])){
  alpha_gain_value = alpha_gain_range[alpha_gain_index]
  for (alpha_loss_index in seq_len(dim(u_certain_array)[2])){
    alpha_loss_value = alpha_loss_range[alpha_loss_index]
    for (lambda_index in seq_len(dim(u_certain_array)[3])){
      lambda_value = lambda_range[lambda_index]
      u_certain_array[alpha_gain_index,alpha_loss_index,lambda_index] = u_certain(v_certain_pos, alpha_gain_value, alpha_loss_value, lambda_value)
    }
  }
}

u_certain_array_melted=melt(u_certain_array)

u_certain_array_melted = u_certain_array_melted %>%
  mutate(
    alpha_gain = alpha_gain_range[Var1],
    alpha_loss = alpha_loss_range[Var2],
    lambda = lambda_range[Var3],
    utility_certain = value
  )%>%
  select(alpha_gain, alpha_loss, lambda, utility_certain)

ggplot(u_certain_array_melted, aes(lambda, utility_certain, color=lambda)) + 
  geom_point() +
  facet_grid(alpha_loss ~ alpha_gain, labeller = label_both) +
  ggtitle(
    "Utility of the certain option in gain trials"
  )
ggsave("u_certain_gain.png",width=10,height=10)

# Observation: 
# Just as in utility_gamble, lambda and alpha_loss have no impact on utility_certain
# Weirdly, utility_certain increases as as alpha_gain (risk aversion for gain trials) increases.
# The plot looks very similar to the utility_gamble_gain plot.
# This is unexpected, as I would expect that the utility of choosing the certain option would increase as
# subjects become more risk-averse (i.e., when alpha_gain is small). 

## 3.3 p_gamble for gain trials

mu = 1 # no info on paper about param range of mu... assumed to be 1 for now

# Make utility_gain array
p_gamble_gain_array = array(NA, dim=c(nrow(u_gamble_array_melted),nrow(u_certain_array_melted)))

for (u_gamble_index in seq_len(dim(p_gamble_gain_array)[1])){
  u_gamble_value = u_gamble_array_melted[u_gamble_index,4]
  for (u_certain_index in seq_len(dim(p_gamble_gain_array)[2])){
    u_certain_value = u_certain_array_melted[u_certain_index,4]
    p_gamble_gain_array[u_gamble_index,u_certain_index] = p_gamble(mu, u_gamble_value, u_certain_value)
  }
}

p_gamble_gain_array_melted=melt(p_gamble_gain_array)

p_gamble_gain_array_melted = p_gamble_gain_array_melted %>%
  mutate(
    u_gamble_gain = u_gamble_array_melted[Var1,4],
    u_certain_gain = u_certain_array_melted[Var2,4],
    p_gamble_gain = value
  )%>%
  select(u_gamble_gain, u_certain_gain, p_gamble_gain)

ggplot(p_gamble_gain_array_melted, aes(u_gamble_gain, p_gamble_gain, color=mu)) + 
  geom_point() +
  facet_grid(. ~ u_certain_gain, labeller = label_both) +
  ggtitle(
    "Probability of selecting the gamble in gain trials"
  )
ggsave("p_gamble_gain.png",width=20,height=10)

#### 4. Loss trial:

set.seed(1) # set seed for reproducibility

v_certain_neg = sample(v_certain_neg_range, 1)   # this is v_certain_neg, because loss trial
v_gain = 0
v_loss = sample(multiplicative_factor, 1) * v_certain_neg

## 4.1 u_gamble for loss trials

# Make u_gamble array
u_gamble_array = array(NA, dim=c(length(alpha_gain_range),length(alpha_loss_range), length(lambda_range)))

for (alpha_gain_index in seq_len(dim(u_gamble_array)[1])){
  alpha_gain_value = alpha_gain_range[alpha_gain_index]
  for (alpha_loss_index in seq_len(dim(u_gamble_array)[2])){
    alpha_loss_value = alpha_loss_range[alpha_loss_index]
    for (lambda_index in seq_len(dim(u_gamble_array)[3])){
      lambda_value = lambda_range[lambda_index]
      u_gamble_array[alpha_gain_index,alpha_loss_index,lambda_index] = u_gamble(v_gain, v_loss, alpha_gain_value, alpha_loss_value, lambda_value)
    }
  }
}

u_gamble_array_melted=melt(u_gamble_array)
#points3d(u_gamble_array_melted$Var1,u_gamble_array_melted$Var2,u_gamble_array_melted$Var3)

u_gamble_array_melted = u_gamble_array_melted %>%
  mutate(
    alpha_gain = alpha_gain_range[Var1],
    alpha_loss = alpha_loss_range[Var2],
    lambda = lambda_range[Var3],
    utility_gamble = value
  )%>%
  select(alpha_gain, alpha_loss, lambda, utility_gamble)

ggplot(u_gamble_array_melted, aes(lambda, utility_gamble, color=lambda)) + 
  geom_point() +
  facet_grid(alpha_loss ~ alpha_gain, labeller = label_both) +
  ggtitle(
    "Utility of the gamble option in loss trials"
  )
ggsave("u_gamble_loss.png",width=10,height=10)

# Observation: 
# alpha_gain (risk aversion for gain trials) has no impact on utility_gamble
# (this is the expected behavior b/c 1st term in utility_gamble function falls away when v_gain=0.)
# utility_gamble increases as as alpha_loss (risk aversion for loss trials) decreases.
# This is also expected, because authors (this time intuitively) defined alpha_loss such, that a **higher** alpha_loss
# value corresponds to **more risk aversion** whereas **lower** alpha_gain values corresponds to 
# **less risk aversion** (-> this is actually conceptualizing **risk aversion**)
# utility_gamble increases as as lambda (loss aversion) decreases.
# Is this intuitive/expected? I guess so: If you are more loss-averse, then you are more averse to experiencing
# the greater loss in gamble trials.

## 4.2 u_certain for loss trials:

# Make u_certain array
u_certain_array = array(NA, dim=c(length(alpha_gain_range),length(alpha_loss_range), length(lambda_range)))

for (alpha_gain_index in seq_len(dim(u_certain_array)[1])){
  alpha_gain_value = alpha_gain_range[alpha_gain_index]
  for (alpha_loss_index in seq_len(dim(u_certain_array)[2])){
    alpha_loss_value = alpha_loss_range[alpha_loss_index]
    for (lambda_index in seq_len(dim(u_certain_array)[3])){
      lambda_value = lambda_range[lambda_index]
      u_certain_array[alpha_gain_index,alpha_loss_index,lambda_index] = u_certain(v_certain_neg, alpha_gain_value, alpha_loss_value, lambda_value)
    }
  }
}

u_certain_array_melted=melt(u_certain_array)

u_certain_array_melted = u_certain_array_melted %>%
  mutate(
    alpha_gain = alpha_gain_range[Var1],
    alpha_loss = alpha_loss_range[Var2],
    lambda = lambda_range[Var3],
    utility_certain = value
  )%>%
  select(alpha_gain, alpha_loss, lambda, utility_certain)

ggplot(u_certain_array_melted, aes(lambda, utility_certain, color=lambda)) + 
  geom_point() +
  facet_grid(alpha_loss ~ alpha_gain, labeller = label_both) +
  ggtitle(
    "Utility of the certain option in loss trials"
  )
ggsave("u_certain_loss.png",width=10,height=10)

# Observation: 
# Expectedly, as in utility_gamble_loss, alpha_gain (risk aversion for gain trials) has no impact on utility_gamble
# utility_gamble increases as as alpha_loss (risk aversion for loss trials) decreases.
# utility_gamble increases as as lambda (loss aversion) decreases.

# Again, this is not expected. The plot looks very similar to the utility_gamble_loss plot.

# Maybe this is a matter of my intuition of risk?

## 4.3 p_gamble for loss trials

mu = 1 # no info on paper about param range of mu... assumed to be 1 for now

# Make utility_loss array
p_gamble_loss_array = array(NA, dim=c(nrow(u_gamble_array_melted),nrow(u_certain_array_melted)))

for (u_gamble_index in seq_len(dim(p_gamble_loss_array)[1])){
  u_gamble_value = u_gamble_array_melted[u_gamble_index,4]
  for (u_certain_index in seq_len(dim(p_gamble_loss_array)[2])){
    u_certain_value = u_certain_array_melted[u_certain_index,4]
    p_gamble_loss_array[u_gamble_index,u_certain_index] = p_gamble(mu, u_gamble_value, u_certain_value)
  }
}

p_gamble_loss_array_melted=melt(p_gamble_loss_array)

p_gamble_loss_array_melted = p_gamble_loss_array_melted %>%
  mutate(
    u_gamble_loss = u_gamble_array_melted[Var1,4],
    u_certain_loss = u_certain_array_melted[Var2,4],
    p_gamble_loss = value
  )%>%
  select(u_gamble_loss, u_certain_loss, p_gamble_loss)

ggplot(p_gamble_loss_array_melted, aes(u_gamble_loss, p_gamble_loss, color=mu)) + 
  geom_point() +
  facet_grid(. ~ u_certain_loss, labeller = label_both) +
  ggtitle(
    "Probability of selecting the gamble in loss trials"
  )
ggsave("p_gamble_loss.png",width=30,height=10, limitsize = FALSE)
