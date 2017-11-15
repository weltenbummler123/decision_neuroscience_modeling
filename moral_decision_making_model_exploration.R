### Implementation & model exploration of moral decision model
### discussed in "Dissociable Effects of Serotonin and Dopamine on the Valuation of 
### Harm in Moral Decision Making"
### (Crockett et al., 2015)

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

### 1.1 Function for determining subjective value of switching

# delta_v = (1-k) * L_m * delta_m - k * L_s * delta_s
# with 
# L_m = 1 if delta_m > 0 ; lambda o.w.
# L_s = 1 if delta_s < 0 ; lambda o.w.

delta_v = function(k, lambda, delta_m, delta_s){
  if(delta_m > 0){
    if(delta_s > 0){
      return((1-k) * delta_m - k * lambda * delta_s)
    }
    else{ # i.e., if(delta_s > 0)
      return((1-k) * delta_m - k * delta_s)
    }
  }
  else{ # i.e., if(delta_m < 0)
    if(delta_s > 0){
      return((1-k) * lambda * delta_m - k * lambda * delta_s)
    }
    else{ # i.e., if(delta_s < 0)
      return((1-k) * lambda * delta_m - k * delta_s)
    }
  }
}

# Meaning of variables:

# delta_v = subjective value of switching from default to alternative option (DV)
# delta_m = objective difference in money between default and alternative option [? ; ?] (IV)
# delta_s = objective difference in number of shocks between default and alternative option [? ; ?] (IV)
# L_m = loss aversion for the money (gain) domain (helper for model param)
# L_s = loss aversion for the shock (loss) domain (helper for model param)
# lambda = loss aversion [? ; ?] (model param)
# k = harm aversion [0 ; 1] (model param)

# Later: Fit two different models for trials with shock recipient: self/other,
# i.e. they split the k parameter into to parameter depending whether it described a self or other trial:
# k_other & k_self

### 1.2 Softmax function

# softmax function to determine the probability of selecting a gamble:
# P_choose_alternative = (1 / (1 + e ^ (-1 * gamma * delta_v) ) ) * (1 - 2 * epsilon) + epsilon

p_choose_alternative = function(gamma, epsilon, delta_v){
  return((1 / (1 + exp(-1 * gamma * delta_v) ) ) * (1 - 2 * epsilon) + epsilon)
}

# gamma = inverse temperature parameter (stochasticity): noise related to delta_v (model param)
# epsilon = irreducible noise unrelated to delta_v (model param)
# p_choose_alternative = probability of selecting the alternative option (DV)


#### 2. Simulate contexts: Construct parameter ranges

# simulated IVs:

# Cannot simulate IV ranges, because no info on paper (??)
# Instead, use specific values (from example, Fig.1)

delta_m_alternative_more_money = 5
delta_s_alternative_more_shocks = 3
delta_m_alternative_less_money = -5
delta_s_alternative_less_shocks = -3


# free params: lambda and k
lambda_range = seq(0, 1, 0.2)       # lambda unknown, use 0.8 (if = 1, no loss)
k_range = seq(0, 1.0, 0.2)          # k e [0,1]: 0 0.2 0.4 0.6 0.8 1.0


#### 3. Trial simulation 1: alternative_more_money && alternative_more_shocks:

# Make delta_v array
delta_v_array = array(NA, dim=c(length(lambda_range),length(k_range)))

for (lambda_index in seq_len(dim(delta_v_array)[1])){
  lambda_value = lambda_range[lambda_index]
  for (k_index in seq_len(dim(delta_v_array)[2])){
    k_value = k_range[k_index]
    delta_v_array[lambda_index,k_index] = delta_v(k_value, lambda_value, delta_m_alternative_more_money, delta_s_alternative_more_shocks)
  }
}

delta_v_array_melted=melt(delta_v_array)

delta_v_array_melted = delta_v_array_melted %>%
  mutate(
    lambda = lambda_range[Var1],
    k = k_range[Var2],
    delta_v = value
  )%>%
  select(lambda, k, delta_v)

ggplot(delta_v_array_melted, aes(k, delta_v, color=lambda)) + 
  geom_point() +
  #facet_grid(k ~ ., labeller = label_both) +
  ggtitle(
    expression(atop("Subjective value of switching to alternative option", paste("(in trial with more money & more shocks in alt)")))
  ) +
  xlab(
    "k (harm aversion)"
  ) +
  ylab(
    "delta_v (subjective value of switching)"
  )
ggsave("graphs/delta_v_more_m_more_s.png",width=5,height=5)

# Observation: 
# As harm aversion increases, alternative with more shocks becomes less attractive 
# -> utility of switching decreases
# loss aversion affects only shock domain (no loss in money, because more money in alt)
# i.e., if loss aversion high, shock loss is extremely painful for you
# loss aversion has only effect if harm aversion > 0, the more loss aversion for shocks, 
# the less likely are you to switch (because of the shock-"loss"), even if you are harm averse.


#### 4. Trial simulation 2: alternative_less_money && alternative_less_shocks:

# Make delta_v array
delta_v_array = array(NA, dim=c(length(lambda_range),length(k_range)))

for (lambda_index in seq_len(dim(delta_v_array)[1])){
  lambda_value = lambda_range[lambda_index]
  for (k_index in seq_len(dim(delta_v_array)[2])){
    k_value = k_range[k_index]
    delta_v_array[lambda_index,k_index] = delta_v(k_value, lambda_value, delta_m_alternative_less_money, delta_s_alternative_less_shocks)
  }
}

delta_v_array_melted=melt(delta_v_array)

delta_v_array_melted = delta_v_array_melted %>%
  mutate(
    lambda = lambda_range[Var1],
    k = k_range[Var2],
    delta_v = value
  )%>%
  select(lambda, k, delta_v)

ggplot(delta_v_array_melted, aes(k, delta_v, color=lambda)) + 
  geom_point() +
  #facet_grid(k ~ ., labeller = label_both) +
  ggtitle(
    expression(atop("Subjective value of switching to alternative option", paste("(in trial with less money & less shocks in alt)")))
  ) +
  xlab(
    "k (harm aversion)"
  ) +
  ylab(
    "delta_v (subjective value of switching)"
  )
ggsave("graphs/delta_v_less_m_less_s.png",width=5,height=5)

# Observation: 
# As harm aversion increases, alternative with less shocks becomes more attractive 
# -> utility of switching increases
# loss aversion affects only money domain (no loss in shocks, because less shocks in alt)
# i.e., if loss aversion high, money loss is extremely painful for you
# loss aversion has only effect if harm aversion < 1, the more loss aversion for money, 
# the less likely are you to switch (because of the money loss), even if you are harm averse.


