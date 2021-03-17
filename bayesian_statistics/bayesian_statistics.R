getwd()
setwd('/Users/Nick/Desktop')

################################################################
################################################################
# Read in and clean up data
################################################################
################################################################

data = read.csv('pulse2020_puf_16.csv')

# Narrow down to only colorado
data = data[data$EST_ST == 8,]
data = data[,c('TBIRTH_YEAR', 'EGENDER', 'RRACE', 'RHISPANIC',
               'EEDUC', 'MS', 'THHLD_NUMKID', 'WRKLOSS', 
               'DELAY', 'TENURE', 'INCOME', 'DOWN')]

data['Age'] = 2020 - data[,'TBIRTH_YEAR']
data = data[,colnames(data) != 'TBIRTH_YEAR']

for(column in colnames(data)){
  data[,column][data[,column] == -88] = NA
  data[,column][data[,column] == -99] = NA
  
  #if(column != 'Age'& column != 'THHLD_NUMKID'){
  #  data[,column] = factor(data[,column])
  #}
}

# Drop missing data
data = na.omit(data)

# Recode variables

# Set DOWN to 0 for 'not at all', otherwise 1
data$DOWN = (data$DOWN != 1) + 0

# Set RACE to 0 for white, 1 for non-white
data$RACE = (data$RRACE != 1 | data$RHISPANIC != 1) + 0 
data = data[,!(colnames(data) %in% c('RRACE', 'RHISPANIC'))]

# Set EDUC to 0 for not college degree, else 1
data$EDUC = (data$EEDUC > 4) + 0
data = data[,!(colnames(data) %in% c('EEDUC'))]

# Set MS to 0 for single, else 1
data$MS = (data$MS == 1) + 0

# Set RENT to 1 if rent or mortgage, otherwise 0
data$RENT = ((data$TENURE == 2) | (data$TENURE == 3)) + 0
data = data[,!(colnames(data) %in% c('TENURE'))]

# What should the income threshold be?
for (eq in 1:8){
  a = dim(data[(data$DOWN == 1 & data$INCOME == eq),])[1]/dim(data[(data$DOWN == 0 & data$INCOME == eq),])[1]
  print(eq) 
  print(a) 
  print('')
}

# It looks like cutoffs should be 3 - 4 ($50,000)
# and 5 - 6 ($100,000)
data$INCOME_2 = (data$INCOME > 3 & data$INCOME <= 5) + 0
data$INCOME_3 = (data$INCOME > 5) + 0

# Replace the income variable with one that has 3 levels that we can use
# For EDA. We'll drop before building the model
data$INCOME = 0
data = within(data, INCOME[INCOME_2 == 1] <- 1)
data = within(data, INCOME[INCOME_3 == 1] <- 2)

# Rename the gender column
colnames(data)[1] = 'GENDER'
data$GENDER = data$GENDER - 1 # Now female is 1

# Rename the number of children column
colnames(data)[3] = "NUM_KIDS"

# Recode delayed medical care to be 1 if delayed medical care, else 0
data$DELAY = data$DELAY - 1 
data$DELAY = (data$DELAY == 0) + 0

# Recode work loss to be 0 if no job loss
data$WRKLOSS = data$WRKLOSS - 1 # Now female is 1
data$WRKLOSS = (data$WRKLOSS == 0) + 0

################################################################
################################################################
# Data exploration
################################################################
################################################################

##################
# Balance of data
##################

# DOWN
dim(data[data$DOWN ==1,])/1992

# GENDER
dim(data[data$GENDER ==1,])/1992

# MS

# NUM_KIDS

# WRKLOSS

# DELAY

# INCOME

# Age

# RACE

# EDUC

# RENT



# Tables for categorical data
####################
# Gender (1 is male) 
####################
a = table(data$DOWN, data$GENDER)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('Male', 'Female'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topleft"),
        main = 'Frequency of feeling depressed over previous 7 days', ylab = 'Count')

# More females are depressed than not, men are about even

####################
# Marriage status (1 is married)
####################
a = table(data$DOWN, data$MS)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('Single', 'Married'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topleft"),
        main = 'Frequency of feeling depressed over previous 7 days', ylab = 'Count')

# Single people tend to be more depressed than not

####################
# Work loss **
####################
a = table(data$DOWN, data$WRKLOSS)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('No', 'Yes'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topright"), xlab = "Recent household job loss", cex.names = 1.5, cex.lab = 1.5)

# Among the people who have lost work, much more of them are depressed than not

####################
# Delayed medical care **
####################
a = table(data$DOWN, data$DELAY)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('No', 'Yes'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topright"), xlab = "Delayed medical care during last 4 weeks due to pandemic", cex.names = 1.5, cex.lab = 1.5)

# People with delayed medical care tend to be a lot more depresed than not, and visa-versa

####################
# Race
####################
a = table(data$DOWN, data$RACE)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('White', 'Non-white'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topright"),
        main = 'Frequency of feeling depressed over previous 7 days', ylab = 'Count')

# Race is close, but imbalanced because most respondents were white

####################
# Education
####################
a = table(data$DOWN, data$EDUC)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('Non-college educated', 'College educated'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topleft"),
        main = 'Frequency of feeling depressed over previous 7 days', ylab = 'Count')

# Non college educated tend to feel down more than not, but again it's imbalanced because
# most respondents were college educated

####################
# Rent/Mortgage
####################
a = table(data$DOWN, data$RENT)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('No rent or mortgage', 'Pay rent or mortgage'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topleft"),
        main = 'Frequency of feeling depressed over previous 7 days', ylab = 'Count')

# People who pay rent or mortgage tend to be more depressed than not and visa-versa

####################
# Income **
####################
a = table(data$DOWN, data$INCOME)
a/margin.table(a)
prop.table(a, 2)

barplot(a, beside = TRUE, names.arg = c('<$50,000', '$50,000-$100,000', '>$100,000'), col = c("darkblue", "red"),
        legend = c('Not at all', 'Several days or more'), args.legend = list(x = "topleft"), xlab = "Household income", cex.names = 1.5, cex.lab = 1.5)

####################
# Box plots for numeric variables
####################
####################
# Age **
####################
#boxplot(data$Age~data$DOWN, ylab = 'Age', xlab = 'Frequency of feeling depressed over previous 7 days',
#        names = c('Not at all', 'Several days or more'), col = c("darkblue", "red"))

boxplot(data$Age~data$DOWN, ylab = 'Age',
        names = c('Not at all', 'Several days or more'), col = c("darkblue", "red"), xlab = "", cex.names = 1.5, cex.lab = 1.5)

a = table(data$DOWN, data$Age)
plot(c(0, 1, 2, 3, 4, 5), prop.table(a, 2)[1,])
lmod = lm(prop.table(a, 2)[1,]~c(0, 1, 2, 3, 4, 5))
summary(lmod)

####################
# Number of children
####################
boxplot(data$NUM_KIDS~data$DOWN, ylab = 'Number of children', xlab = 'Frequency of feeling depressed over previous 7 days',
        names = c('Not at all', 'Several days or more'), col = c("darkblue", "red"))

a = table(data$DOWN, data$NUM_KIDS)
plot(c(0, 1, 2, 3, 4, 5), prop.table(a, 2)[1,])
lmod = lm(prop.table(a, 2)[1,]~c(0, 1, 2, 3, 4, 5))
summary(lmod)
################################################################
################################################################
# Backward selection with frequentist to remove some variables using rstanarm
################################################################
################################################################

####################
# Full model
####################
data = data[,!(colnames(data) %in% 'INCOME')]

glmmod = glm(data2$DOWN ~., data = data, family = binomial)  
summary(glmmod)  

# Remove race
data2 = data[,!(colnames(data) %in% c('RACE'))]
glmmod = glm(data2$DOWN ~., data = data2, family = binomial)  
summary(glmmod)  

# Remove Rent  
data2 = data2[,!(colnames(data2) %in% c('RENT'))]
glmmod = glm(data2$DOWN ~., data = data2, family = binomial)  
summary(glmmod) 

# Check with a chi-square test
chisq.test(data$DOWN, data$RENT) # 0.08595, so maybe it's explained by something else

# Remove education  
data2 = data2[,!(colnames(data2) %in% c('EDUC'))]
glmmod = glm(data2$DOWN ~., data = data2, family = binomial)  
summary(glmmod) 
  
# Now all p-values are significant at the 0.10 level and switch to Bayes

################################################################
################################################################
# now try fitting some models with stanarm
################################################################
################################################################
data = data2

# full model
glmod_logit = stan_glm(DOWN ~ ., data = data2,
                       family = binomial(link = "logit"),
                       prior = normal(0, sqrt(1000)),
                       prior_intercept = normal(0, sqrt(1000)),
                       iter = 10000, chains = 4, seed = 101)

summary(glmod_logit)

# Try fitting models without variables that have a small impact:
# GENDER, MS, NUM_KIDS, and Age

##############
# No gender
##############
glmod_logit_nogender = stan_glm(DOWN ~ .-GENDER, data = data2,
                       family = binomial(link = "logit"),
                       prior = normal(0, sqrt(1000)),
                       prior_intercept = normal(0, sqrt(1000)),
                       iter = 10000, chains = 4, seed = 101)

summary(glmod_logit_nogender)

##############
# No MS
##############
glmod_logit_noms = stan_glm(DOWN ~ .-MS, data = data2,
                                family = binomial(link = "logit"),
                                prior = normal(0, sqrt(1000)),
                                prior_intercept = normal(0, sqrt(1000)),
                                iter = 10000, chains = 4, seed = 101)

summary(glmod_logit_noms)

##############
# No NUM_KIDS
##############
glmod_logit_nokids = stan_glm(DOWN ~ .-NUM_KIDS, data = data2,
                            family = binomial(link = "logit"),
                            prior = normal(0, sqrt(1000)),
                            prior_intercept = normal(0, sqrt(1000)),
                            iter = 10000, chains = 4, seed = 101)

summary(glmod_logit_nokids)

##############
# No Age
##############
glmod_logit_noage = stan_glm(DOWN ~ .-Age, data = data2,
                              family = binomial(link = "logit"),
                              prior = normal(0, sqrt(1000)),
                              prior_intercept = normal(0, sqrt(1000)),
                              iter = 10000, chains = 4, seed = 101)

summary(glmod_logit_noage)

####### Compare WAIC and LOOIC
waic_full = waic(glmod_logit)
loo_full = loo(glmod_logit)
waic_nogender = waic(glmod_logit_nogender)
loo_nogender = loo(glmod_logit_nogender)
waic_noms = waic(glmod_logit_noms)
loo_noms = loo(glmod_logit_noms)
waic_nokids = waic(glmod_logit_nokids)
loo_nokids = loo(glmod_logit_nokids)
waic_noage = waic(glmod_logit_noage)
loo_noage = loo(glmod_logit_noage)

loo_compare(waic_full, waic_nogender, waic_noms, waic_nokids, waic_noage)
loo_compare(loo_full, loo_nogender, loo_noms, loo_nokids, loo_noage)

# Could remove gender, noms
glmod_logit_nogender_noms = stan_glm(DOWN ~ .-(GENDER + MS), data = data2,
                             family = binomial(link = "logit"),
                             prior = normal(0, sqrt(1000)),
                             prior_intercept = normal(0, sqrt(1000)),
                             iter = 10000, chains = 4, seed = 101)

# Could remove gender, kids
glmod_logit_nogender_nokids = stan_glm(DOWN ~ .-(GENDER + NUM_KIDS), data = data2,
                                     family = binomial(link = "logit"),
                                     prior = normal(0, sqrt(1000)),
                                     prior_intercept = normal(0, sqrt(1000)),
                                     iter = 10000, chains = 4, seed = 101)


# Could remove ms, kids
glmod_logit_noms_nokids = stan_glm(DOWN ~ .-(NUM_KIDS + MS), data = data2,
                                     family = binomial(link = "logit"),
                                     prior = normal(0, sqrt(1000)),
                                     prior_intercept = normal(0, sqrt(1000)),
                                     iter = 10000, chains = 4, seed = 101)

# Remove ms, gender, kids
glmod_logit_nogender_noms_nokids = stan_glm(DOWN ~ .-(GENDER + NUM_KIDS + MS), data = data2,
                                   family = binomial(link = "logit"),
                                   prior = normal(0, sqrt(1000)),
                                   prior_intercept = normal(0, sqrt(1000)),
                                   iter = 10000, chains = 4, seed = 101)

waic_nogender_noms = waic(glmod_logit_nogender_noms)
waic_nogender_nokids = waic(glmod_logit_nogender_nokids)
waic_noms_nokids = waic(glmod_logit_noms_nokids)
waic_nogender_noms_nokids = waic(glmod_logit_nogender_noms_nokids)

looic_nogender_noms = loo(glmod_logit_nogender_noms)
looic_nogender_nokids = loo(glmod_logit_nogender_nokids)
looic_noms_nokids = loo(glmod_logit_noms_nokids)
looic_nogender_noms_nokids = loo(glmod_logit_nogender_noms_nokids)

loo_compare(waic_full, waic_nogender_noms, waic_nogender_nokids, waic_noms_nokids)
loo_compare(loo_full, looic_nogender_noms, looic_nogender_nokids, looic_noms_nokids)

models = c('Full', 'Without GENDER', 'Without MS', 'Without NUM_KIDS', 'Without Age', 
           'Without GENDER or MS', 'Without GENDER or NUM_KIDS', 'Without MS or NUM_KIDS',
           'Without GENDER or MS or NUM_KIDS')

waics = c(waic_full$waic, waic_nogender$waic, waic_noms$waic, waic_nokids$waic, waic_noage$waic, waic_nogender_noms$waic,
          waic_nogender_nokids$waic, waic_noms_nokids$waic, waic_nogender_noms_nokids$waic)

loos = c(loo_full$looic, loo_nogender$looic, loo_noms$looic, loo_nokids$looic, loo_noage$looic, looic_nogender_noms$looic,
          looic_nogender_nokids$looic, looic_noms_nokids$looic, looic_nogender_noms_nokids$looic)

results = data.frame(models, waics, loos)

loo_compare(waic_full, waic_nogender, waic_noms, waic_nokids, waic_noage, waic_nogender_noms, waic_nogender_nokids, waic_noms_nokids, waic_nogender_noms_nokids)
loo_compare(loo_full, loo_nogender, loo_noms, loo_nokids, loo_noage, looic_nogender_noms, looic_nogender_nokids, looic_noms_nokids, looic_nogender_noms_nokids)

# Remove gender and ms from the model because they're within one standard deviation
colnames(data)
# Final model will have NUM_KIDS, WRKLOSS, DELAY, Age, INCOME_2, and INCOME_3
summary(glmod_logit_nogender_noms)

# Number of kids -> Less likely to be depressed (Maybe having more people in the household means less feeling of isolation)
# Loss of work -> More likely to be depressed
# Delayed medical care -> More likely to be depressed
# Younger -> More likely to be depressed
# Higher income -> Less likely to be depressed

################################################################
################################################################
# Try different priors and compare WAIC, LOOIC
################################################################
################################################################

glmod_logit_nogender_noms = stan_glm(DOWN ~ .-(GENDER + MS), data = data2,
                                     family = binomial(link = "logit"),
                                     prior = normal(0, sqrt(1000)),
                                     prior_intercept = normal(0, sqrt(1000)),
                                     iter = 10000, chains = 4, seed = 101)

glmod_probit_nogender_noms = stan_glm(DOWN ~ .-(GENDER + MS), data = data2,
                                     family = binomial(link = "probit"),
                                     prior = normal(0, sqrt(1000)),
                                     prior_intercept = normal(0, sqrt(1000)),
                                     iter = 10000, chains = 4, seed = 101, init_r = 0.5)

glmod_cloglog_nogender_noms = stan_glm(DOWN ~ .-(GENDER + MS), data = data2,
                                     family = binomial(link = "cloglog"),
                                     prior = normal(0, sqrt(1000)),
                                     prior_intercept = normal(0, sqrt(1000)),
                                     iter = 10000, chains = 4, seed = 101, init_r = 0.5)

waic_logit = waic(glmod_logit_nogender_noms_nokids)
waic_probit = waic(glmod_probit_nogender_noms_nokids)
waic_cloglog = waic(glmod_cloglog_nogender_noms_nokids)

looic_logit = loo(glmod_logit_nogender_noms)
looic_probit = loo(glmod_probit_nogender_noms)
looic_cloglog = loo(glmod_cloglog_nogender_noms)

################################################################
################################################################
# Fitting the model and checking covergence
################################################################
################################################################

stanmod = "
data {
  int<lower=1> n; // number of observations
  int y[n];       // indicator of senility
  vector[n] num_kids;
  vector[n] work_loss;
  vector[n] delayed_care;
  vector[n] age;
  vector[n] income_2;
  vector[n] income_3;
}
parameters {
  real beta0;
  real beta1; // num_kids
  real beta2; // work_loss
  real beta3; // delayed_care
  real beta4; // age
  real beta5; // income_2
  real beta6; // income_3
}
transformed parameters {
  vector[n] mu;           // mean of observations
  for(i in 1:n){
    mu[i] = beta0 + beta1*num_kids[i] + beta2*work_loss[i] + beta3*delayed_care[i] + beta4*age[i] + beta5*income_2[i] + beta6*income_3[i];
  }
}
model {
  // prior distributions
  beta0 ~ normal(0, sqrt(1000));
  beta1 ~ normal(0, sqrt(1000));
  beta2 ~ normal(0, sqrt(1000));
  beta3 ~ normal(0, sqrt(1000));
  beta4 ~ normal(0, sqrt(1000));
  beta5 ~ normal(0, sqrt(1000));
  beta6 ~ normal(0, sqrt(1000));
  // data distribution
  // data distribution
  for(i in 1:n){
    y[i] ~ bernoulli_logit(mu[i]);
  }
}
generated quantities {
  real exp_beta0;
  real exp_beta1;
  real exp_beta2;
  real exp_beta3;
  real exp_beta4;
  real exp_beta5;
  real exp_beta6;
  
  vector[n] log_lik;  // log likelihood of data
  
  exp_beta0 = exp(beta0);
  exp_beta1 = exp(beta1);
  exp_beta2 = exp(beta2);
  exp_beta3 = exp(beta3);
  exp_beta4 = exp(beta4);
  exp_beta5 = exp(beta5);
  exp_beta6 = exp(beta6);

  for (i in 1:n) log_lik[i] = bernoulli_logit_lpmf(y[i]|mu[i]);
}
"

stan_dat = list(n = length(data$DOWN), y = data$DOWN, num_kids = data$NUM_KIDS, work_loss = data$WRKLOSS,
                delayed_care = data$DELAY, age = data$Age, income_2 = data$INCOME_2,
                income_3 = data$INCOME_3)

stan_fit = stan(model_code = stanmod, data = stan_dat, iter = 10000)

coeffs = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6")

summary(stan_fit)$summary[coeffs, c("mean", "50%", "2.5%", "97.5%")]

# Gelman-Rubin statistic
summary(stan_fit)$summary[coeffs,"Rhat"]

# check convergence with trace plots
stan_trace(stan_fit, coeffs)

# Check the ACF of draws
stan_ac(stan_fit, coeffs)

log_lik = extract_log_lik(stan_fit, merge_chains = FALSE)
r_eff = exp(relative_eff(log_lik))

waic(log_lik) # want models with smaller waic
# 2470.2

loo(log_lik, r_eff = r_eff) # want models with a smaller looic
# 2470.2

#####################
# Posterior densities
#####################
stan_dens(stan_fit, par = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6"),
          separate_chains = TRUE)

################################################################
################################################################
# Posterior checks
################################################################
################################################################

library(bayesplot)

B = 1e4 #10000 samples

sumy = sum(data$DOWN)
n = length(data$DOWN)
y = data$DOWN
# sample from posterior distribution of theta
theta = rbeta(B, sumy + 1, n - sumy + 1)

# number of switches between 0 and 1 using rle
# (run length encoding) function
nswitch = function(x) length(rle(x)$values) - 1

# generate yrep
yrep = matrix(0, nrow = B, ncol = n)
for (i in 1:B) {
  # sample n bernoulli trials with success probability theta[i]
  yrep[i, ] = rbinom(n, size = 1, prob = theta[i])
}

nswitch_y = nswitch(y)
nswitch_yrep = apply(yrep, 1, nswitch)

# posterior predictive probability
(sum(nswitch_yrep >= nswitch_y) + 1)/(B + 1)

# histogram of number of switches for yrep
# compared to observed value
hist(nswitch_yrep, xlab = "switches", main = "")
abline(v = nswitch_y, col = "darkred")

ppc_stat(y, yrep, stat = nswitch)

# The null hypothesis that it's just random chance
# is random chance, there is no lack of fit

# Bayesian p-value is the "probability that the replicated
# data could be more extreme than the observed data

################################################################
################################################################
# Cross-validation on week 17
################################################################
################################################################

val = read.csv('pulse2020_puf_17.csv')

# Narrow down to only colorado
val = val[val$EST_ST == 8,]
val = val[,c('TBIRTH_YEAR', 'THHLD_NUMKID', 'WRKLOSS', 
               'DELAY', 'INCOME', 'DOWN')]

val['Age'] = 2020 - val[,'TBIRTH_YEAR']
val = val[,colnames(val) != 'TBIRTH_YEAR']

for(column in colnames(val)){
  val[,column][val[,column] == -88] = NA
  val[,column][val[,column] == -99] = NA
}

# Drop missing data
val = na.omit(val)

# Recode variables

# Set DOWN to 0 for 'not at all', otherwise 1
val$DOWN = (val$DOWN != 1) + 0

# It looks like cutoffs should be 3 - 4 ($50,000)
# and 5 - 6 ($100,000)
val$INCOME_2 = (val$INCOME > 3 & val$INCOME <= 5) + 0
val$INCOME_3 = (val$INCOME > 5) + 0

# Rename the number of children column
colnames(val)[1] = "NUM_KIDS"

# Recode delayed medical care to be 1 if delayed medical care, else 0
val$DELAY = val$DELAY - 1 
val$DELAY = (val$DELAY == 0) + 0

# Recode work loss to be 0 if no job loss
val$WRKLOSS = val$WRKLOSS - 1 # Now female is 1
val$WRKLOSS = (val$WRKLOSS == 0) + 0

val = val[,!(colnames(val) %in% 'INCOME')]

coeffs = c('beta0', 'beta1', 'beta2', 'beta3', 'beta4', 'beta5', 'beta6')
a = summary(stan_fit)$summary[coeffs, c("mean")]

summ = exp(a['beta0'] + a['beta1']*val$NUM_KIDS + a['beta2']*val$WRKLOSS + a['beta3']*val$DELAY + 
             a['beta4']*val$Age + a['beta5']*val$INCOME_2 + a['beta4']*val$INCOME_3) 

b = summ/(1 + summ)

r = c()
for(i in b){
  if(i > 0.5){
    r = c(r, 1) 
  }
  else{
    r = c(r, 0)
  }
}
sum(r == val$DOWN)/length(val$DOWN)

# 62% accurate on a validation set

# confusion matrix
confusionMatrix(as.factor(val$DOWN), as.factor(r))

################################################################
################################################################
# Interpreting coefficients
################################################################
################################################################

######## 
# Estimates for exp(beta_j) for the coefficients
########
summary(stan_fit)$summary[c("exp_beta0", "exp_beta1", "exp_beta2", "exp_beta3", 
                            "exp_beta4", "exp_beta5", "exp_beta6"), c("mean")]


# Default person

kids = 0
work_loss = 0
delayed_care = 0
age = 35
income2 = 1
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.512

# If that person had 3 kids

kids = 3
work_loss = 0
delayed_care = 0
age = 35
income2 = 1
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.376

# If the person lost work

kids = 0
work_loss = 1
delayed_care = 0
age = 35
income2 = 1
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.646

# If the person had delayed medical care

kids = 0
work_loss = 0
delayed_care = 1
age = 35
income2 = 1
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.781

# If the person was 18 years old

kids = 0
work_loss = 0
delayed_care = 0
age = 18
income2 = 1
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.604

# If the person was 60 years old

kids = 0
work_loss = 0
delayed_care = 0
age = 60
income2 = 1
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.377

# If the person made less than $50,000

kids = 0
work_loss = 0
delayed_care = 0
age = 35
income2 = 0
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.596

# If the person was over $100,000

kids = 0
work_loss = 0
delayed_care = 0
age = 35
income2 = 0
income3 = 1

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) # 0.413

# Worst case scenario: low income, lost work and delayed medical care

kids = 0
work_loss = 1
delayed_care = 1
age = 35
income2 = 0
income3 = 0

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) #0.897

# Best case scenario: high income, no lost work or delayed medical care, 3 kids, and 60 years old

kids = 3
work_loss = 0
delayed_care = 0
age = 60
income2 = 0
income3 = 1

summ = exp(a['beta0'] + a['beta1']*kids + a['beta2']*work_loss + a['beta3']*delayed_care + 
             a['beta4']*age + a['beta5']*income2 + a['beta6']*income3) 

print(summ/(1 + summ)) #0.189


