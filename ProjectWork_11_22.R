###################################################
# Import data
###################################################

setwd('/Users/Nick/Desktop/CU Classes Fall 2019/Applied Regression/Project/Data')

food_df = read.csv('FoodAvailabilityData.csv')
overweight_df = read.csv('Overweight_and_Obese_Adults__CDPHE_Community_Level_Estimates_Census_Tracts.csv')
obesity_df = read.csv('Obesity_in_Adults__CDPHE_Community_Level_Estimates_Census_Tracts.csv')

setwd('/Users/Nick/Downloads')
density_df = read.csv('Population_Density_Census_Tracts.csv')
density_df = density_df[,c('FIPS', 'Population_Density_PerLandSquareMile')]

food_df = food_df[food_df$State == 'Colorado',]

df = merge(food_df, obesity_df, by.x = 'CensusTract', by.y = 'Census_Tract_FIPS')

df = merge(df, density_df, by.x = 'CensusTract', by.y = 'FIPS')

df = df[,c('County', 'CensusTract', 'Urban', 'POP2010', 'PovertyRate', 'MedianFamilyIncome',
           'LA1and10', 'LAhalfand10', 'LA1and20', 'TractLOWI',
           'TractWhite', 'TractBlack', 'TractAsian', 'TractNHOPI',
           'TractAIAN', 'TractOMultir', 'TractHispanic', 'TractHUNV', 'TractSNAP',
           'OHU2010', 'Obese_Census_Tract_Estimate', 'Population_Density_PerLandSquareMile')]

###################################################
# Feature generation and EDA
###################################################

# Are there any unusual values?

summary(df)

# POP 2010:
# min = 0, which could be a problem, unless there are really 
# tracts with no population

# MedianFamilyIncome:
# min = 0, which could be a problem, unless again there are
# tracts with no population and therefore no income

# Obese_Census_Tract_Estimate 
# min is -1, so we should omit that row, since it's the predictor variable

# For TractWhite, TractBlack, TractAsian, TractNHOPI, 
# TractAIAN, TractOMultir, TractHispanic, TractHUNV, TractSNAP,
# OHU2010 all have min values of 0, so we should just make sure we're 
# not missing data for those things

df[df['POP2010'] == 0, c('County', 'CensusTract')] # (NOTE: THIS ISN'T A PROBLEM AFTER ADDING DENSITY DATAFRAME)

# This gives the census tracts with 0 population.
# Could replace with AdultPopulation_Age18_and_over,
# But that's also 0, so it's likely that there's just 0 population

# Are these the only rows with missing data?
# Let's drop those records and see
zero_pop_tracts = df[df['POP2010'] == 0, c('County', 'CensusTract')][['CensusTract']]

df = df[!(df$CensusTract %in% zero_pop_tracts), ]

summary(df)

# That got rid of a lot of problems.  We still have records
# where the MedianFamilyIncome is 0.
df[df$MedianFamilyIncome == 0,]

# There can't be a median family income of 0, so replace the missing median family incomes with the median
# family income for the county
zero_medIncome_tracts = df[df$MedianFamilyIncome == 0,][['CensusTract']]

for(tract in zero_medIncome_tracts){
  tract_county = df[df$CensusTract == tract, 'County']
  med_county_income = median(df[df$County == tract_county, 'MedianFamilyIncome'])
  df[(df$MedianFamilyIncome == 0 & df$CensusTract == tract), 'MedianFamilyIncome'] = med_county_income
}

summary(df)

# That seems to have fixed the problem for median family income

# We should look into the Poverty rate of 0
df[df$PovertyRate == 0,]

# The national poverty rate is $25,750 for a household of 4.
# Since all these tracts have a high median family income (>= 48,000),
# I think it's okay to leave it for now

summary(df)
# There are some tracts that also have 0 low income population
# These should at least be the same as the tracts with a 0 poverty rate,
# and should have a reasonable median family income.

df[df$TractLOWI == 0,][,c('MedianFamilyIncome', 'CensusTract')] 

df[(df$PovertyRate == 0  & df$TractLOWI == 0),]['CensusTract']

# They are the same, and the median family income id decent,
# so I think we can just leave it

summary(df)

# As far as races go, my concern would be that race just isn't recorded for
# some tracts, so let's see if there are any rows where the counts for ALL races is 0
nrow(df[(df$TractWhite == 0 & df$TractBlack == 0 &
       df$TractAsian == 0 & df$TractNHOPI == 0 &
       df$TractAIAN == 0 & df$TractOMultir == 0 &
       df$TractHispanic == 0),])

# There are no records, so I'll just really assume that if these are 0
# it's because there are 0 people of that race

summary(df)

# There are rows where TractHUNV is 0.  Are there really tracts
# where all tracts have a vehicle?
df[df$TractHUNV == 0,'PovertyRate']

# The poverty rate is high in some of these tracts, but it's plausible that everyone has a vehicle
# (or at least there's no way to know for sure that it's an error), so we'll leave it

summary(df)

# There are tracts where OHU2010 is 0.  That doesn't really make sense
df[df$OHU2010 == 0,]

# Let's find the mean number of households per population in the dataframe
temp_df = df[df$OHU2010 != 0,]
mean(temp_df$OHU2010/temp_df$POP2010)
# output is  0.4004779

# We'll replace the 0 values with 0.4004779* pop2010
df[df$OHU2010 == 0,'OHU2010'] = 0.4004779*df[df$OHU2010 == 0,'POP2010']

summary(df)

# The last thing to deal with is the Obese_Census_Tract_Estimate of -1
df[df$Obese_Census_Tract_Estimate == -1,]

# That's only the case for two counties, so we'll just drop them
obese_missing_tracts = df[df$Obese_Census_Tract_Estimate == -1, 'CensusTract']

df = df[!(df$CensusTract %in% obese_missing_tracts),]

summary(df)

######## I think by this point, the data is clean
######## Feature generation, then we'll look at univariate stats

# Percent population low income
df['PercentLowIncome'] = df$TractLOWI/df$POP2010*100

# Percent white
df['PercentWhite'] = df$TractWhite/df$POP2010*100

# Percent black
df['PercentBlack'] = df$TractBlack/df$POP2010*100

# Percent asian
df['PercentAsian'] = df$TractAsian/df$POP2010*100

# Percent native hawaiin or pacific islander
df['PercentNHOPI'] = df$TractNHOPI/df$POP2010*100

# Percent native american / alaskan native
df['PercentAIAN'] = df$TractAIAN/df$POP2010*100

# Percent multicultural or other
df['PercentOMultir'] = df$TractOMultir/df$POP2010*100

# Percent hispanic
df['PercentHispanic'] = df$TractHispanic/df$POP2010*100

# Percent housing units without a vehicle
df['PercentHUNV'] = df$TractHUNV/df$OHU2010*100

# Percent housing units with SNAP benefits
df['PercentSNAP'] = df$TractSNAP/df$OHU2010*100

# Percent obese
df['PercentObese'] = df$Obese_Census_Tract_Estimate

############ Now let's take a look at some bivariate statistics

boxplot(PercentObese~Urban, data = df, cex.lab = 1.5, cex.axis = 1.5)#, main = 'Proportion of Population that is Obese vs. Urban') #****
# Nothing too outstanding.  Rural areas look like they're a bit more obese

plot(PercentObese ~ PovertyRate, data = df, cex.lab = 1.5, cex.axis = 1.5)#main = 'Proportion of Population that is Obese vs. Poverty Rate') #****
# Hard to tell, maybe a sight positive association

plot(PercentObese ~ MedianFamilyIncome, data = df, cex.lab = 1.5, cex.axis = 1.5)# xlab = "Median Family Income (In Thousands of Dollars)", ylab = "Obesity Level", main = 'Obesity Level vs. Median Family Income') #****
# Looks like there's a negative association

boxplot(PercentObese ~ LA1and10, data = df,  cex.lab = 1.5, cex.axis = 1.5)#xlab = 'Lack of Food Access', ylab = 'Obesity Level', main = "Obesity Level vs. Lack of Access to Food") 
boxplot(PercentObese ~ LA1and20, data = df,  cex.lab = 1.5, cex.axis = 1.5)#main = 'Proportion of Population that is Obese vs. Limited Food Access') #****
boxplot(PercentObese ~ LAhalfand10, data = df, cex.lab = 1.5, cex.axis = 1.5)#
# It does look like low access has a higher percent of obesity

plot(PercentObese ~ PercentLowIncome, data = df, main = 'Proportion of Population that is Obese vs. Proportion of Low Income Population') #****
# Maybe a positive correlation

plot(df$PercentObese ~df$PercentWhite)
# Doesn't look like an association

plot(df$PercentObese ~df$PercentBlack)
# Doesn't look like an association

plot(df$PercentObese ~df$PercentAsian)
# Doesn't look like an association

plot(df$PercentObese ~df$PercentNHOPI)

plot(df$PercentObese ~df$PercentAIAN)

plot(df$PercentObese ~df$PercentOMultir)
# Maybe positive, not much

plot(PercentObese ~ PercentHispanic, data = df, cex.lab = 1.5, cex.axis = 1.5)# main = 'Proportion of Population that is Obese vs. \nProportion of Hispanic Population') #****
# Does look like a positive association

plot(df$PercentObese ~df$PercentHUNV)
# Maybe slightly positive

plot(PercentObese ~ PercentSNAP, data = df,  cex.lab = 1.5, cex.axis = 1.5)#xlab = 'Population SNAP benefits', ylab = 'Obesity Level', main = 'Obesity Levels vs. Proportion \nof Population with SNAP benefits') #****
# Looks positive

###################################################
##Collinearity
###################################################

# Start creating a model with all variables 
# But note that we don't need percent of all races 
# since those are linearly dependent, so we'll take out
# PercentAIAN 
lmod = lm(PercentObese ~ PercentSNAP + 
          PercentHUNV +
          PercentHispanic + 
          PercentOMultir + 
          #PercentNHOPI +
          PercentAsian + 
          PercentBlack +
          MedianFamilyIncome + 
          PercentWhite + 
          PercentLowIncome +
          LA1and20 + 
          LAhalfand10 + 
          LA1and10 +
          PovertyRate + 
          Urban, data = df)

# Start by looking at variance inflation factors
library(car)
vif(lmod)

# The variables with VIF's greater than 5 are
# PercentHispanic
# PercentOMultir
# PercentBlack
# PercentWhite
# LA1and20
# LA1and10

# This makes sense because these things are related
# So we'll eliminate some of the races and some of the food access flags

# For the food desert indicators, which one has the strongest
# association with PercentObese? We'll keep that one and throw
# out the others
lmod1 = lm(PercentObese ~ LA1and20, data = df)
summary(lmod1)
# R-squared is 0.0146

lmod1 = lm(PercentObese ~ LAhalfand10, data = df)
summary(lmod1)
# R-squared is 0.009568

lmod1 = lm(PercentObese ~ LA1and10, data = df)
summary(lmod1)
# R-squared is 0.02506

# So take out everything except hispanic, black, and white,
# and all the food desert indicators except LA1and20
lmod = lm(PercentObese ~ PercentSNAP + PercentHUNV +
            PercentHispanic +
            PercentBlack +
            PercentWhite + PercentLowIncome +
            MedianFamilyIncome +
            LA1and10 +
            PovertyRate + Urban, data = df)

vif(lmod)

# There's a couple that are higher than 5, so we should consider taking out PercetWhite
# or PercentHispanic.  Since we think race has an impact let's take PercentWhite out
lmod = lm(PercentObese ~ PercentSNAP + PercentHUNV +
            PercentHispanic +
            PercentBlack +
            PercentLowIncome +
            MedianFamilyIncome +
            LA1and10 +
            PovertyRate + Urban, data = df)

vif(lmod)

# PercentLowIncome is still high, so take it out
lmod = lm(PercentObese ~ PercentSNAP + 
            PercentHUNV +
            PercentHispanic +
            PercentBlack +
            #PercentLowIncome +
            MedianFamilyIncome +
            LA1and10 +
            PovertyRate + 
            Urban, data = df)

vif(lmod)
# That looks good per VIF's

cor(lmod$model)

library(perturb)
colldiag(lmod)
# The colldiag method (condition numbers) also shows there is no more collinearity

# Pairwise correlations
cor(lmod$model)
# There is high pairwise correlation between PercentLowIncome, PovertyRate, PercentSNAP, and MedianFamilyIncome
# which isn't surprising, since they're all related to income.  And even though the 
# VIF's ane condition numbers are good, let's remove a couple of them

lmod = lm(df$PercentObese~df$PercentLowIncome)
summary(lmod) # R^2 = 0.0481
lmod = lm(df$PercentObese~df$PovertyRate)
summary(lmod) # R^2 = 0.0153
lmod = lm(df$PercentObese~df$PercentSNAP)
summary(lmod) # R^2 = 0.113
lmod = lm(df$PercentObese~df$MedianFamilyIncome)
summary(lmod) # R^2 = 0.0981

# Let's take out PercentLowIncome and PovertyRate, but keep MedianFamilyIncome
lmod = lm(PercentObese ~ PercentSNAP + PercentHUNV +
            PercentHispanic +
            PercentBlack +
            #PercentLowIncome +
            MedianFamilyIncome +
            LA1and10 +
            #PovertyRate + 
            Urban, data = df)

vif(lmod)
colldiag(lmod)
cor(lmod$model)

###################################################
# Model selection
###################################################

df$MedianFamilyIncome = df$MedianFamilyIncome/1000
# Scale median income

############ 1. Backward elmination with a = 0.05
summary(lmod)

# Take out PercentBlack
lmod = lm(PercentObese ~ PercentSNAP + 
            PercentHUNV +
            PercentHispanic +
            PercentLowIncome +
            LA1and10 +
            #PovertyRate + 
            MedianFamilyIncome +
            Urban, data = df)

summary(lmod)
# That's it, everything else is significant at the 0.05 level,
# but MedianFamilyIncome variable is very small

############ 2. AIC
b = regsubsets(PercentObese ~ PercentSNAP + 
                 PercentHUNV +
                  PercentHispanic +
                  #PercentLowIncome +
                  PercentBlack + 
                  LA1and10 +
                  MedianFamilyIncome +
                  #PovertyRate + 
                 Urban, data = df)

rs = summary(b) # summarize model that minimizes RSS for each p

# calculate AIC of each model
n = nobs(lmod)
aic = n*log(rs$rss/n) + 2*(2:8)
aic2 = rs$bic + (2 - log(n)) * 2:8
# plot AIC vs p
plot(2:8, aic, xlab = "p", ylab = "AIC")

# It's hard to tell from the plot, so just look at the values
aic

# AIC recommends to keep the model with all variables

############ 3. BIC

library(car)
subsets(b, min.size = 4, statistic = "bic", legend = FALSE)

# BIC suggests the model without PercentBlack or MedianFamilyIncome

############ 4. Mallow's CP
subsets(b, min.size = 4, statistic = "cp", legend = FALSE)
abline(1, 1)

# The one that has the lowest Cp is the model with all variables

############ 5. Adjusted R^2
subsets(b, statistic = "adjr2", min.size = 4, legend = FALSE)

# Says to use the model with all variables

############ 6. Stepwise selection
lmod = lm(PercentObese ~ PercentSNAP + PercentHUNV +
            PercentHispanic +
            PercentBlack +
            MedianFamilyIncome +
            LA1and10 +
            #PovertyRate +
            Urban, data = df)

step(lmod) #with AIC

# Keeps everything

step(lmod, k = log(n)) # With BIC

# Takes out PercentBlack and MedianFamilyIncome

# So there are 3 models:
# The one with all the variables
# The one without PercentBlack
# The one without PercentBlack or MedianFamilyIncome

# Full model
lmod = lm(PercentObese ~ PercentSNAP + 
            PercentHUNV +
            PercentHispanic +
            PercentBlack +
            MedianFamilyIncome +
            LA1and10 +
            #PovertyRate +
            Urban, data = df)
summary(lmod)

# Without PercentBlack
lmod = lm(PercentObese ~ PercentSNAP + 
            PercentHUNV +
            PercentHispanic +
            #PercentBlack +
            MedianFamilyIncome +
            LA1and10 +
            #PovertyRate +
            Urban, data = df)
summary(lmod)

# Without PercentBlack or MedianFamilyIncome
lmod = lm(PercentObese ~ PercentSNAP + 
            PercentHUNV +
            PercentHispanic +
            #PercentBlack +
            #MedianFamilyIncome +
            LA1and10 +
            #PovertyRate +
            Urban, data = df)
summary(lmod)

###### Cross validation on these models

install.packages('caret')
library(caret)

# All variables
ols_cv = train(PercentObese ~ PercentSNAP + 
                 PercentHUNV +
                 PercentHispanic +
                 MedianFamilyIncome +
                 PercentBlack + 
                 LA1and10 +
                 #PovertyRate + 
                 Urban, data = df,
                  method = 'lm',
               trControl = trainControl(
                 method = "cv",
                 number = 10,
                 savePredictions = TRUE,
                 verboseIter = TRUE
               ))
ols_cv$results

# For the full model, the RMSE is 4.8274,
# MAE is 3.5825

# Without PercentBlack
ols_cv2 = train(PercentObese ~ PercentSNAP + 
                  PercentHUNV +
                 PercentHispanic +
                 MedianFamilyIncome +
                 LA1and10 +
                 #PovertyRate + 
                  Urban, data = df,
               method = 'lm',
               trControl = trainControl(
                 method = "cv",
                 number = 10,
                 savePredictions = TRUE,
                 verboseIter = TRUE
               ))
ols_cv2$results

# For the model without PercentBlack,
# the RMSE is 4.8326,
# MAE is 3.5821

# Without PercentBlack or MedianFamilyIncome
ols_cv3 = train(PercentObese ~ PercentSNAP + 
                  PercentHUNV +
                  PercentHispanic +
                  LA1and10 +
                  #PovertyRate + 
                  Urban, data = df,
                method = 'lm',
                trControl = trainControl(
                  method = "cv",
                  number = 10,
                  savePredictions = TRUE,
                  verboseIter = TRUE
                ))
ols_cv3$results

# For the model without PercentBlack or
# PercentLowIncome, the RMSE is 4.8331,
# and the MAE is 3.5922

# They're all about the same, so in an effort to keep the model parsimonious,
# we'll go without PercentBlack and MedianFamilyIncome (since MedianFamilyIncome and PercentSNAP and PovertyRate
# had high correlation anyway)

###################################################
# Model structure
###################################################

# Our model so far...
lmod = lm(PercentObese ~ PercentSNAP + 
            PercentHUNV +
            PercentHispanic +
            LA1and10 +
            #PovertyRate + 
            Urban, data = df)

# Component plus residual plots can suggest transformations 
crPlots(lmod)

# That looks like we should add a log transformation for PercentHispanic
lmod = lm(PercentObese ~ PercentSNAP + PercentHUNV +
            #PercentHispanic +
            LA1and10 +
            #PovertyRate + 
            Urban +
            log(PercentHispanic), data = df)

summary(lmod)

crPlots(lmod)

# PercentHispanic looks much better after that transformation

# It's hard to tell if we should apply transformations to PercentSNAP 
#, since there are some influential observations that are skewing the line.

# So let's try a lack of fit test
lmod = lm(PercentObese ~ PercentSNAP + I(PercentSNAP^2) +
            PercentHUNV +
            #PercentHispanic +
            LA1and10 +
            #PovertyRate + 
            Urban +
            log(PercentHispanic), data = df)

summary(lmod)

# Adding PercentSNAP^2 isn't significant at 0.05 level, so leave it out
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            #PercentHispanic +
            LA1and10 +
            #PovertyRate + 
            Urban +
            log(PercentHispanic), data = df)

summary(lmod)

crPlots(lmod)

# Everything looks good on the crPlots, and everything is significant

# The residual plots shouldn't have any systematic curves or patterns
residualPlots(lmod)

# No curves or patterns (except for posible), so I think we're good to go

# Tukey's test tells whether or not the response should be squared

lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

yhatall = predict(lmod, newdata = df)

lmod.tukeytest = lm(PercentObese ~ PercentSNAP +
                      PercentHUNV +
                      LA1and10 +
                      Urban +
                      log(PercentHispanic) +
                      I(yhatall^2), data = df)

summary(lmod.tukeytest)

# That test fails
# So this will be our model so far
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

summary(lmod) 
# Note that the R^2 is 0.26 so the model explains about 1/3
# of the observed variation

###################################################
# Check again for collinearity and model selection with transformations
###################################################

lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

######### Collinearity
vif(lmod) 

colldiag(lmod)

cor(lmod$model)
# There is a bit of collinearity between PercentSNAP and log(PercentHispanic),
# but everything else is low or certainly within reason. It won't be possible
# to get no collinearity between these variables

######### Model selection

# Everything is significant at the 0.05 level, so no need to do backward selection

# Try with AIC
b = regsubsets(PercentObese ~ PercentSNAP +
                 PercentHUNV +
                 LA1and10 +
                 Urban +
                 log(PercentHispanic), data = df)

rs = summary(b) # summarize model that minimizes RSS for each p

# calculate AIC of each model
n = nobs(lmod)
aic = n*log(rs$rss/n) + 2*(2:6)

# AIC says to keep the model with all the variables

# Try Mallow's CP
subsets(b, statistic = "cp", legend = FALSE)
abline(1, 1)

# Mallow's CP says that the modwl with all variables is good! 

# Try Adjusted R^2
subsets(b, statistic = "adjr2", legend = FALSE)

# Says to keep the model with all variables.

# In conclusion, I think our model is good.  
# We've checked model structure and applied transformations,
# addressed collinearity by amputating variables, and used
# model selection techniques to evaluate.

###################################################
# Influential observations
###################################################

# Leverage points
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

h = hatvalues(lmod)

row_names = df[,'CensusTract']
halfnorm(h, nlab = 2, labs = row_names, cex.lab = 1.5, cex.axis = 1.5, ylab = "leverage")

# It looks like maybe there's two leverage points
sort(h, decreasing = TRUE)[1:2]
df[(df$CensusTract %in% c(8041004402, 8031000800)),]

# They're the points with indices 373 and 647, so let's take a look 
# at them and see if anything stands out (could be an entry error, etc.)
df[(rownames(df) %in% c(373, 647)),]

# Nothing stands out or looks like a data entry error.

# Test for outliers
outlierTest(lmod) 

# No outliers at the Bonferroni 0.05 level

# Make an inflence plot
influencePlot(lmod)
# This suggests that the rows with index 647 and 373 MIGHT be influential, which we 
# already found.

# Test for influential observations using Cook's distance
cook <- cooks.distance(lmod)
halfnorm(cook, n = 3, labs = row_names, cex.lab = 1.5, cex.axis = 1.5, ylab = "Cook's distances")
sort(cook, decreasing = TRUE)[1:3]

# It looks like the possible influential points are the ones with indexes 373, 647, and 350
# We already looked at 647 and 373, so let's look them all again
df[(rownames(df) %in% c(350, 373, 647)),]

# One thing that stands out to me about 350 is that nearly half the population is obese. 
# Maybe part of the policy is to see what's going on in that tract specifically.

# Let's see what influence these points have by taking them out 1-by-1 and see what effect they have on 
# the model

# Remove point 1084
lmod2 = lm(PercentObese ~ PercentSNAP +
             PercentHUNV +
             LA1and10 +
             Urban +
             log(PercentHispanic), data = df, subset = (rownames(df) != 350))

compareCoefs(lmod, lmod2)

# Doesn't change the inference at all

# Remove point 377
lmod2 = lm(PercentObese ~ PercentSNAP +
             PercentHUNV +
             LA1and10 +
             Urban +
             log(PercentHispanic), data = df, subset = (rownames(df) != 373))

compareCoefs(lmod, lmod2)

# Doesn't change inference at all

# Remove point 652
lmod2 = lm(PercentObese ~ PercentSNAP +
             PercentHUNV +
             LA1and10 +
             Urban +
             log(PercentHispanic), data = df, subset = (rownames(df) != 647))

compareCoefs(lmod, lmod2)

# Again, doesn't change inference.

# In conclusion, I don't see any need to take any of these points out of the model

###################################################
# Checking error assumptions
###################################################

# To check that the mean of the errors is 0, we look at residuals vs. fitted values
# and residuals vs. values of the predictors to see that they're centered around 0

lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

# plot of residuals versus fitted values
plot(lmod, which = 3)
# Looks good

# plot of residuals versus predictors
residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# We definitely see horns in the plots of PercentSNAP and PercentHUNV,
# which means that our variance is not constant. We can try to remedy that by applying 
# transformations to the response or adding weights.

# Try weighing by the population
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), weights = df$POP2010, data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# That didn't seem to fix it.  What about weighting by housing units?
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), weights = df$OHU2010, data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# That's better, but still not great.

# Try log transformation on the response
lmod = lm(log(PercentObese) ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# Not good.  What about a square root transformation of the response?
lmod = lm(sqrt(PercentObese) ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# Better than log, but still not good.

# Try using the residuals from a model without weights as weights (as recommended by online research)
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), data = df)

lmod2 = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), weights = abs(1/lmod$residuals), data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# That's about the same, still not great.

# After thinking about it a bit, it makes sense that we might have less variability in more 
# densely populated areas, so try weighting by density
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            Urban +
            log(PercentHispanic), weights = df$Population_Density_PerLandSquareMile, data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# That seems to fix PercentSNAP and PercentHUNV, but now Urban doesn't have constant variance.
# So let's take Urban out.  The information we gain for that variable is probably mostly 
# accounted for by weighing for density

lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), weights = df$Population_Density_PerLandSquareMile, data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# There's almost a wedge shape on log(PercentHispanic) now.  Try a sqrt transormation instead
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            sqrt(PercentHispanic), weights = df$Population_Density_PerLandSquareMile, data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

# I'd say those look pretty good

# So let's leave the model where it is, now that our error assumptions are satisfied and re-check 
# collinearity, model structure, and model selection methods

lmod = lm(PercentObese^2 ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            sqrt(PercentHispanic), weights = df$Population_Density_PerLandSquareMile, data = df)

residualPlots(lmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)

######## Collinearity:
vif(lmod)
colldiag(lmod)
cor(lmod$model)

# Collinearity looks good

crPlots(lmod)

# Looks good aside from possible influential points on PercentSNAP

residualPlots(lmod)

# Maybe curving on the fitted values? Try Tukey's test again
lmod = lm(PercentObese ~ PercentSNAP +
            Urban +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), 
          weights = df$Population_Density_PerLandSquareMile, 
          data = df)

yhatall = predict(lmod, newdata = df)

lmod.tukeytest = lm(PercentObese ~ PercentSNAP +
                      
                      PercentHUNV +
                      LA1and10 +
                      log(PercentHispanic) +
                      I(yhatall^2), 
                   # weights = df$Population_Density_PerLandSquareMile, 
                    data = df)

summary(lmod.tukeytest)

# That is significant now.  So let's try undoing some of the things we did
# Start by keeping log(PercentHispanic)
lmod = lm(PercentObese ~ PercentSNAP +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), weights = df$Population_Density_PerLandSquareMile, data = df)

residualPlots(lmod)

# What about adding in the Urban variable again
lmod = lm(PercentObese ~ PercentSNAP +
            Urban +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), weights = df$Population_Density_PerLandSquareMile, data = df)

residualPlots(lmod)

# No, so the weights are causing other problems, so remove the weights
lmod = lm(PercentObese ~ PercentSNAP +
            Urban +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), data = df)

residualPlots(lmod)

# I think it's better to move ahead with this and note that some of our error assumptions may 
# be violated.  If I had more time, I would figure out how to incorporate weights without
# adding unaccounted for curvature to the model

############ NOTE: IN paper, need to mention that we SHOULD check for autocorrelation, 
# but we're not going to do that (because we didn't learn it)

# Make sure to mention that WE ASSUME that the errors are uncorrelated

###################################################
# Interpretation
###################################################
lmod = lm(PercentObese ~ PercentSNAP +
            Urban +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), data = df)


summary(lmod)

# The intercept is 15.8011
# PercentSNAP is .1209
# Urban is -2.3763
# PercentHUNV is -0.1563
# LA1and20 is 1.7211
# log(PercentHispanic) is 2.7430

# PercentSNAP
# When the percent of the population with SNAP benefits goes up, we expect that the Percent of the population
# that is obese will go up by 0.1209

# Urban
# Locations that are urban have an expected percent of obesity that's -2.3763 less than rural locations,
# while holding all other variables constant

# PercentHUNV
# When the percent of housing units without vehicles goes up by 1, we expect the percent of obesity 
# to go down -0.1563, while holding other variables constant

# LA1and20
# For housing units with low access, we expect the percent of obesity to be 1.7211 percent higher,
# while holding other things constant

# log(PercentHispanic)
# Increasing by 1%, while holding others constant, is associated with 
# APPROXIMATELY a 2.743/100 increase in the response variable, on average

# Note that the R^2 value seems appropriate since a plot of y vs yhat is approximately linear
plot(df$PercentObese~lmod$fitted)

###################
# Things to add
###################

# Effects plots (95% confidence bands of the effects of regressor on response)
# This could be useful for a poster
library(effects) # for Effect function
plot(Effect("PercentHUNV", lmod))
plot(Effect("log(PercentHispanic)", lmod))

# Create a scatterplot of Urban vs. the other variables
# where urban or not are different colors to see if value, slopes are the same
# If slopes change, consider including interactions.
# If not, use it as a justification why NOT to include interactions
# You did this in a homework at some point, so just find the code for that
sumary(lmod)

pch_type = c(19, 1)[factor(df$Urban)]
col_type = c(153, 4)[factor(df$Urban)]

pchs = c(19, 1)
coltypes = c(153, 4)

plot(PercentObese~PercentSNAP, data = df, col = col_type, pch = pch_type, cex.lab = 1.5, cex.axis = 1.5)# main = "Obesity Level vs. PercentSNAP")
legend(70, 45, legend = c('Urban', 'non-Urban'), pch = pchs, col = coltypes)
# Maybe? Worth trying...
plot(PercentObese~PercentHUNV, data = df, col = col_type, pch = pch_type, cex.lab = 1.5, cex.axis = 1.5)#, main = "Obesity Level vs. PercentHUNV")
legend(30, 45, legend = c('Urban', 'non-Urban'), pch = pchs, col = coltypes)
# Nothing there
plot(PercentObese~log(PercentHispanic), data = df, col = col_type, pch = pch_type, cex.lab = 1.5, cex.axis = 1.5)#, main = "Obesity Level vs. log(PercentHispanic")
legend(0.5, 45, legend = c('Urban', 'non-Urban'), pch = pchs, col = coltypes)
# Those look the same, so good to go

# What about LA1and10?

pch_type = c(19, 1)[factor(df$LA1and10)]
col_type = c(153, 2)[factor(df$LA1and10)]

pchs = c(19, 1)
coltypes = c(153, 2)

plot(PercentObese~PercentSNAP, data = df, col = col_type, pch = pch_type, cex.lab = 1.5, cex.axis = 1.5)#main = "Obesity Level vs. PercentSNAP")
legend(70, 45, legend = c('Urban', 'non-Urban'), pch = pchs, col = coltypes)
# No
plot(PercentObese~PercentHUNV, data = df, col = col_type, pch = pch_type, cex.lab = 1.5, cex.axis = 1.5)#main = "Obesity Level vs. PercentHUNV")
legend(30, 45, legend = c('Urban', 'non-Urban'), pch = pchs, col = coltypes)
# No
plot(PercentObese~log(PercentHispanic), data = df, col = col_type, pch = pch_type, cex.lab = 1.5, cex.axis = 1.5)#main = "Obesity Level vs. log(PercentHispanic)")
legend(0.5, 45, legend = c('Urban', 'non-Urban'), pch = pchs, col = coltypes)
# Those look the same, so good to go

# So let's try an interaction term for Urban and PercentSNAP
lmod = lm(PercentObese ~ PercentSNAP +
            Urban +
            PercentSNAP*Urban +
            PercentHUNV +
            LA1and10 +
            log(PercentHispanic), data = df)


summary(lmod)
# Makes the P-value for Urban really high

# What about residual plots?
residualPlots(lmod)

# The residual plots also look worse, and Tukey's test is significant, so leave it out.

# Note that we chose not to scale the regressors to make it easy to 
# interpret the coefficients



