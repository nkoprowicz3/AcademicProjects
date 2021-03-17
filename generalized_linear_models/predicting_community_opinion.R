#########################################################################
#*************************** CODE FOR FINAL PROJECT*********************#
#########################################################################

# Load the data

d = read.table("http://inside.mines.edu/~wnavidi/math436536/projects/Hamilton.txt", header = TRUE)
attach(d)

##################################
# Let's just investigate the data!
##################################


# Check for association between years lived in town and open/close

tapply(d$lived, d$school, mean) 

# It looks like people who have lived in the town longet want to keep the school open



# Check for association between education and open/close

tapply(d$educ, d$school, mean)

# There doesn't appear to be a difference


# Check for an association between gender and open/close

table(d$gender, d$school)

# It looks like the females are evenly split, but the men preferred
# to keep the school open



# Check for an association between kids and school

table(d$kids, d$school)

# People who have kids are evenly split, but people who don't have kids prefer
# to keep the school open



# Check for an association between hsc and school

table(d$hsc, d$school)

# People who went to the hsc want to close the school, but those who didn't prefer to 
# keep the school open



# check for an association between contam and school

table(d$contam, d$school)

# If people believed that their own property had been contaminated, they preferred
# to close, but if people didn't believe their property had been contaminated, they 
# wanted to keep it open.



###############################################
# Let's fit a multinomial model with all the variables:
# Note that although 'lived' and 'educ' are discrete, we cannot consider
# them as factor variables because we do not have enough data to fit that many coefficients

library(nnet)
out = multinom(d$school~d$gender + d$lived + d$kids + d$educ + d$hsc + d$contam)
summary(out)

###############################################
# Let's use AIC to eliminate variables from the model
outa = step(out)

# kids is the only variable that's eliminated

# Let's look at the fit of the minimum AIC model:
out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam)
summary(out)

###############################################
# Let's compute P-values for the coefficients:
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# all the coefficients seem to be significant, even with the full model, although 
# kids does have the highest P-value


#*******************************************************************************************#
#*******************************************************************************************#

# Let's fit each variable, one at a time, and compute P-values

out = multinom(d$school ~ d$gender)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# gender appears to be significant

out = multinom(d$school ~ d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# years lived is very significant

out = multinom(d$school ~ d$kids)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# kids seems to be significant

out = multinom(d$school ~ d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# education DOES NOT appear to be significant

out = multinom(d$school ~ d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# hsc is very significant

out = multinom(d$school ~ d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

# contam appears to be very significant

#*******************************************************************************************#
#*******************************************************************************************#

# It's interesting that kids was kicked out of the model when using AIC and
# education was kept in, although this is not the case when using P-values.
# This is probably something that can be explored with the deviance

# Let's see what happens when we start with the model without kids and then 
# see what happens to the deviance when we put kids back in the model

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam)
summary(out)

# The residual deviance is 146.5677

# Now, let's look at the model with kids back in it

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$kids)
summary(out)

# The residual deviance is 146.5421

# The decrease in deviance is: 0.0256 with 1 degree of freedom
146.5677 - 146.5421

# The P-value is:
1 - pchisq(0.0256, 1)

# So there is no justification for adding kids back into the model.

# Now, let's consider the model without kids or education and see what happens
# when we put education back in the model

out = multinom(d$school~d$gender + d$lived + d$hsc + d$contam)
summary(out)

# The residual deviance is 150.1884

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam)
summary(out)

# The residual deviance is 146.5677

# The decrease in the deviance is: 3.6207
150.1884 - 146.5677

# The P-value is:
1 - pchisq(3.6207, 1)

# The P-value is just greater than 0.05. We'll keep education in the model.

# (Note that when we performed this test, we had already removed kids from the 
# model, but even if we had left kids in the model, the P-value is not very different.)

out = multinom(d$school~d$gender + d$lived + d$hsc + d$contam + d$kids)
summary(out)

# The residual deviance is 150.147

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$kids)
summary(out)

# The residual deviance is 146.5421

# The decrease in the deviance is: 3.6207
150.147 - 146.5421

# The P-value is:
1 - pchisq(3.6049, 1)

# The P-value is still just greater than 0.05. We'll keep education in the model.

#*******************************************************************************************#
#*******************************************************************************************#

# It's probably a good idea to ask Navidi about why the P-value for education
# was so high, when it really looks like it should be kept in the model

# So it seems like the full model has everything except kids


# Is education correlated with something else?

plot(d$lived, d$educ) # doesn't appear to be a relationship

tapply(d$educ, d$gender, mean) # doesn't appear to be a relationship

tapply(d$educ, d$kids, mean) # doesn't appear to be a relationship

tapply(d$educ, d$hsc, mean) # doesn't appear to be a relationship

tapply(d$educ, d$contam, mean) # doesn't appear to be a relationship

#*******************************************************************************************#
#*******************************************************************************************#


# Let's try fitting a model by adding variables one at a time

out = multinom(d$school ~ d$gender)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# gender appears to be significant, let's add another vriable and see what happens

out = multinom(d$school ~ d$gender + d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# the p-value for gender went up, but still below 0.25.  The P-value for lived is very low

out = multinom(d$school ~ d$gender + d$lived + d$kids)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# The P-value for kids is high, greater than 0.25, let's remove it from the model and add something else

out = multinom(d$school ~ d$gender + d$lived + d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# All P-values are less than 0.25

out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# All P-values get very small when hsc is addes to the model -> Apparently there is a strong association with HSC

out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# All P-values are significant.  Education should be kept in the model

# Now, try interactions:
out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# Keep gender*lived

out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# don't add gender*educ
out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + gender*educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# don't add it

out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# don't add it

out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

# don't add it

out = multinom(d$school ~ d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2 # P = 0.05
p

#*******************************************************************************************#
#*******************************************************************************************#

# Now let's try adding interactions to the model

install.packages('caret')
require(caret)

library(nnet)
out = multinom(d$school~d$gender + d$lived + d$kids + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$kids + d$gender*d$educ + d$gender*d$hsc+
                 d$gender*d$contam + d$lived*d$kids + d$lived*d$educ + d$lived*d$hsc + d$lived*d$contam + d$kids*d$educ + d$kids*d$hsc + d$kids*d$contam + 
                 d$educ*d$hsc + d$educ*d$contam + d$hsc*d$contam)
summary(out)

###############################################
# Let's use AIC to eliminate variables from the model
outa = step(out)


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$educ + d$gender*d$hsc+
                 d$gender*d$contam + d$lived*d$educ + d$lived*d$hsc + d$lived*d$contam + d$kids*d$hsc + 
                 d$educ*d$hsc + d$educ*d$contam + d$hsc*d$contam)

outa = step(out)

summary(outa)


###############################################
# Let's try adding interactions one at a time


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 4.577331e-02 


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 5.696440e-01 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
#  0.7199851814 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 9.351619e-01 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 2.588432e-02

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.3522227366 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$kids*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 8.265897e-02 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.736400109 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$kids*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 9.946393e-02 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 1.293124e-01 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$lived*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.070647901 


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 5.148044e-01 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$educ*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.321255320 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$educ*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 8.996138e-01 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$hsc*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 6.385609e-01 


################## The Interaction with the smallest P-value is gender*kids.  Let's add that one and try the others.

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 1.375130e-01

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 4.567487e-02 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 6.602128e-02

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 8.044017e-02

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$lived*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 6.131900e-02 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$educ*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$educ*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$contam*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

############################# The next best interaction is kids and education.  Let's add that one and try the others

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 2.063672e-01 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 6.479929e-02 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$lived*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 1.304956e-01

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$educ*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$hsc*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

################################## The next best one is kids*contam.  Let's add that one and try the others


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc )
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 2.028830e-01

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$educ*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.136542052 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ +d$kids*d$contam + d$hsc*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

##################### The next best one is lived*hsc Let's add that one and try the others


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.172632531

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc  +d$educ*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.042077253

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ +d$kids*d$contam +d$lived*d$hsc  + d$hsc*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

####################################### The next best one is hsc*educ.  Let's add that one and go from there

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# 0.127370398 

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ +d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  + d$hsc*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

############# The only one that works is kids*hsc.  Let's add that one and go from there.




out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  + d$gender*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  +d$gender*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  +d$gender*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  +d$gender*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  +d$kids*d$lived)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  +d$lived*d$educ)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc  +d$lived*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ +d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc   +d$kids*d$hsc + d$hsc*d$contam)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p
# no


################################## That's it! So our final model is:


out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc)
z = summary(out)$coefficients/summary(out)$standard.errors
p = (1 - pnorm(abs(z), 0, 1))*2
p

################################## Let's try with the model with the interactions
# that were only in common to both models:

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam + d$educ*d$hsc)


########## To check:

probabilities = predict(out) 

count = 0
for (i in 1:length(probabilities)){
  if(probabilities[i] == d$school[i]){count = count + 1}
}
count/length(probabilities)


# The percent of the add one in model is 0.8169935
# The percent of the AIC reduced model is 0.7973856
# The percent of the model with interactions in common to both 0.7973856
# The percent with no interactions at all is 0.7712418

#*******************************************************************************************#
#*******************************************************************************************#

#################################
# Performing diagnostics


# Model 1: Reduced by AIC

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$kids + 
                 d$lived*d$educ + d$kids*d$educ + d$kids*d$contam + d$educ*d$contam + d$educ*d$hsc)

# Model 2: Add 1 in model

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc)

# Model 3: interactions that are in common to both models

out = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam + d$educ*d$hsc)

# The percent of the add one in model is 0.8169935
# The percent of the AIC reduced model is 0.7973856
# The percent of the model with interactions in common to both 0.7973856
# The percent with no interactions at all is 0.7712418

probabilities = predict(out)

################################## Confusion matrix

confusionMatrix(probabilities, d$school)

################################## Pseudo R^2

install.packages('pscl')
require(pscl)

pR2(out)

################################## likelihood ratio tests

# Model 1:

model1 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$kids + 
                    d$lived*d$educ + d$kids*d$educ + d$kids*d$contam + d$educ*d$contam + d$educ*d$hsc)

# remove gender*lived

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$kids + 
                    d$lived*d$educ + d$kids*d$educ + d$kids*d$contam + d$educ*d$contam + d$educ*d$hsc)

lrtest(model1, model2) # P = 0.05107

# remove lived*educ

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$kids + 
                    d$kids*d$educ + d$kids*d$contam + d$educ*d$contam + d$educ*d$hsc)


# remove educ*contam

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$contam + d$gender*d$lived + d$gender*d$kids + 
                    d$lived*d$educ + d$kids*d$educ + d$kids*d$contam + d$educ*d$hsc)


lrtest(model1, model2) # P = 0.1454


# Model 2:

model1 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc  +d$kids*d$hsc)

# remove lived*hsc

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$educ*d$hsc  +d$kids*d$hsc)

lrtest(model1, model2) # P = 0.01468

# remove kids*hsc

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam +d$lived*d$hsc +d$educ*d$hsc)

lrtest(model1, model2) # P = 0.102


# Model 3: 

model1 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam + d$educ*d$hsc)

# remove gender*kids

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$kids*d$educ + d$kids*d$contam + d$educ*d$hsc)

lrtest(model1, model2) # P = 0.01

# remove educ*kids

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$contam + d$educ*d$hsc)

lrtest(model1, model2) # P = 0.02198

# remove kids*contam

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$educ*d$hsc)

lrtest(model1, model2) # P = 0.02721

# remove educ*hsc

model2 = multinom(d$school~d$gender + d$lived + d$educ + d$hsc + d$kids + d$contam + d$gender*d$kids + d$kids*d$educ + d$kids*d$contam)

lrtest(model1, model2) # P = 0.1353

################################## K-fold cross validation ?????????

ctrl = trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
out = train(school~gender + lived + educ + hsc + kids + contam + gender*kids + kids*educ + kids*contam + educ*hsc,
            data = d, method = "multinom", trControl = ctrl)

probabilities = predict(out, d) 



################################# Exploring how kids is related to other variables

b = d[d$kids == 'yes',]

mean(b$educ)
mean(d$educ)

mean(b$lived)
mean(d$lived)

dim(b[b$hsc == 'yes',])[1]/dim(b)[1]
dim(d[d$hsc == 'yes',])[1]/dim(d)[1]

dim(b[b$contam == 'yes',])[1]/dim(b)[1]
dim(d[d$contam == 'yes',])[1]/dim(d)[1]

dim(b[b$gender == 'male',])[1]/dim(b)[1]
dim(d[d$gender == 'male',])[1]/dim(d)[1]

# This is what you're looking for!

####### kids*gender

b = d[d$kids == 'yes' & d$gender == 'male',]
c = d[d$gender == 'male',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]

b = d[d$kids == 'yes' & d$gender == 'female',]
c = d[d$gender == 'female',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]

######## kids*contam

b = d[d$kids == 'yes' & d$contam == 'yes',]
c = d[d$contam == 'yes',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]

b = d[d$kids == 'yes' & d$contam == 'no',]
c = d[d$contam == 'no',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]


####### kids*hsc

b = d[d$kids == 'yes' & d$hsc == 'yes',]
c = d[d$hsc == 'yes',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]

b = d[d$kids == 'no' & d$hsc == 'yes',]
c = d[d$hsc == 'yes',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]

b = d[d$kids == 'yes' & d$hsc == 'no',]
c = d[d$hsc == 'no',]
dim(b[b$school == 'close',])[1]/dim(b)[1]
dim(c[c$school == 'close',])[1]/dim(c)[1]

####### kids*educ

b = d[d$kids == 'yes',]
tapply(b$educ, b$school, mean)
tapply(d$educ, d$school, mean)

b = d[d$kids == 'no',]
tapply(b$educ, b$school, mean)
tapply(d$educ, d$school, mean)








