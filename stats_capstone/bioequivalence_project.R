library("Hmisc")

set.seed(Sys.time())

dataA = read.table("http://inside.mines.edu/~wnavidi/math482/tretinoinA", header = TRUE)
dataA = as.matrix(dataA)
dataA[dataA == 0] = 3
dataA = log(dataA)

dataB = read.table("http://inside.mines.edu/~wnavidi/math482/tretinoinB", header = TRUE)
dataB = as.matrix(dataB)
dataB[dataB == 0] = 3
dataB = log(dataB)

require(expm)

timea = c(0.25, 0.5, 1, 1.5, 4.5, 7.5, 10.5, 13.5)
timeb = c(0.1, 0.35, 0.75, 1.25, 3.0, 6.0, 9.0, 12.0)
timec = c(0.35, 0.75, 1.25, 3.0, 6.0, 9.0, 12.0, 15.0)
timed = c(1.5, 1.5, 1.5, 1.5, 10.5, 10.5, 10.5, 10.5)

time = timeb

#######################################################################
#######################################################################
# Find means for formulations A and B

meansA = c()

for(i in 1:length(dataA[1,])){
  if (any(timea == time[i])){
    t = which(timea %in% time[i])
    meansA[i] = mean(dataA[,t])
  }
  else if(time[i] < timea[1]){
    meansA[i] = approxExtrap(c(timea[1], timea[2]), c(mean(dataA[,1]), mean(dataA[,2])), time[i])$y
  }
  else if(time[i] > timea[8]){
    meansA[i] = approxExtrap(c(timea[7], timea[8]), c(mean(dataA[,7]), mean(dataA[,8])), time[i])$y
  }
  else{
    hight = time[time[i] < timea]
    lowt = time[time[i] > timea]
    a = which(time %in% min(hight))
    b = which(time %in% max(lowt))
    meansA[i] = mean(c(mean(dataA[,a]), mean(dataA[,b])))
  }
}

r = 1 #Take the ratio to be one
r2 = 1

meansB = c()
meansB[1:4] = meansA[1:4] + log(r)
for(t in 5:8){
  meansB[t] = meansA[4] - r2*(meansA[4] - meansA[t]) + log(r)
}

# Find variances for formulations A and B

varianceA = c()
varianceB = c()

for(i in 1:length(dataA[1,])){
  if (any(timea == time[i])){
    t = which(timea %in% time[i])
    varianceA[i] = var(dataA[,t])
    varianceB[i] = var(dataB[,t])
  }
  else if(time[i] < timea[1]){
    varianceA[i] = var(dataA[,1])
    varianceB[i] = var(dataB[,1])
  }
  else if(time[i] > timea[8]){
    varianceA[i] = var(dataA[,8])
    varianceB[i] = var(dataB[,8])
  }
  else{
    hight = time[time[i] < timea]
    lowt = time[time[i] > timea]
    a = which(time %in% min(hight))
    b = which(time %in% max(lowt))
    varianceA[i] = mean(c(var(dataA[,a]), var(dataA[,b])))
    varianceB[i] = mean(c(var(dataB[,a]), var(dataB[,b])))
  }
}

#######################################################################
#######################################################################

l = 0.4 #The correlation between any pair of log measurements
n = 50 #The number of subjects

# Compute the covariance matrix of the log of the tretinoin data

variance = c(varianceA, varianceB)

covmat = matrix(0, nrow = 16, ncol = 16)
for(i in 1:16){
  for(j in 1:16){
    if(i == j){
      covmat[i, j] = variance[i]
    }
    else{
      covmat[i, j] = l*sqrt(variance[i])*sqrt(variance[j])
    }
  }
}

#######################################################################
#######################################################################

##################### For normal data  
newmat =  mvrnorm(50, rep(0, 16), covmat) 
  
##################### For skewed data 
# newmat = mvrnorm(50, rep(0, 16), diag(1, nrow = 16, ncol = 16))
# newmat = newmat^2
# newmat = 0.5*newmat - 0.5
# newmat = newmat%*%sqrtm(covmat)

# Add the true mean for formulation A to the first 8 columns and the true mean for 
# formulation B to the second 8 columns

means = c(meansA, meansB)

for(i in 1:n){
  newmat[i,] = newmat[i,] + means
}

newmat = exp(newmat)

# Estimate Cmax for each subject

FormA = newmat[,1:8]
FormB = newmat[,9:16]

# Make vectors os functions

ratio = c(medianratio, ratiomeans, biasratio, lognormalratio, meanlogratio)
params = c(Cmax, AUC, AUC2, ek, C0)
methods = c(bootint1, bootint2, permutation)

##########################################################
########************ Start Below! ****************########
##########################################################

bioequivalent = array(0, c(5, 3, 5))
bioinequivalent = array(0, c(5, 3, 5))
neither = array(0, c(5, 3, 5))

for(h in 1:100){
  
  newmat =  mvrnorm(50, rep(0, 16), covmat) 
  
  ##################### Below is for skewed data
  # newmat = mvrnorm(50, rep(0, 16), diag(1, nrow = 16, ncol = 16))
  # newmat = newmat^2
  # newmat = 0.5*newmat - 0.5
  # newmat = newmat%*%sqrtm(covmat)
  
  means = c(meansA, meansB)
  
  for(i in 1:n){
    newmat[i,] = newmat[i,] + means
  }
  newmat = exp(newmat)
  
  FormA = newmat[,1:8]
  FormB = newmat[,9:16]
  
  for (i in 1:3){
    for (j in 1:5){
      for(k in 1:5){
        
        if (identical(ratio[[k]], meanlogratio)){
          if(exp(methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[1]) > 0.8 & exp(methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[2]) < 1.25){
            bioequivalent[k, i, j] = bioequivalent[k, i, j] + 1}
          else if(exp(methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[1]) > 1.25 || exp(methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[2]) < 0.8){        
            bioinequivalent[k, i, j] = bioinequivalent[k, i, j] + 1}
          else{
            neither[k, i, j] = neither[k, i, j] + 1}
        }
        else {
          if(methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[1] > 0.8 & methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[2] < 1.25){
            bioequivalent[k, i, j] = bioequivalent[k, i, j] + 1}
          else if(methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[1] > 1.25 || methods[[i]](FormA, FormB, 50, time, params[[j]], ratio[[k]])[2] < 0.8){        
            bioinequivalent[k, i, j] = bioinequivalent[k, i, j] + 1}
          else{
            neither[k, i, j] = neither[k, i, j] + 1}
        }
        print(bioequivalent/100)
      }
    }
  }
}

bioequivalent = bioequivalent/100
bioinequivalent = bioinequivalent/100
neither = neither/100












