library(mirt)

###### Initialize Variables For Simulation ####################

#m = number of dimensions for our mirt model
m<- 2
#n = number of items in scale
n<- 10
#l is our number of respondents
l<- 100
#set number of trials in the simulation
trials <- 500
#count will keep track of the number of trials that result in errors 
count = 0
# "dif" is a matrix that will contain the accumulated differences 
#   between the estimated and truth ipar values
dif <- matrix(0, nrow = n, ncol = m+1)


############## Begin Simulation ########################
  #Note: if you change the number of items, you will have to add/remove coefficients in line 59 

for (k in 1:trials){ #loop over each trial
  #probmatr will contain the probabilities per item of each respondent enswering correctly
  #out will contain the data generated based off those probabilities (1 or 0)
  probmatr <- out<- matrix(NA, nrow = l, ncol = nrow(d))
  # must name each column for the mirt function later (a unique name for each item)
  colnames(out)<-  paste("Item.", c(1:nrow(d)))

#a = our discrimination parameters; each column is for each dimension, each row is per item
a<-matrix(data = runif(n*m, min = .5, max = 1.5), nrow = n, ncol = m)
#d = item intercept (one per item)
d<- matrix(data = rnorm(n, mean = 0, sd = 1), nrow = n)

#generate thetas: each row is a different dimension; each column is a different respondent
thetas<- matrix(data = rnorm(m*l,0,1), nrow=m, ncol = l)

  for (i in 1:l){ #looping over respondents
    #calculate the value in the exponent (xp) for one respondent
    xp <- a%*%thetas[,i]+d
    #find the probability that someone answers "correctly" for each of the items 
      #(should result in a mx1 vector of probabilities; m number of items for 1 participent) 
      #add this as a row to a matrix of probabilities "probmatr"
    probmatr[i,]<- 1/(1+exp(-1.7*xp))
    
  for (j in 1:nrow(d)){ #looping over number of items
    #generate outcomes based on the probabilities from the probability matrix using
    #  the binomial distribution; each probability gets replaced with either a 0 or a 1
    out[i,j]<-rbinom(n=1, size = 1, prob = probmatr[i,j]) 
  }
  }
   
#recover parameters using the data generated in "out" and m number of dimensions
new<- try(mirt(out, m,  itemtype = '2PL', TOL = 0.0005))
#if the mirt function results in an error, "1" is added to the count
if (inherits(new, "try-error")) {
    count = count+1
  }else{ 
    #otherwise, if no error in mirt, extract the items from the object "new" and bind them as rows in ipar.est
     #this code is written for 10 items and must be revised if the number of items is changed 
      ipar.est<- rbind(coef(new)$`Item. 1`, coef(new)$`Item. 2`, coef(new)$`Item. 3`, coef(new)$`Item. 4`, 
                       coef(new)$`Item. 5`, coef(new)$`Item. 6`, coef(new)$`Item. 7`, coef(new)$`Item. 8`, 
                       coef(new)$`Item. 9`, coef(new)$`Item. 10`)
      
    #update the matrix "dif" with the difference between the actual and estimated parameters
    dif<- dif + cbind(a,d) - ipar.est[,1:3]
}
}#end simulation
#divide the accumulated differences by the number of trials - the number of trials with errors
dif/(trials-count)

#count is the number of trials with errors
count
colSums(dif/(trials-count))
