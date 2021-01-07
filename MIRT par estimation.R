library(mirt)


########### Function for estimating parameters ##############

par.est<- function(a, d, c, thetas, out, probmatr, dif, sqbias){
  
  for (i in 1:l){ #looping over respondents
    #calculate the value in the exponent (xp) for one respondent
    xp <- a%*%thetas[,i]+d
    #find the probability that someone answers "correctly" for each of the items 
    #(should result in a mx1 vector of probabilities; m number of items for 1 participent) 
    #add this as a row to a matrix of probabilities "probmatr"
    probmatr[i,]<- 1/(1+exp(-1*xp))
    
    for (j in 1:nrow(d)){ #looping over number of items
      #generate outcomes based on the probabilities from the probability matrix using
      #  the binomial distribution; each probability gets replaced with either a 0 or a 1
      out[i,j]<-rbinom(n=1, size = 1, prob = probmatr[i,j]) 
    }
  }
  #Recover parameters using the data generated in "out" and m number of dimensions
  #obtain the starting values before we fix & constrain values
  sv<- try(mirt(out, model = 2, pars = 'values', itemtype = '2PL', TOL = 0.0005))
  if (inherits(sv, "try-error")) {
    count = count+1
  }else{
    #fix the starting values to 1.0
    sv$value[sv$name=="a1"]<-sv$value[sv$name=="a2"]<- 1.0
    #fix the first value to 0.0 
    sv$value[1]<- 0.0
    #set estimation of first value to false so that we will not be estimating it further
    sv$est[1]<- F
    #set the lower bounds for the "a" parameters 
    sv$lbound[sv$name == "a1"] <- sv$lbound[sv$name == "a2"]<- 0.5
    #set the upper bounds for the "a" parameters  
    sv$ubound[sv$name == "a1"] <- sv$ubound[sv$name == "a2"]<- 1.5
    
    # estimate parameters given the new parameter specifications
    new<- try(mirt(out, model = 2, pars = sv, itemtype = '2PL'))
    
    if (inherits(new, "try-error")) {
      count = count+1
    }else{ 
      #if no error occurs, extract the parameters and bind them in ipar.est
      ipar.est<- coef(new, simplify=TRUE)$items
      #add the bias to update the matrix "dif" 
      dif<- dif + ipar.est[, 1:3]-cbind(a,d) 
      #add up the squared bias for rmse later
      sqbias<- sqbias + (ipar.est[, 1:3]-cbind(a,d))^2
      
    }}
  return(list("sqbias" = sqbias, "dif" = dif))
}

###### Initialize Variables For Simulation ####################

#m = number of dimensions for our mirt model
m<- 2
#n = number of items in scale
n<- 10
#l is our number of respondents
l<- 3000

#set number of trials in the simulation
trials <- 20
#count will keep track of the number of trials that result in errors 
count = 0
# "dif" is a matrix that will contain the accumulated biases;  
#   sqbias is the squared bias used for rmse later
dif <- sqbias<- matrix(0, nrow = n, ncol = m+1)


############## Begin Simulation ########################

for (k in 1:trials){ #loop over each trial
  #a = our discrimination parameters; each column is for each dimension, each row is per item
  a<- matrix(data = runif(n*m, min = .5, max = 1.5), nrow = n, ncol = m)
  #set the first item for the first parameter to 0
  a[1,1]=0.0
  #d = item intercept 
  d<- matrix(data = rnorm(n, mean = 0, sd = 1), nrow = n)
  
  #probmatr will contain the probabilities per item of each respondent enswering correctly
  #out will contain the data generated based off those probabilities (1 or 0)
  probmatr <- out<- matrix(NA, nrow = l, ncol = nrow(d))
  # must name each column for the mirt function later (a unique name for each item)
  colnames(out)<-  paste0("Item.", c(1:nrow(d)))
  #generate thetas: each row is a different dimension; each column is a different respondent
  thetas<- matrix(data = rnorm(m*l,0,1), nrow=m, ncol = l)

  final<- par.est(a, d, c, thetas, out, probmatr, dif, sqbias)
  
  

}
#end simulation


################ Final Results ######################
#the average RMSE for each item
rmse <- colSums(sqrt(final$sqbias/trials))/n
rmse
#the average bias for each parameter per item
write.table(round(final$dif/(trials), 4))
#the average bias for each parameter averaged over the items
write.table(round(colSums(final$dif/(trials))/n, 4))





