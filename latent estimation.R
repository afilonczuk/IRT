library(MASS)

## begin function
trait.est <- function(a,d, c, thetas, iter, probmatr, out){
  
  #create a matrix that will hold the bias between the generated and recovered thetas
  diff<-theta.est<- matrix(NA, nrow=p, ncol = l)
  #e will hold "delta" for each step in the approximation; this is specific for p=2
  e<- matrix(NA, nrow = p*l, ncol = iter)
  
  
  #generate probabilities and data
  for (i in 1:ncol(thetas)){ #looping over respondents
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
  
  #begin estimating thetas
  for (k in 1:ncol(thetas)){#loop over praticipants
    #initialize theta to be 0
    theta_1<-  matrix(0, nrow = ncol(a))
    for (j in 1:iter){
      #set new theta as our old theta
      theta_0<- theta_1 
      
      #calculate value in exponent
      xp <- a%*%theta_0+d
      #calculate probability of answering each item correctly
      P<- 1/(1+exp(-1.7*xp))
      
      #find 1st and 2nd derivatives
      firstderiv<- 1.7*t(a)%*%((out[k,]-P)*(P-c)/((1-c)*P))
      secderiv<- (1.7^2)*(t(a)%*%a)*colSums((1-P)*(P-c)*(c*out[k,]-P*P)/((P*P)*(1-c)^2))
      
      
      delta <- ginv(secderiv)%*%firstderiv
      
      #subtract the delta to our approximate theta
      theta_1 <- theta_0 - delta
      
    }
   
    
  
  
  #view the estimated thetas later
  theta.est[,k]<- theta_1
  #subtract the true from the estimated theta 
  diff[,k]<- theta_1 - thetas[,k]
  }
  return(diff)
}

#p = number of dimensions for our mirt model
p<- 2
#n = number of items in scale
n<- 100
#l is our number of respondents
l<- 2000
#set number of simulations
sim=50
#initialize bias and rmse at 0
bias<-rsme<- matrix(0, nrow = p)
#set number of iterations for Newton-Raphson approximations
iter<-50
#count=0


####################   #begin simulation ############################

for(h in 1:sim){
#a = our discrimination parameters; each column is for each dimension, each row is per item
a<- matrix(data = runif(n*p, min = .5, max = 1.5), nrow = n, ncol = p)
#d = item intercept (one per item)
d<- matrix(data = rnorm(n, mean = 0, sd = 1), nrow = n)
#c is guessing parameter, set to 0
c<-matrix(0, nrow = n)

#probmatr will contain the probabilities per item of each respondent enswering correctly
#out will contain the data generated based off those probabilities (1 or 0)
probmatr <- out<- matrix(NA, nrow = l, ncol = nrow(d))
#generate thetas: each row is a different dimension; each column is a different respondent
thetas<- matrix(data = rnorm(p*l,0,1), nrow=p, ncol = l)

final<- trait.est(a,d,c,thetas, iter, probmatr, out)
  
#find the average bias by adding up all thetas across each dimension and dividing by number of participants
bias<- bias+ rowSums(final)/l
rsme<- rsme +sqrt(rowSums(final^2)/l)
}
bias/sim
rsme/sim
