# holly
#initial guess of the sample size
#we know that alpha>=0.05 and beta>=0.2
#we know delta = 0.7- 0.5 = 0.2
sigma^2 = 
  initial<-function(sigma, delta, alpha, beta){
    n<-sigma*(qnorm(1-alpha)-qnorm(beta))^2/delta^2
    return(n)
  }
initial(0.25,0.2,0.05,0.2)
#38.64098 , round up no 39=n1
#choose n2=78


#now we want to find an appropiate gamma and alpha
#finding the expected sample size for any hypothesis
evaluate_design <- function(gamma, lambda, n1, n2, theta) {
  #stage 1
  #set number of simulations 
  M1 <- 10^4
  #simulation observations using prior distribution
  y1 <- rbinom(M1, n1, theta)
  #find posterior distribution parameters
  a1 <- 0.5 + y1
  b1 <- 0.5 + n1 - y1
  #find probability of futility (posterior distribution)
  fut1 <- pbeta(theta, a1, b1)
  #decision value
  c1 <- 1 - lambda * (n1 / n2)^gamma
  
  #number of successes, is used in stage 2
  M2<- sum(fut1>c1)
  #simulation y2 
  y2 <- rbinom(M2, n2, theta)
  #set new posterior parameters
  a2 <- 0.5 + y2
  b2<- 0.5 + n2 - y2
  #find propbability of futility
  fut2 <- pbeta(theta, a2, b2)
  #find decision value
  c2<- 1 - lambda * (n2 / n2)^gamma
  #find the type I and type II error using monte carlo estimates
  return(c(typeI = sum(fut1>c1)/M1, typeII=sum(fut2>c2)/M2))
}
#example
evaluate_design(0.4,0.75,50,70,0.5) 
#test