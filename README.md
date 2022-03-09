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