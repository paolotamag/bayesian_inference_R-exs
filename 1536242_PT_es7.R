#Paolo Tamagnini
#1536242
#paolotamag@gmail.com

#target density f(x) : Beta(a1,b1)
#instrumental density g(y) : Beta(a2,b2)
#U is Uniform(0,1)
#acceptance condition: k*U < Beta(Y,a1,b1)/Beta(Y,a2,b2)
#parameter k: max(Beta(x,a1,b1)/Beta(x,a2,b2))

generateNBetafromBeta = function(N,a1,b1,a2,b2) {
  set.seed(123)
  #defining function to optimize
  funzBeta2 = function(x){
    dbeta(x,a1,b1)/dbeta(x,a2,b2)
  }
  #optimization starts from E(x)
  expf = c(a1/(a1+b1))
  k = optim(expf,funzBeta2,control=list(fnscale=-1),method = 'BFGS')$value
  print(k)  
  X=NULL
  
  #We will create samples U and Y of length N*k,
  #until X reaches length N.
  #if we are lucky this 'while' will iter just once.
  #This because to create N xi with p of accept. = 1/k,
  #we need approximately N*k ui and yi.
  #If we don't select enough xi in the first iteration,
  #we should definately get done in the second.
  
  while ( length(X) < N ){
    U =runif(N*k,min = 0,max = 1)
    Y= rbeta(N*k,a2,b2)
    X=c(X,Y[U<dbeta(Y,a1,b1)/(k*dbeta(Y,a2,b2))])}
  #let's take away any extra xi
  X=X[1:N]
  return(X) }

#I chose a beta(2,2) as instrumental or candidate distribution.
#It is better than a beta(1,1)=Unif(0,1) because the resulting k is higher.
#This gives a smaller p of acceptance which brings more efficiency.

X = generateNBetafromBeta(100000,3,3,2,2)

#anyway the exercise require a uniform candidate density, then is: 
X = generateNBetafromBeta(100000,3,3,1,1)

#as we can see it fits the target density
hist(X,freq = F,breaks = 75, col = 'orange', main = 'empirical histogram vs target density')
xfit<-seq(min(X),max(X),length=length(X)) 
yfit<-dbeta(xfit,3,3) 
lines(xfit, yfit, col="blue", lwd=2)

#we can now generate instead a Bernoulli sample on which we can approximate the prob of accept.
generateBernFromNTrials = function(N,a1,b1,a2,b2){
  set.seed(123)
  funzBeta2 = function(x){
    dbeta(x,a1,b1)/dbeta(x,a2,b2)
  }
  k = optim(c(a1/(a1+b1)),funzBeta2,method="BFGS",control=list(fnscale=-1))$value
  U =runif(N,min = 0,max = 1)
  Y= rbeta(N,a2,b2)
  X = Y[U<=dbeta(Y,a1,b1)/(k*dbeta(Y,a2,b2))]
  succ = rep(1,length(X))
  unsuc = rep(0,N - length(X))
  X = c(succ,unsuc)
  print('I =')
  print(1/k)
  print('I_hat =')
  print(mean(X))
  return(X) }

#candidate density = beta(2,2)
Xtilde = generateBernFromNTrials(10000,3,3,2,2)

#candidate density = beta(1,1) = unif(0,1)
Xtilde = generateBernFromNTrials(10000,3,3,1,1)