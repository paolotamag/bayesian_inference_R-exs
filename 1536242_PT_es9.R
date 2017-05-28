#Paolo Tamagnini
#1536242
#paolotamag@gmail.com

generateTetaPost = function(N) {
  set.seed(123)
  Loglikeli = function(x){
    dnorm(x,mean=0,sd=1)/dcauchy(x,location = 0, scale = 1)
  }
  logLikeMLE = optim(c(1),Loglikeli,method="BFGS",control=list(fnscale=-1))$value
  tetaPost=NULL
  while (length(tetaPost)<N){
    U =runif(N*logLikeMLE)
    teta= rcauchy(N*logLikeMLE,location = 0, scale = 1)
    tetaPost=c(tetaPost,teta[U<=dnorm(teta,mean=0,sd=1)/(logLikeMLE*dcauchy(teta,location = 0, scale = 1))])}
  tetaPost=tetaPost[1:N]
  return(tetaPost)
}
tetapost = generateTetaPost(100000)

mean(tetapost)

hist(tetapost,freq = F,breaks = 75, col = 'orange', main = 'empirical histogram vs target density')
xfit<-seq(min(tetapost),max(tetapost),length=length(tetapost)) 
yfit<-dnorm(xfit,0,1) 
lines(xfit, yfit, col="blue", lwd=2)

generateBernFromNTrials = function(N){
  set.seed(123)
  Loglikeli = function(x){
    dnorm(x,mean=0,sd=1)/dcauchy(x,location = 0, scale = 1)
  }
  logLikeMLE = optim(c(1),Loglikeli,method="BFGS",control=list(fnscale=-1))$value
  U =runif(N,min = 0,max = 1)
  teta= rcauchy(N,location = 0, scale = 1)
  tetapost = teta[U<=dnorm(teta,mean=0,sd=1)/(logLikeMLE*dcauchy(teta,location = 0, scale = 1))]
  succ = rep(1,length(tetapost))
  unsuc = rep(0,N - length(tetapost))
  X = c(succ,unsuc)
  print('approx. accept. prob.')
  print(mean(X))
  print('theoretical accept. prob.')
  print(1/logLikeMLE)
  print('# of successes')
  print(sum(X))
  print('# of trials')
  print(N)
  return(X) }

Xtilde = generateBernFromNTrials(100000)
