#Paolo Tamagnini
#1536242
#paolotamag@gmail.com

generateTetaPost = function(N,a_post,b_post) {
  set.seed(123)
  a_prior = 1
  b_prior = 1
  fk = function(x){
    dbeta(x,a_post,b_post)/dbeta(x,a_prior,b_prior) }
  k = optim(c(a_post/(a_post+b_post)),fk,method="BFGS",control=list(fnscale=-1))$value
  
  tetaPost=NULL
  while (length(tetaPost)<N){
    
    U =runif(N*k)
    teta= rbeta(N*k,a_prior,b_prior)
    tetaPost=c(tetaPost,teta[U<dbeta(teta,a_post,b_post)/(k*dbeta(teta,a_prior,b_prior))])}
  
  tetaPost=tetaPost[1:N]
  return(tetaPost) }

a_post = 4
b_post = 8

tetapost = generateTetaPost(100000,4,8)

mean(tetapost)

a_post/(a_post+b_post)

#the mode(tetapost)
t = table(round(tetapost, digits = 3))
t[t==max(t)]


hist(tetapost,freq = F,breaks = 75, col = 'orange', main = 'empirical histogram vs target density')
xfit<-seq(min(tetapost),max(tetapost),length=length(tetapost)) 
yfit<-dbeta(xfit,4,8) 
lines(xfit, yfit, col="blue", lwd=2)

#alternative solution of exercise 8 where the condition

#instead of the condition Ui <= posterior(tetai)/(prior(tetai)*k)
#we use: Ui <= Likelihood(tetai)/Likelihood(tetaMLE)
#the result is the same

#all the data are from exercise 1
a_prior = 1
b_prior = 1
a_post = 4
b_post = 8
sumx=3
n=10
likeli = function(teta){
  return(teta^sumx*(1-teta)^(n-sumx)) }
LikeMLE = optim(c(a_post/(a_post+b_post)),likeli,method="BFGS",control=list(fnscale=-1))$value
N=100000
tetaPost=NULL
while (length(tetaPost)<N){
  
  U =runif(N*LikeMLE)
  teta= rbeta(N*LikeMLE,a_prior,b_prior)
  tetaPost=c(tetaPost,teta[U<Loglikeli(teta)/LikeMLE])}

tetaPost=tetaPost[1:N]

mean(tetaPost)

hist(tetaPost,freq = F,breaks = 75, col = 'orange', main = 'empirical histogram vs target density')
xfit<-seq(min(tetaPost),max(tetaPost),length=length(tetaPost)) 
yfit<-dbeta(xfit,4,8) 
lines(xfit, yfit, col="blue", lwd=2)
