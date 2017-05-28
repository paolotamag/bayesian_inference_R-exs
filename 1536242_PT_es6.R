#Paolo Tamagnini
#1536242
#paolotamag@gmail.com

set.seed(123)
N = 100
U = runif(N)

genNsim = function(N){
  set.seed(123)
  U = runif(N)
  X = numeric(length = N)
  for(i in 1:N){
    if(U[i]>=0 && U[i]<=0.25){
      X[i]=0 }
    else if(U[i]> 0.25 && U[i]<=0.6){
      X[i]=1 }
    else if(U[i]> 0.6 && U[i]<=1){
      X[i]=2 } }
  return(X) }

N = 100
X = genNsim(N)
Xdirect=sample(c(0,1,2),prob=c(0.25,0.35,0.4),size=N,replace=T)


mean(X)
mean(Xdirect)