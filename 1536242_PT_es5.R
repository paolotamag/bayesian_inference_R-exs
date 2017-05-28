#Paolo Tamagnini
#1536242
#paolotamag@gmail.com

set.seed(123)
N=10000
U = runif(N)
X = (1-U)^(-2/5)

dpar = function(x,a,b){
  if(x>b){
    return(a*(b^a/x^(a+1))) }
  else{
    return(0) } }

Xplot=X[X<5]
hist(Xplot,freq = F,breaks = 75, col = 'orange', main = 'empirical histogram vs target density')
xfit<-seq(0,max(Xplot),length=N) 
yfit=rep(0,N)
for (i in 1:N){
  yfit[i] = dpar(xfit[i],2.5,1)
}
lines(xfit, yfit, col="blue", lwd=3)


approxI=mean(X)
expval=2.5/1.5
runningmeans=cumsum(X)/(1:N)
plot(1:N,runningmeans,type="l")
abline(h=expval, col='red')




