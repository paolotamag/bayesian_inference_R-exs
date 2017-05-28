#Paolo Tamagnini
#1536242
#paolotamag@gmail.com

x = c( 1.0,  1.5,  1.5,  1.5, 2.5, 4.0, 5.0, 5.0,  7.0, 8.0,  8.5,  9.0,  9.5, 9.5, 10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)
y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)
n = 27

loglike=function(teta){
  summa = 0
  for(i in 1:n){
    fi = dnorm(y[i],
               mean = teta[1]-teta[2]*teta[3]^x[i], 
               sd = sqrt(teta[4]), log = TRUE)
    summa = summa + fi }
  return(summa) }

#best starting point (see later)
startMLE = c(0.1,0.7,0.08,0.89) 

loglike(startMLE)

MLE_BFGS=optim(startMLE,loglike,method="BFGS",control=list(fnscale=-1))
MLE_BFGS$par
MLE_BFGS$value

MLE_ND=optim(startMLE,loglike,method="Nelder-Mead",control=list(fnscale=-1))
MLE_ND$par
MLE_ND$value

posteriorFunction=function(teta){
  pi = dunif(teta[3],min = 0, max = 1, log = TRUE)
  pi = pi + dnorm(teta[1], mean=0, sd = 100, log = TRUE)
  pi = pi + dnorm(teta[2], mean=0, sd = 100, log = TRUE)
  pi = pi +log(1/dgamma(teta[4],shape= 0.001, scale = 1/0.001))
  summa = loglike(teta) + pi
  return(summa) 
}

startMAP = MLE_ND$par

posteriorFunction(startMAP)


MAP_BFGS=optim(startMAP,posteriorFunction,method="BFGS",control=list(fnscale=-1))
MAP_BFGS$par
MAP_BFGS$value

MAP_ND=optim(startMAP,posteriorFunction,method="Nelder-Mead",control=list(fnscale=-1))
MAP_ND$par
MAP_ND$value

#------------------------------


#plotting the regression

plot(x, y)

tetaMLE = MLE_ND$par
#tetaMLE= c(2.65806,0.96351,0.87145,0.00806)
lines(x, tetaMLE[1]-tetaMLE[2]*tetaMLE[3]^x, col = "red")

tetaMAP = MAP_ND$par
#tetaMAP = c(2.65756, 0.96342, 0.87124, 0.00870)
lines(x, tetaMAP[1]-tetaMAP[2]*tetaMAP[3]^x, col = "blue")

#------------------------------




#Techniques to find the best starting point


#METHOD 1
#iterations each with a different combination that 
#gives a new starting point

#looking for the best starting point for MLE
bestsofarvalue = -10^6
for(i in (1/10)*1:100){
  for(j in (1/10)*1:100){
    for(k in (1/100)*1:100){
      for(h in (1/100)*1:100){
        startMLE = c(i,j,k,h)
        #print(startMLE)
        MLE_ND=optim(startMLE,loglike,method="Nelder-Mead",control=list(fnscale=-1))
        #print(MLE_ND$value)
        if (MLE_ND$value > bestsofarvalue) {
          bestsofarvalue = MLE_ND$value
          bestsofar = MLE_ND
          bestStart = startMLE
          print('--found a better startMLE--')
          print(bestStart)
          print(bestsofar$par)
          print(bestsofar$value)
        }
      }}}}
#bestMLE
bestsofar$par
bestsofar$value
bestStart

#looking for the best starting point for MAP
bestsofarvalue = -10^6
for(i in (1/10)*1:100){
  for(j in (1/10)*1:100){
    for(k in (1/100)*1:100){
      for(h in (1/100)*1:100){
        startMAP = c(i,j,k,h)
        #print(startMAP)
        MAP_ND=optim(startMAP,posteriorFunction,method="Nelder-Mead",control=list(fnscale=-1))
        #print(MLE_ND$value)
        if (MAP_ND$value > bestsofarvalue) {
          bestsofarvalue = MAP_ND$value
          bestsofar = MAP_ND
          print('--found a better startMAP--')
          print(startMAP)
          print(bestsofar$par)
          print(bestsofar$value)}
      }}}}
#bestMAP
bestsofar$par
bestsofar$value

#METHOD 2
#each optimization give the starting point 
#to the next one until it doesnt change anymore

#MLE
bestsofarvalue = -10^6
valuenew = -10^6
valueold = -10^7
startMLE = c(1,10,0.001,10)
while(abs(valuenew - valueold)>10^(-7)){
  valueold = valuenew
  MLE_ND=optim(startMLE,loglike,method="Nelder-Mead",control=list(fnscale=-1))
  valuenew = MLE_ND$value
  startMLE = MLE_ND$par
  if (MLE_ND$value > bestsofarvalue) {
    bestsofarvalue = MLE_ND$value
    bestsofar = MLE_ND
    print('--found a better startMLE--')
    print(startMLE)
    print(bestsofar$par)
    print(bestsofar$value)}}
bestsofar$par
bestsofar$value

#MAP
bestsofarvalue = -10^6
valuenew = -10^6
valueold = -10^7
startMAP = c(1,10,0.001,10)
while(abs(valuenew - valueold)>10^(-8)){
  valueold = valuenew
  MAP_ND=optim(startMAP,posteriorFunction,method="Nelder-Mead",control=list(fnscale=-1))
  valuenew = MAP_ND$value
  startMAP = MAP_ND$par
  if (MAP_ND$value > bestsofarvalue) {
    bestsofarvalue = MAP_ND$value
    bestsofar = MAP_ND
    print('--found a better startMAP--')
    print(startMAP)
    print(bestsofar$par)
    print(bestsofar$value)}}
bestsofar$par
bestsofar$value


