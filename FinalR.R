##Data Import
library(tuneR) #package
setwd("C:/Users/user/Downloads") #setting directory
s = readWave("Test.wav") #reading the audio file

##Exploratory Analysis
s #Summary
play(s) #playing the audio file in R
plot(s@left,ty="l",col="blue",
     xlab="Index",ylab="Values",
     main="Time Domain Plot") #plotting the left part



##Applying k consecutive regression

res= numeric(length(s)-1000) #creating a null vector

for(i in 1:(length(res))){
  z=sl[seq(i,i+999,1)] #taking k points at a time
  q = seq(1,1000,1) #the indices
  p = lm(z~q) #plotting the model
  res[i]= sum((abs(p$residuals)^3))#residuals
  print(i)#to see the progress
  }

##Smoothing the previous vector 

a = floor(length(res)/5000)*5000 #Finding the precise intervals
mres = numeric(length(res))
c = seq(1,a-4999,5000)
for(i in c){
  y = res[seq(i,i+4999,1)]
  mres[seq(i,i+4999,1)]=mean(y)
}

##Detecting the Peaks

k=0 #counter
peak = numeric() #null vector

for(i in c[-c(1,length(c))]){
  y = mres[seq(i-5000,i-1,1)] #previous block
  z = mres[seq(i,i+4999,1)] #current block
  w = mres[seq(i+5000,i+9999,1)] #next block
  
  #Choose appropriate value of alpha in place of 10^10 in
  #the following code, generally it is recommended to use
  #something in between the square root of maximum value 
  #of mres and order of 10 in maximum of value of mres-1
  #however slight changes may have under or over detection
  if(z[1]-y[1]>10^10 && z[1]-w[1]>10^10){
    k=k+1 #increase the counter 
    peak[k]=median(seq(i,i+4999,1)) #detecting the peak
  }
}
onset = peak/s@samp.rate #final time stamps

#Plotting the final detected peaks in the time domain plot
plot(s@left,ty="l",col="blue",
     xlab="Index",ylab="Values",
     main="Detected peaks in pink")
abline(v=peak,col="deeppink")



###The following part is for removing noise from the data
#Hence it is totally optional, however if there are much
#noises in the data, it is recommended to remove the noise first
#applying FFT
z = fft(s@left)/sqrt(length(s)) #creating a new vector in f.d
zclean = z
zclean[abs(z)<2000]=0 #removing low frequency
plot(abs(zclean),ty="l")
sclean = s #creating a new vector in t.d
snd  = fft(zclean, inv=T)*sqrt(length(s)) #applying inverse FFT
tmp = Re(snd)#taking real part
sclean@left = 30000*tmp/max(abs(tmp))
sclean@right = rep(0,length(tmp))
play(sclean)




















