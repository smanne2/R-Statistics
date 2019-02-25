r<-3
p<-0.001
x<-2000
q<-1-p
PX2000<-pnbinom(x-r,r,p)
EX<-r/p
sdx<-sqrt((r*q)/(p^2))