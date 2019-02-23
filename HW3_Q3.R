m<-5
k<-10
n<-70
p<-m/(m+n)
fx<-dhyper(0,m,n,k)
fxAtleast1<-1-fx
fxExactly1<-dhyper(1,m,n,k)
EX<-k*p