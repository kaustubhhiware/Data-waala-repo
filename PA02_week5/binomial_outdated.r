



#A<-array(data = 20,dim = 10,000,dimnames = NULL)
#B<-arrat

mat <-matrix(data =0,nrow = 10, ncol = 20)# n = 20, nrow actually 10K
for(i in 1:10)
{
  p<-runif(1,0,1)# generate p between 0 and 1
  n<-sample(20:100000,1)# generate n value from 20 to 1,00,000
  
  mat[i]<-array(data = rbinom(20,n,p),dim = 10,dimnames = NULL)
}

print mat