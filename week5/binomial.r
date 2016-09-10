rowVars1 <- function (x,na.rm = TRUE) 
{
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}
###
### Results generated are stored in binomial.csv and the plots are in binomial_histogram.pdf
###
binomial.samples <- function(mean, n) {
  arr <- matrix(nrow = 1, ncol = n);
  for(i in 1:n) {
    s<-sample(20:100000,1)# generate sample size value from 20 to 1,00,000
    arr[1,i] <- pbinom(i-1, s,mean*1.0/s);
  }
  return(arr);
}

get.samples <- function(n,rows) {
  arr <- matrix(nrow = rows, ncol = n);
  meanarr <- runif(rows, min = 1, max = n);
  for(i in 1:rows) {
    temparr <- binomial.samples(meanarr[i], n);
    for(j in 1:n) {
      arr[i,] <- temparr[1,];
    }
  }
  write.table(arr, file = "binomial.csv", sep=",", row.names = F, col.names=T);
  
  Data <-read.csv("binomial.csv",header = T,sep=",")
  Data$mean <- round(rowMeans(Data[,-1],na.rm=TRUE),4);
  Data$var <- round(rowVars1(Data[,-1],na.rm=TRUE),4);
  
  pdf("binomial_histogram.pdf");
  hist(Data$mean);
  hist(Data$var);
  dev.off();
  write.table(Data[,-1], file = "binomial.csv", sep=",", row.names=F, col.names=T);
  
}
get.samples(20,10000);# value of n is 20 and 10,000 samples drawn
