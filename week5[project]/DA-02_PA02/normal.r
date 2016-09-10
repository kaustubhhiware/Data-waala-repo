
rowVars1 <- function (x,na.rm = TRUE) 
{
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}
###
### Results generated are stored in normal.csv and the plots are in normal_histogram.pdf
### Since the output is random , you might have to run this a couple of times to get dispersed data
get.samples <- function(n,rows) {
	arr <- matrix(nrow = rows, ncol = n);
	meanarr <- runif(rows,min = 0.01, max = 1);
	sdarr <- runif(rows, min = 0.01, max = 1);
	xrange <- runif(rows, min = -10, max = 10);
	for(i in 1:rows) {
		temparr <- dnorm(xrange, mean = meanarr[i], sd = sdarr[i]);
		for(j in 1:n) {
			arr[i,j] <- temparr[j];
		}
	}
	write.table(arr, file = "normal.csv", sep=",", row.names = F, col.names=T);
	table1 <-read.csv("normal.csv",header = T,sep=",")
  	table1$mean <- rowMeans(table1[,-1],na.rm=TRUE);
	table1$var <- rowVars1(table1[,-1],na.rm=TRUE);
	pdf("normal_histogram.pdf");
	hist(table1$mean);
	hist(table1$var);
	dev.off();
 	write.table(table1[,-1], file = "normal.csv", sep=",", row.names=F, col.names=T);
 	
 	# print();
}
get.samples(20,10000);# n = 20 , 10K samples