poisson.samples <- function(mean, n) {
	arr <- matrix(nrow = 1, ncol = n);
	for(i in 1:n) {
		arr[1,i] <- ppois(i-1, lambda = mean);
	}
	# print("The ppois array is\n");
	# print(arr)
	return(arr);
}
rowVars1 <- function (x,na.rm = TRUE) 
{
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

get.samples <- function(n,rows) {
	arr <- matrix(nrow = rows, ncol = n);
	meanarr <- runif(rows, min = 1, max = n);
	for(i in 1:rows) {
		temparr <- poisson.samples(meanarr[i], n);
		for(j in 1:n) {
			arr[i,] <- temparr[1,];
		}
	}
	write.table(arr, file = "poisson.csv", sep=",", row.names = F, col.names=T);
	table1 <-read.csv("poisson.csv",header = T,sep=",")
  	table1$mean <- round(rowMeans(table1[,-1],na.rm=TRUE),2);
	table1$var <- round(rowVars1(table1[,-1],na.rm=TRUE),2);
	pdf("poisson_histogram.pdf");
	hist(table1$mean);
	hist(table1$var);
	dev.off();
 	write.table(table1[,-1], file = "poisson.csv", sep=",", row.names=F, col.names=T);
}
get.samples(5,10000);