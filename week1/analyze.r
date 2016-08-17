rowVars <- function (x,na.rm = TRUE) 
{
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}

read.tcsv = function(file, header=TRUE, sep=",", ...) {
  
  n = max(count.fields(file, sep=sep), na.rm=TRUE)
  x = readLines(file)
  
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  
  x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x = apply(x, 1, paste, collapse=sep) 
  out = read.csv(text=x, sep=sep, header=header, ...)
  return(out)
  
}

getData <- function(path) {
	table1 <- read.csv(path, header = T, sep=",");
	table2 <- read.tcsv(path, header = T, sep=",");
	
	table1$mean <- round(rowMeans(table1[,-1],na.rm=TRUE),2);
	table1$var <- round(rowVars(table1[,-1],na.rm=TRUE),2);
	table1$stddev <- round(sqrt(table1$var),2);

	table2$mean <- round(rowMeans(table2[,-1],na.rm=TRUE),2);
	table2$var <- round(rowVars(table2[,-1],na.rm=TRUE),2);
	table2$stddev <- round(sqrt(table2$var),2);

	stud_medians <- median(table1$mean)#apply(table1,1,median,na.rm=TRUE);
	sub_medians <- median(table2$mean);
	print(stud_medians);
	print(sub_medians);
	write.table(table1[,-1], file = "students.csv", sep=",", row.names=F, col.names=T);
	write.table(table2, file = "subjects.csv", sep=",", row.names=F, col.names=T);

}

getData("Data1.csv");
#getData("Data2.csv");

