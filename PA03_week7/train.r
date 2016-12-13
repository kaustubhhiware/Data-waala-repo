
loc = "Mumbai_Housing.csv"# local address might be changed
# create a outout folder in pwd in case code fails to store csv
MyData <- read.csv(file = loc, header = TRUE, sep = ',',nrows=506)
#MyData <- na.omit(MyData)
dt = sort(sample(nrow(MyData), nrow(MyData)*.9))

train<-MyData[dt,]
test<-MyData[-dt,]
print("Training set for this iteration saved in train.csv ")
write.table(train, file ="output/train.csv", sep=",", row.names=T, col.names=T);

print("Test set for this iteration saved in test.csv")
write.table(test, file ="output/test.csv", sep=",", row.names=T, col.names=T);

R_squared = lm(formula = MEDV~. , data = train)
R_squared_test = predict(R_squared,newdata = test)

print("Printing predictions and actual values :")
##print predicted and actual value
cat("Index\tActual value\tPrediction\n")
for(i in 1:51)#51 is 10 %
{
  cat(paste0(i,"\t",test[i,14],"\t",R_squared_test[i],"\n"))
  #print(paste0("actual value=",test[i,14]))
  #print(paste0("predicted value=",R_squared_test[i]))  
}
print("+----")  
R_2 <- summary(R_squared)$r.squared
print(paste0("Value of R*R = ",R_2))

corrtable = cor(test, use = "all.obs")
print("Correlation matrix save in correlation.csv")
write.table(corrtable, file ="output/correlation.csv", sep=",", row.names=F, col.names=T);


max = 0
max_i=0
max_j=0
for(i in 1:12)
{
  for (j in (i+1):14)
  {
    mod = corrtable[i,j]
    if(mod < 0){
      mod = -1*mod
      }
    if(max < mod)
    {
      max_i = i
      max_j = j
      max = mod
    }
  }
}
print("One of the following has to be removed")
print(rownames(corrtable)[max_i])
print(rownames(corrtable)[max_j])

# part 5 is to be covered in report