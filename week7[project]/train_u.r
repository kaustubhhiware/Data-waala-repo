
loc = "Mumbai_Housing.csv"# local address might be changed

MyData <- read.csv(file = loc, header = TRUE, sep = ',',nrows=506)
#MyData <- na.omit(MyData)
dt = sort(sample(nrow(MyData), nrow(MyData)*.9))

train<-MyData[dt,]
test<-MyData[-dt,]
print("\nTraining set for this iteration : \n")
print(train)

print("\nTest set for this iteration : \n")
print(train)

R_squared = lm(formula = MEDV~. , data = train)
R_squared_test = predict(R_squared,newdata = test)

print("Printing predictions and actual values :\n\n\n")
##print predicted and actual value
for(i in 1:51)#51 is 10 %
{
  print(paste0("actual value=",test[i,14]))
  print(paste0("predicted value=",R_squared_test[i]))  
}
print("+----")  
R_2 <- summary(R_squared)$r.squared
print(paste0("Value of R*R = ",R_2))

corrtable = cor(test, use = "all.obs")
print("Correlation matrix : \n")
print(corrtable)

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