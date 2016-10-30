# Write the following command in R console if code gives an error
# install.packages("readxl")
library(readxl)

getSets <- function(file,trainSet,testSet) {
    table1 = read_excel(file);
    # print table1;
  
    print(table1);
}

getSets("Mumbai_Housing.xlsx",train,test)