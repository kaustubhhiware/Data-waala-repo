reader<- function(path){
  data = read.csv(file=path,header=T,sep=',');  
  stud_mean <- apply(data[,-1],1,mean);
  stud_var <- apply(data[,-1],1,var);

  sub_mean <- apply(data[,-1],2,mean);
  sub_var <- apply(data[,-1],2,var);
  
  write.table(stud_mean,file="stud_mean.csv",append=F,sep=',',col.names = T);
  write.table(stud_var,file="stud_var.csv",append=F,sep=',',col.names = T);
  write.table(sub_mean,file="sub_mean.csv",append=F,sep=',',col.names = T);
  write.table(sub_var,file="sub_var.csv",append=F,sep=',',col.names = T);  
}

reader("Data1.csv")