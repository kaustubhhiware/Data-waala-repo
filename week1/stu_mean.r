
getData <- function(path) {
	table1 <- read.csv(path, header = T, sep = ",");
	stud_mean <- matrix(apply(table1[,-1], 1, mean));
	stud_var <- matrix(apply(table1[,-1], 1, var));
	stud_stddev <- matrix(apply(stud_var, 1, sqrt));
	sub_mean <- matrix(apply(table1[,-1], 2, mean));
	sub_var <- matrix(apply(table1[,-1], 2, var));
	sub_stddev <- matrix(apply(sub_var, 2, sqrt));

	#stud_std <- apply(table1[,-1], 1, std);
	stud_medians <- median(stud_mean);
	sub_medians <- median(sub_mean);
	print(stud_medians);
	print(sub_medians);
	write.table(stud_mean, file = "stu_mean.csv", sep=",", col.names=F);
	write.table(stud_var, file = "stu_var.csv" , sep=",", col.names=F);
	write.table(stud_stddev, file = "stu_stddev.csv" , sep=",", col.names=F);
	write.table(sub_mean, file = "sub_mean.csv", sep=",", col.names=F);
	write.table(sub_var, file = "sub_var.csv" , sep=",", col.names=F);
	write.table(sub_stddev, file = "sub_stddev.csv" , sep=",", col.names=F);
}

getData("Data1.csv");



# stu.var = function(path) {
# 	table1 = read.csv(path);
# 	stud_mean = apply(table1[-1,], 1, var);
# 	write.csv(studmean);
# }
# stumean = function(path) {
# 	table1 = read.csv(path);
# 	stud_mean = apply(table1[-1,], 1, std);
# 	write.csv(studmean);
# }
