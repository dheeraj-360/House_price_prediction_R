ds = read.csv("E:\\DV_MA304\\Assignment\\37-00049_UOF-P_2016_prepped.csv")
new_ds <- ds[-1,]
#Exploring the house_data

#View its dimensions
dim(new_ds)

#Getting column names
names(new_ds)

#display the structure of the whole data set
str(new_ds)

#getting head and tail of the data
head(new_ds,5)
tail(new_ds,5)

#Checking for Missing values and percentage of missing values
colSums(is.na(new_ds))
print("Percentage of Missing Values")
colMeans(is.na(new_ds))*100

missing_cols<-names(which(colSums(is.na(new_ds)) > 0))
missing_cols
