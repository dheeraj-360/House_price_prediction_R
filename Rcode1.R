#install required packages
install.packages("dplyr")
install.packages("corrplot")
install.packages("VIM")
install.packages("MICE")
install.packages("psych")
install.packages("ggstatsplot")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("knitr")
install.packages("ipred")

#Attaching the required libraries

library(dplyr)
library(VIM)
library(mice)
library(psych)
library(ggstatsplot)
library(caret)
library(nnet)
library(randomForest)
library(e1071)
library(corrplot)
library(knitr)
library(ipred)
#Loading house_data set

house_data <- read.csv("C:\\Users\\dheer\\OneDrive\\Desktop\\MA321\\Assignment\\house-data.csv")

#Exploring the house_data

#View its dimensions
dim(house_data)

#Getting column names
names(house_data)

#display the structure of the whole data set
str(house_data)

#getting head and tail of the data
head(house_data,5)
tail(house_data,5)


#Checking for Missing values and percentage of missing values
colSums(is.na(house_data))
print("Percentage of Missing Values")
colMeans(is.na(house_data))*100

missing_cols<-names(which(colSums(is.na(house_data)) > 0))
missing_cols

#To visualize how the missing values are distributed
missing_plot <- aggr(house_data[missing_cols], col=c('red','skyblue'),
                     numbers=TRUE, sortVars=TRUE,
                     labels=names(house_data[missing_cols]), cex.axis=.7,
                     gap=3, ylab=c("Proportion","Pattern"))

#Removing the columns with missing values equal or greater than 80%
miss_high <- names(which(colMeans(is.na(house_data))>0.8))
miss_high

house_data_new<-house_data[, which(colMeans(!is.na(house_data)) >=0.8)]
colMeans(is.na(house_data_new))
names(house_data_new)

temp_house_data<- house_data_new


#Data Analysis and summaries

#numerical data

data_num2 <- select_if(temp_house_data, is.numeric)             
dim(data_num2)
names(data_num2)

summary(data_num2)

#correlations

correlations <- cor(na.omit(data_num2[,-1]))

#correlation plot using numerical varaibles
row <- apply(correlations, 1, function(x) sum(x > 0.5 | x < -0.5) > 1)
correlations<- correlations[row ,row ]
corrplot(correlations, method="number")

#converting all character to factor varaible 

for (col in names(temp_house_data)) {
  if (is.character(temp_house_data[[col]])) {
    temp_house_data[[col]] <- as.factor(temp_house_data[[col]])
  }
}

#selecting only factor (categorical) varaibles
cat <- select_if(temp_house_data, is.factor)   
summary(cat)


str(temp_house_data)

#plotting the histogram for every categorical varaibles
for (var in names(cat)) {
  plot<-ggplot(temp_house_data, aes(x=get(var))) +
    geom_bar() +
    labs(title=paste("Histogram of", var), x=var, y="Frequency")
  print(plot)
}


#Exploring Sale price column by its histogram
plot<-ggplot(temp_house_data, aes(x=SalePrice)) +
  geom_histogram(fill="blue",binwidth=10000) +
  labs(title=paste("Histogram of SalePrice"), x="SalePrice", y="Frequency")+
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))
print(plot)

#using kable function to get the descriptive statistics of sale price in table format
kable(as.array(summary(temp_house_data$SalePrice)),
      caption = 'Summary of Sales Price')

# Create a scatter plot (year_sold vs saleprice)
plot <- ggplot(temp_house_data, aes(x = YrSold, y = SalePrice)) +
  geom_point(col = "blue") +
  labs(title = "Sale Price vs. Sale Year", x = "Sale Year", y = "Sale Price") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))
print(plot)

#Scatter plot for living area vs sale_price
plot<-ggplot(temp_house_data, aes(x=SalePrice, y=GrLivArea)) +
  geom_point(colour="red", alpha=0.3)+
  labs(title = "General Living Area vs. Sale Price", x="SalePrice", y="LivingArea")+
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))
print(plot)

#boxplot for visualazing outliers:
boxplot(house_data$SalePrice, main = "SalePrice Boxplot",
        boxwex = 0.5, # sets the width of the box
        whisklty = 2, # sets the line type of the whiskers
        col = "lightblue", # sets the color of the box and whiskers
        ylim = c(0, 800000)
)

# Getting the summary of saleprice column to know quartile values
summary(temp_house_data$SalePrice)

#Finding Inter Quartile Range(IQR) to remove outliers

IQR_SalePrice =  214000 - 129975

lower= 214000 -1.5*IQR_SalePrice
upper = 129975 + 1.5*IQR_SalePrice



#Removing outliers
temp_new= subset(temp_house_data, SalePrice >lower & SalePrice < upper)

boxplot(temp_new$SalePrice, main = "SalePrice Boxplot",
        boxwex = 0.5, # sets the width of the box
        whisklty = 2, # sets the line type of the whiskers
        col = "lightblue", # sets the color of the box and whiskers
        ylim = c(0, 800000)
)


#DATA CLEANING

###Dealing with Missing values

#imputing the LotFrontage with the MEDIAN value

house_data_new$LotFrontage[which(is.na(house_data_new$LotFrontage))] <- median(house_data_new$LotFrontage,na.rm = T)


#Imputing the MasVnrArea with MEAN value

house_data_new$MasVnrArea[which(is.na(house_data_new$MasVnrArea))] <- mean(house_data_new$MasVnrArea,na.rm=T)

#imputing the character variables as given in data description and converting them as factor variables
house_data_new$BsmtQual[which(is.na(house_data_new$BsmtQual))] <- "No Basement"
house_data_new$BsmtQual <- as.factor(house_data_new$BsmtQual)

house_data_new$BsmtCond [which(is.na(house_data_new$BsmtCond ))] <- "No Basement"
house_data_new$BsmtCond  <- as.factor(house_data_new$BsmtCond)


house_data_new$GarageType [which(is.na(house_data_new$GarageType))] <- "No Garage"
house_data_new$GarageType  <- as.factor(house_data_new$GarageType)


house_data_new$GarageCond [which(is.na(house_data_new$GarageCond))] <- "No Garage"
house_data_new$GarageCond <- as.factor(house_data_new$GarageCond)

#checking the structure of the dataset
str(house_data_new)


#converting all Character variable to Factor variable in the dataframe

cleaned_data <- house_data_new %>%
  mutate_if(sapply(house_data_new, is.character), as.factor)

str(cleaned_data)

#checking for missing values after imputation of all missing values
missing_cols<-names(which(colSums(is.na(cleaned_data)) > 0))
missing_cols

print("Percentage of Missing Values")
colMeans(is.na(cleaned_data))*100

#assigning the cleaned_data(without any missing values) to another dataframe.
new_cleaned_data2 <- cleaned_data
#############Question 2


#Dividing the houses based on the Overall condition of the house into Poor,Good, Average 
#based on the conditions
cleaned_house_data <- new_cleaned_data2 %>%
  mutate(New_cond= case_when(
    new_cleaned_data2$OverallCond>=1 & new_cleaned_data2$OverallCond<=3 ~ "Poor", 
    new_cleaned_data2$OverallCond>=4 & new_cleaned_data2$OverallCond<=6~ "Average", 
    new_cleaned_data2$OverallCond>=7 & new_cleaned_data2$OverallCond<=10 ~ "Good"))
cleaned_house_data

str(cleaned_house_data)

#getting the newly created column New_cond which has created based on the overallcondition of house as a factor variable
cleaned_house_data$New_cond <- as.factor(cleaned_house_data$New_cond)

#Removing the Id column
cleaned_house_data <- cleaned_house_data[,-1]
#Splitting the dataset
# Set the random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets into 80 to 20 set using caret package
trainIndex <- createDataPartition(cleaned_house_data$New_cond, p = 0.8, list = FALSE)
train <- cleaned_house_data[trainIndex, ]
test <- cleaned_house_data[-trainIndex, ]

# Fit a multinomial logistic regression model to the training data
model <- multinom(New_cond ~ ., data = train, family = binomial)
summary(model)

mlr_pred <- predict(model, newdata = test)
head(mlr_pred)
head(train$New_cond)
mean(mlr_pred == test$New_cond)


# Create a confusion matrix
conf_matrix <- confusionMatrix(mlr_pred, test$New_cond,mode="everything")

# Predict the class probabilities for the test data
mlr_prob <- predict(model, newdata = test, type = "probs")

# Calculate the accuracy
acc <- conf_matrix$overall["Accuracy"]
library(pROC)

# Calculate the F1 score
f1 <- conf_matrix$byClass[,"F1"]

# Calculate the AUC-ROC for each category
auc <- sapply(levels(test$New_cond), function(x) {
  roc(test$New_cond == x, mlr_prob[, x])$auc
})

# Combine the metrics into a table
metrics <- data.frame(Category = levels(test$New_cond),
                      Accuracy = round(acc, 3),
                      F1_score = round(f1, 3),
                      AUC_ROC = round(auc, 3))
print(metrics)

library(pROC)
install.packages("pROC")
###Naive bayes

NB_model = naiveBayes(New_cond ~ ., data = train)
NB_model     # The Y values are the means and standard deviations of the predictors within each class.
NB_model.predicted = predict(NB_model,type="class", newdata = test)
NB_prob =predict(model, newdata = test, type = "probs")

# confusion matrix
conf_matrix<-table(NB_model.predicted, test$New_cond)
mean(NB_model.predicted== test$New_cond)

accuracy <- (conf_matrix[1,1] + conf_matrix[2,2] + conf_matrix[3,3]) / sum(conf_matrix)
precision <- conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[2,1] + conf_matrix[3,1])
recall <- conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[1,2] + conf_matrix[1,3])
f1_score <- 2 * (precision * recall) / (precision + recall)
auc_roc <-multiclass.roc(test$New_cond, NB_prob)


# Print results in a table
result_table <- data.frame(metric = c("Accuracy", "Precision", "Recall", "F1 Score","AUC_ROC"), 
                           value = c(accuracy, precision, recall, f1_score,auc_roc$auc))

result_table




#######################Question 3a)

#Predicting the SalePrice of house using Linear Regression

#filtering some of the data to fit linear regression model

# removing unused factor levels from the column/ removing the unused column

cleaned_house_data<- new_cleaned_data2

#removing the columns which are very less dependent on the target variable
cleaned_house_data = subset(cleaned_house_data, select = -c(Utilities))

cleaned_house_data = subset(cleaned_house_data, select = -c(Condition2))

#Removing the lease used factor levels in the data 
cleaned_house_data <- cleaned_house_data %>%
  filter(Exterior1st!='AsphShn' & Exterior1st!='CBlock') %>% 
  filter(Exterior1st!='ImStucc')%>% filter(Functional!='Sev')%>%filter(Heating!='Floor' & Heating!='OthW')%>%
  filter(ExterCond!='Po')%>%filter(BsmtCond!='Po')%>%filter(Condition1!='RRNe')

#splitting the dataset for training the model and testing the model
set.seed(123)
train_index <- createDataPartition(cleaned_house_data$SalePrice, p = 0.8, list = FALSE)
train_data <- cleaned_house_data[train_index, ]
test_data <- cleaned_house_data[-train_index, ]

# Preprocessing the data using center and scale method to get better results
pre_proces<- preProcess(train_data, method = c("center", "scale"))
train_data <- predict(pre_proces, train_data)
test_data <- predict(pre_proces, test_data)


library("ggplot2")


# Fit a linear regression model
lm_model <- lm(SalePrice ~ ., data = train_data)

# Evaluate the model
predictions <- predict(lm_model, newdata = test_data)
mse <- mean((test_data$SalePrice - predictions)^2)
rmse <- sqrt(mse)
r_squared <- cor(predictions, test_data$SalePrice)^2

# Print the results
cat("Mean squared error: ", mse, "\n")
cat("R-squared value: ", r_squared, "\n")


test_data <- test_data %>% 
  mutate(diff = predictions - SalePrice)


ggplot(test_data, aes(x = predictions, y = SalePrice, color = diff)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_color_gradient2(low = "red", mid = "blue", high = "green", midpoint = 0) +
  labs(x = "Predicted SalePrice", y = "Actual SalePrice", color = "Difference") +
  theme_classic()

##Random forest
library(caret)
set.seed(123)
#splitting the dataset into tarin and test 

train_index <- createDataPartition(cleaned_house_data$SalePrice, p = 0.8, list = FALSE)
training <- cleaned_house_data[train_index, ]
testing <- cleaned_house_data[-train_index, ]
library(randomForest)

#Training the model using house_data 
house_model <- randomForest(SalePrice~.,
                            data = training)
# Predict using the test set

prediction <- predict(house_model,testing)

# Evaluating the value root mean square(RMSE)

x <- prediction
y <- testing$SalePrice
a <- sqrt(sum((log(x)-log(y))^2)/length(y))
rmse <- round(a, digits = 5)
rmse
#calculating Mean absolute error 
MAE <- mean(abs(prediction - testing$SalePrice))
MAE
#calculating sum of squares of residuals
res <- sum((testing$SalePrice - prediction)^2)
#calculating total sum of price (difference b/w saleprice and mean of saleprice)
tot <- sum((testing$SalePrice - mean(testing$SalePrice))^2)
#calculating the r2 value which always lies in o to 1.
R2 <- 1 - (res / tot)
R2
cat("Root mean squared error: ", rmse, "\n")
cat("R-squared value: ", R2, "\n")

########################Question 3b)
library(ipred)
###Applying Re-sampling Methods to calculate the Misclassification error.

#defining a function that uses predict function inside
mypredict <- function(object, newdata)
  predict(object, newdata = newdata, type = c("response"))

#calculate the misclassification error using cross fold validation for randomForest algorithm used 
#to predict saleprice
cv_result<-errorest(SalePrice ~ ., data=cleaned_house_data, model = randomForest,
                    estimator = "cv", predict= mypredict)
cv_result
summary(cv_result)


#Calculating the Miscalssification error using BootStrapping for randomForest algorithm used 
#to predict SalePrice
boot_result <- errorest(SalePrice ~ ., data=cleaned_house_data, model = randomForest,
                        estimator = "boot", predict= mypredict)
boot_result



###################################
# Question 4

#Predicting the size of the living area based on sale_price of the house using 

#splitting the size of the train and test dataset 
set.seed(100)
trainIndex <- createDataPartition(cleaned_house_data$GrLivArea, p = 0.8, list = FALSE)
train <- house_data[trainIndex, ]
test <- house_data[-trainIndex, ]

# Fit a random forest model to predict living area based on the parameters which are realted to the
#target varaible

names(house_data_new)

model <- lm(GrLivArea ~ SalePrice+LotArea+FullBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+OverallQual+X2ndFlrSF
            , data = train)

# Predict living area for the test set based on the model
predicted_living_area <- predict(model, newdata = test)

# Compute the mean squared error
mse <- mean((predicted_living_area - test$GrLivArea)^2)

# Print the mean squared error
print(paste("Mean squared error:", mse))
r_squared <- cor(predicted_living_area, test$GrLivArea)^2

# Print the results
cat("Mean squared error: ", mse, "\n")
cat("R-squared value: ", r_squared, "\n")





