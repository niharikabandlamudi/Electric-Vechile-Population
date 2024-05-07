#Libraries required
library(MASS)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(car)
library(dplyr)
library(tidyr)  
library(caret)  
library(glmnet)
library(pROC)
library(class)
library(viridisLite)
library(viridis)
library(e1071)  # For SVM model
library(nnet)
library(randomForest)
library(rpart)
install.packages("ROSE")
library(ROSE)
install.packages("irr")
library(irr)

#Read data
EV = read.csv('C:/Users/nihar/OneDrive/Desktop/Data Analytics Applications/Project/Electric_Vehicle_Population_Data.csv')

head(EV)
str(EV)

summary(EV)

# Check how many NA'a are there
colSums(is.na(EV))

# Next, we check for duplicate rows in the dataset

any(duplicated(EV))

#Count the number of unique values in each column of the dataset
EV_unique <- summarize_all(EV, n_distinct)
EV_unique

# Updating column names
colnames(EV)[colnames(EV) == "VIN..1.10."] <- "VIN"
colnames(EV)[colnames(EV) == "Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility"] <- "CAFV_Eligibility"
colnames(EV)[colnames(EV) == "Model.Year"] <- "Model_Year"
colnames(EV)[colnames(EV) == "Electric.Vehicle.Type"] <- "Electric_Vehicle_Type"
colnames(EV)[colnames(EV) == "Electric.Range"] <- "Electric_Range"
colnames(EV)[colnames(EV) == "Postal.Code"] <- "Postal_Code"
colnames(EV)[colnames(EV) == "Base.MSRP"] <- "Base_MSRP"
colnames(EV)[colnames(EV) == "Legislative.District"] <- "Legislative_District"
colnames(EV)[colnames(EV) == "DOL.Vehicle.ID"] <- "DOL_Vehicle_ID"
colnames(EV)[colnames(EV) == "Electric.Utility"] <- "Electric_Utility"
colnames(EV)[colnames(EV) == "X2020.Census.Tract"] <- "X2020_Census_Tract"
colnames(EV)[colnames(EV) == "Vehicle.Location"] <- "Vehicle_Location"


# Creating a new cleaned dataframe
Evechile <- EV

# Data preprocessing
# Assuming some columns are irrelevant for this analysis, we'll focus on key columns
Ev_electric <- Evechile %>%
  select(VIN, County, City, Model_Year, Make, Model, Electric_Vehicle_Type, CAFV_Eligibility, Electric_Range, State, Electric_Utility, Vehicle_Location,Legislative_District) %>%
  filter(!is.na(CAFV_Eligibility)& CAFV_Eligibility!= "",
         !is.na(County) & County != "",
         !is.na(City) & City != "")  # Remove rows with missing CAFV eligibility
print(colnames(Ev_electric))

# Check how many NA'a are there and remove it
na.omit(Ev_electric$Electric_Vehicle_Type)
na.omit(Ev_electric$Model)


Ev_electric$Electric_Range <- ifelse(Ev_electric$Electric_Range != 0, Ev_electric$Electric_Range, NA)
#Ev_electric_sub <- subset(Ev_electric, Electric_Range != 0)
Ev_electric

Ev_electric <- na.omit(Ev_electric)
str(Ev_electric)
colSums(is.na(Ev_electric))
Ev_electric$Legislative_District <- factor(Ev_electric$Legislative_District)


# Convert CAFV_Eligibility to numeric using dummy encoding
Ev_electric <- Ev_electric %>%
 mutate(CAFV_Eligibility = as.factor(CAFV_Eligibility)) %>%
 select(-VIN)  # Remove VIN column for simplicity

# Create a new variable with values 1 and 2 based on Electric_Vehicle_Type 
Ev_electric$EV_Type_Num <- ifelse(Ev_electric$Electric_Vehicle_Type == "Plug-in Hybrid Electric Vehicle (PHEV)", 1,
                                  ifelse(Ev_electric$Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)", 2, NA))

Ev_electric$EV_Type_Num <- factor(Ev_electric$EV_Type_Num)

# Define custom levels for CAFV_Eligibility
# Create a new variable with values 1 and 2 based on CAFV_Eligibility
Ev_electric$CAFV_Eligibility_Num <- ifelse(Ev_electric$CAFV_Eligibility == "Clean Alternative Fuel Vehicle Eligible", 1,
                                           ifelse(Ev_electric$CAFV_Eligibility == "Not eligible due to low battery range", 2, NA))
Ev_electric$CAFV_Eligibility_Num <- factor(Ev_electric$CAFV_Eligibility_Num)
colSums(is.na(Ev_electric))
# Exploratory data analysis
# Calculate average electric range by make
# Grouping the data by 'Make' and calculating the average 'Electric Range'
average_range_by_make <- Ev_electric %>%
  group_by(Make) %>%
  summarize(Average_Electric_Range = mean(`Electric_Range`)) %>%
  arrange(desc(Average_Electric_Range))

# Visualizing the top 10 manufacturers by average electric range
ggplot(head(average_range_by_make, 10), aes(x=Make, y=Average_Electric_Range, fill=Make)) +
  geom_bar(stat="identity") +
  labs(title="Top 10 Manufacturers by Average Electric Range",
       x="Make",
       y="Average Electric Range (miles)") +
  scale_fill_viridis(discrete=TRUE) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#What are the Top 10 counts of cars per City and per County?

top_cities <- Ev_electric %>%
  group_by(City) %>%
  summarise(total_cars = n()) %>%
  arrange(desc(total_cars)) %>%
  top_n(10)

# Top 10 counties with the highest number of EV's
top_counties <- Ev_electric %>%
  group_by(County) %>%
  summarise(total_cars = n()) %>%
  arrange(desc(total_cars)) %>%
  top_n(10)

# Bar graph for top 10 cities
ggplot(top_cities, aes(x = City, y = total_cars, fill = City, label = total_cars)) +
  geom_bar(stat = "identity") +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Top 10 Cities with the Highest Number of EV's",
       x = "City", y = "Number of Cars") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.3),
        panel.grid.major = element_line(colour = "gray", linetype = "dotted"))

# Bar graph for top 10 counties
ggplot(top_counties, aes(x = County, y = total_cars, fill = County, label = total_cars)) +
  geom_bar(stat = "identity") +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Top 10 Counties with the Highest Number of EV's",
       x = "County", y = "Number of Cars") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.3),
        panel.grid.major = element_line(colour = "gray", linetype = "dotted"))

# In which Year Electric Vehicles increased?
# electric vehicle adoption trend by Model Year
# calculate no.of vechiles registered each year
ev_trend <- table(Ev_electric$Model_Year)
ev_trend

#plot the trend over the years
plot(ev_trend, type='o', col='purple', pch=16, xlab='Model year',
     ylab='Number of Electric vehicles registered',
     main='Electric Vehicle Trend by Model Year')


# What are the Top 5 vs Bottom 5 Comparison?
# Calculate the frequency of each EV model in the dataset
ev_model_freq <- Ev_electric %>%
  group_by(Model) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
# Identify the top 5 and bottom 5 EV models
top_5_models <- head(ev_model_freq, 5)
bottom_5_models <- tail(ev_model_freq, 5)
# Combine the top 5 and bottom 5 models into a single data frame
top_bottom_models <- bind_rows(top_5_models, bottom_5_models)

# Create a new column to indicate if the model is in the top 5 or bottom 5
top_bottom_models$Group <- ifelse(top_bottom_models$Model %in% top_5_models$Model, "Top 5", "Bottom 5")

# Convert the Group column to a factor with specific levels
top_bottom_models$Group <- factor(top_bottom_models$Group, levels = c("Top 5", "Bottom 5"))

# Print the top and bottom models with their groups
print(top_bottom_models)

# Create a bar chart to compare adoption rates
ggplot(top_bottom_models, aes(x = Model, y = Count, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 vs Bottom 5 EV Models Based on Adoption Rates",
       x = "EV Model", y = "Number of Vehicles") +
  scale_fill_manual(values = c("Top 5" = "steelblue", "Bottom 5" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  facet_wrap(~Group, scales = "free_x", ncol = 2)
#########################
# Create the contingency table
contingency_table <- table(Ev_electric$Model, Ev_electric$CAFV_Eligibility_Num)

# Print the contingency table
print("Contingency Table:")
print(contingency_table)

# Perform Monte Carlo simulation for Fisher's exact test
fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)


# Print the result
print("Fisher's Exact Test:")
print(fisher_result)
###############
# Are there demographic patterns that influence EV adoption?
#chisq_result <- chisq.test(Ev_electric$Model, Ev_electric$CAFV_Eligibility_Num)

#Print the chi-square test result
#print(chisq_result)

# Regression analysis for quantifying the impact of demographic factors on EV adoption rates
# Assuming 'Demographic_Variable2' is a demographic variable you want to analyze
# Fit the multinomial logistic regression model
regression_model <- multinom(CAFV_Eligibility_Num ~ Model, data = Ev_electric)

# Summarize the model
summary(regression_model)

# What is the distribution of EV types (eg: batter electric vehicle, plug-in hybrid electric vehicle) in the dataset and how did it evolve overtime?
#Vehicle Type Distribution
# Set the theme for ggplot
theme_set(theme_minimal())

# Plotting the distribution of electric vehicle types over time separately
ggplot(Ev_electric, aes(x=`Model_Year`, fill=`Electric_Vehicle_Type`)) +
  geom_bar(position="stack", stat="count") +
  labs(title="Distribution of Electric Vehicle Types Over Time",
       x="Model Year",
       y="Number of Vehicles") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~ Electric_Vehicle_Type, scales = "free_y")

# Is there any relationship between range anxiety and EV adoption?

# Perform regression analysis
regression_model <- multinom(CAFV_Eligibility ~ Electric_Range + EV_Type_Num, data = Ev_electric)


# Summary of the regression model
summary(regression_model)

avg_range_year <- tapply(Ev_electric$Electric_Range, Ev_electric$Model_Year, mean)
avg_range_year
#plotting the average electric range over the years
plot(names(avg_range_year),avg_range_year, type='o', col='purple', pch=16, xlab='Model Year', 
     ylab='Average Electric Range(miles)', main='Avg Electric Range by Model Year')

# Are certain EV models preferred over others by consumers?
Ev_model <- table(Ev_electric$Model)

print(head(Ev_model[order(Ev_model, decreasing = TRUE)], 10))

barplot(head(Ev_model[order(Ev_model, decreasing = TRUE)], 10), main = "Top 10 Popular Ev Models", 
        xlab = "Model", ylab = "registration count", col = "pink", las=2, cex.names = 0.7)
##########################################################
Ev_electric <- Ev_electric[, -which(names(Ev_electric) == "State")]
Ev_electric <- Ev_electric[, -which(names(Ev_electric) == "City")]


# Split the data into training and testing sets

set.seed(1)
tr_ind=sample(nrow(Ev_electric),0.7*nrow(Ev_electric), replace = F) #without replacement sampling
EVtrain=Ev_electric[tr_ind,]#this will give indexes of heart where i am going to sample 70% no.of rows of heart
EVtest=Ev_electric[-tr_ind,]#
colSums(is.na(EVtrain))
colSums(is.na(EVtest))

# Define cross-validation control
ctrl <- trainControl(method = "cv", number = 5)

# Multinomial Logistic Regression

model_multinom <- train(CAFV_Eligibility_Num ~ Electric_Range + EV_Type_Num + Legislative_District  , data = EVtrain,
                        method = "multinom", trControl = ctrl, tuneLength = 1)

predictions_multinom <- predict(model_multinom, newdata = EVtest)

# Calculate accuracy
accuracy_multinom <- mean(predictions_multinom == EVtest$CAFV_Eligibility_Num)
cat("Multinomial Logistic Regression Accuracy:", accuracy_multinom)

# Calculate confusion matrix

conf_matrix_ml <- confusionMatrix(predictions_multinom, EVtest$CAFV_Eligibility_Num)
conf_matrix_ml

precision_ml <- posPredValue(predictions_multinom, EVtest$CAFV_Eligibility_Num, positive = "1")
recall_ml <- sensitivity(predictions_multinom, EVtest$CAFV_Eligibility_Num, positive = "1")
f1_ml <- 2 * (precision_ml * recall_ml) / (precision_ml + recall_ml)

precision_ml
recall_ml
f1_ml
# ROC curve and AUC
roc_multinom <- roc(EVtest$CAFV_Eligibility_Num, as.numeric(predictions_multinom))
plot(roc_multinom, main = "ROC Curve - Multinomial Logistic Regression", col = "blue")

# Decision Tree
set.seed(123)  
model_dt <- train(CAFV_Eligibility_Num ~ Electric_Range + EV_Type_Num + Legislative_District, data = EVtrain,
                  method = "rpart", trControl = ctrl, tuneLength = 1)
predictions_dt <- predict(model_dt, newdata = EVtest)

# Calculate accuracy
accuracy_dt <- mean(predictions_dt == EVtest$CAFV_Eligibility_Num)
cat("Decision Tree Accuracy:", accuracy_dt, "\n")

# Calculate confusion matrix
conf_matrix_dt <- confusionMatrix(predictions_dt, EVtest$CAFV_Eligibility_Num)
conf_matrix_dt

precision_dt <- posPredValue(predictions_dt, EVtest$CAFV_Eligibility_Num, positive = "1")
recall_dt <- sensitivity(predictions_dt, EVtest$CAFV_Eligibility_Num, positive = "1")
f1_dt <- 2 * (precision_dt * recall_dt) / (precision_dt + recall_dt)

precision_dt
recall_dt
f1_dt

# ROC curve 
roc_dt <- roc(as.numeric(EVtest$CAFV_Eligibility_Num), as.numeric(predictions_dt))

# Plot the ROC curve
plot(roc_dt, main = "ROC Curve - Decision Tree Model", col = "red")

#svm
set.seed(123)  
model_svm <- train(CAFV_Eligibility_Num ~ Electric_Range + EV_Type_Num + Legislative_District, data = EVtrain,
                   method = "svmLinear", trControl = ctrl)  # Use linear SVM for simplicity
predictions_svm <- predict(model_svm, newdata = EVtest)

# Calculate accuracy
accuracy_svm <- mean(predictions_svm == EVtest$CAFV_Eligibility_Num)
cat("SVM Accuracy:", accuracy_svm, "\n")

# Calculate confusion matrix
conf_matrix_svm <- confusionMatrix(predictions_svm, EVtest$CAFV_Eligibility_Num)
conf_matrix_svm

precision_svm <- posPredValue(predictions_svm, EVtest$CAFV_Eligibility_Num, positive = "1")
recall_svm <- sensitivity(predictions_svm, EVtest$CAFV_Eligibility_Num, positive = "1")
f1_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)

precision_svm
recall_svm
f1_svm

# ROC curve 
roc_svm <- roc(EVtest$CAFV_Eligibility_Num, as.numeric(predictions_svm))

# Plot the ROC curve
plot(roc_svm, main = "ROC Curve - SVM Model", col = "green")

###########################################
set.seed(125)
# Balancing the dataset for multinomial logistic regression
EVtrain_balanced_ml <- ovun.sample(CAFV_Eligibility_Num ~ ., data = EVtrain, method = "over", N = length(which(EVtrain$CAFV_Eligibility_Num == 1))*2, seed = 1234)$data

# Fit the multinomial logistic regression model on the balanced dataset
model_multinom_balanced <- train(CAFV_Eligibility_Num ~ Electric_Range + EV_Type_Num + Legislative_District, data = EVtrain_balanced_ml,
                                 method = "multinom", trControl = ctrl, tuneLength = 1)

predictions_ml_bl <- predict(model_multinom_balanced, newdata = EVtest)

# Calculate accuracy
accuracy_multinom_bl <- mean(predictions_ml_bl == EVtest$CAFV_Eligibility_Num)
cat("Multinomial Logistic Regression Accuracy:", accuracy_multinom_bl)

# Calculate confusion matrix

conf_matrix_ml_bl <- confusionMatrix(predictions_ml_bl, EVtest$CAFV_Eligibility_Num)
conf_matrix_ml_bl

precision_ml_bl <- posPredValue(predictions_ml_bl, EVtest$CAFV_Eligibility_Num, positive = "1")
recall_ml_bl <- sensitivity(predictions_ml_bl, EVtest$CAFV_Eligibility_Num, positive = "1")
f1_ml_bl <- 2 * (precision_ml_bl * recall_ml_bl) / (precision_ml_bl + recall_ml_bl)

precision_ml_bl
recall_ml_bl
f1_ml_bl

# ROC curve and AUC
roc_multinom_bl <- roc(EVtest$CAFV_Eligibility_Num, as.numeric(predictions_ml_bl))
plot(roc_multinom_bl, main = "ROC Curve - Multinomial Logistic Regression", col = "blue")

# Fit the decision tree model on the balanced dataset
model_dt_balanced <- train(CAFV_Eligibility_Num ~ Electric_Range + EV_Type_Num + Legislative_District, data = EVtrain_balanced_ml,
                           method = "rpart", trControl = ctrl, tuneLength = 1)

predictions_dt_bl <- predict(model_dt_balanced, newdata = EVtest)

# Calculate accuracy
accuracy_dt_bl <- mean(predictions_dt_bl == EVtest$CAFV_Eligibility_Num)
cat("Decision Tree Accuracy:", accuracy_dt_bl, "\n")

# Calculate confusion matrix
conf_matrix_dt_bl <- confusionMatrix(predictions_dt_bl, EVtest$CAFV_Eligibility_Num)
conf_matrix_dt_bl

precision_dt_bl <- posPredValue(predictions_dt_bl, EVtest$CAFV_Eligibility_Num, positive = "1")
recall_dt_bl <- sensitivity(predictions_dt_bl, EVtest$CAFV_Eligibility_Num, positive = "1")
f1_dt_bl <- 2 * (precision_dt_bl * recall_dt_bl) / (precision_dt_bl + recall_dt_bl)

precision_dt_bl
recall_dt_bl
f1_dt_bl

# ROC curve 
roc_dt_bl <- roc(as.numeric(EVtest$CAFV_Eligibility_Num), as.numeric(predictions_dt_bl))

# Plot the ROC curve
plot(roc_dt_bl, main = "ROC Curve - Decision Tree Model", col = "red")

# Fit the SVM model on the balanced dataset
model_svm_balanced <- train(CAFV_Eligibility_Num ~ Electric_Range + EV_Type_Num + Legislative_District, data = EVtrain_balanced_ml,
                            method = "svmLinear", trControl = ctrl)
predictions_svm_bl <- predict(model_svm_balanced, newdata = EVtest)

# Calculate accuracy
accuracy_svm_bl <- mean(predictions_svm_bl == EVtest$CAFV_Eligibility_Num)
cat("SVM Accuracy:", accuracy_svm_bl, "\n")

# Calculate confusion matrix
conf_matrix_svm_bl <- confusionMatrix(predictions_svm_bl, EVtest$CAFV_Eligibility_Num)
conf_matrix_svm_bl

precision_svm_bl <- posPredValue(predictions_svm_bl, EVtest$CAFV_Eligibility_Num, positive = "1")
recall_svm_bl <- sensitivity(predictions_svm_bl, EVtest$CAFV_Eligibility_Num, positive = "1")
f1_svm_bl <- 2 * (precision_svm_bl * recall_svm_bl) / (precision_svm_bl + recall_svm_bl)

precision_svm_bl
recall_svm_bl
f1_svm_bl

# ROC curve 
roc_svm_bl <- roc(EVtest$CAFV_Eligibility_Num, as.numeric(predictions_svm_bl))

# Plot the ROC curve
plot(roc_svm_bl, main = "ROC Curve - SVM Model", col = "green")
