# RUN install.packages(c("ggplot2", "ggthemes", "tidyverse", "gridExtra", "corrplot", "randomForest", "caret", "MLmetrics"))
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(readr)
library(dplyr)
library(gridExtra)
library(corrplot)
library(randomForest)
library(caret)
library(MLmetrics)

data <- read_csv("onlinefoods.csv") # load the dataset



data <- select(data, -...13) # removes a column that the original creator of the dataset says is an error
data <- rename(data, `Ordered Again` = Output) # creator of dataset mistaken the name of this column
data <- data |> mutate(`Monthly Income` = factor(`Monthly Income`, levels = c("No Income", "Below Rs.10000", "10001 to 25000", "25001 to 50000", "More than 50000"), ordered = TRUE))
data <- data |> mutate(`Family size` = factor(`Family size`, levels = c(1, 2, 3, 4, 5, 6), ordered=TRUE))
data <- data |> mutate(`Educational Qualifications` = factor(`Educational Qualifications`, levels = c("Uneducated", "School", "Graduate", "Post Graduate", "Ph.D"), ordered=TRUE))
head(data, 10)
str(data) #displays all variables in the data, along with their class


################################
#Basic Demographic Analysis

# pie chart showcasing the gender demographic among the customers
gender_count <- data |> group_by(Gender) |> summarize(count = n())
gender_count <- mutate(gender_count, percentage = round(count / sum(count) * 100, 2))
pie_chart <- gender_count |> ggplot(aes(x="", y = count, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  labs(title = "Pie Chart with ggplot2", fill = "Gender") + 
  geom_text(aes(label = paste(count, " (", percentage, "%)", sep="")), position = position_stack(vjust = 0.5)) 
pie_chart

# Bar graphs for different variables (Marital Status, Occupation, Monthly Income, Family size, Educational Qualifications)
# Marital Status Demographic in Proportions
marital_status_numbers <- data |> group_by(`Marital Status`) |> 
  summarize(count = n()) |> 
  mutate(proportion = round(count / sum(count), 2)) |> arrange(desc(proportion))
bar_graph_1 <- marital_status_numbers |> 
  ggplot(aes(x = `Marital Status`, y = proportion, fill = `Marital Status`)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = count), vjust = 0, color = "black") + scale_y_continuous(limit = c(0, 0.7)) +
  labs(x = "Marital Status", y = "Proportion", title = "Marital Status Demographic Among Customers") +
  theme_classic()

bar_graph_1

# Occupation Demographic in Occupation
occupation_numbers <- data |> group_by(Occupation) |> summarize(count = n()) |> mutate(proportion = round(count / sum(count), 2)) |> arrange(desc(proportion))
bar_graph_2 <- occupation_numbers |> 
  ggplot(aes(x = Occupation, y = proportion, fill = Occupation)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = count), vjust = 0, color = "black") + scale_y_continuous(limit = c(0, 0.7)) +
  labs(x = "Occupation", y = "Proportion", title = "Occupation Demographic Among Customers") +
  theme_classic()

bar_graph_2

# Monthly Income Demographic in Monthly Income
income_numbers <- data |> group_by(`Monthly Income`) |> summarize(count = n()) |> mutate(proportion = round(count / sum(count), 2)) 
bar_graph_3 <- income_numbers |> 
  ggplot(aes(x = `Monthly Income`, y = proportion, fill = `Monthly Income`)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = count), vjust = 0, color = "black") + scale_y_continuous(limit = c(0, 0.7)) +
  labs(x = "Monthly Income", y = "Proportion", title = "Monthly Income Demographic Among Customers") +
  theme_classic()

bar_graph_3

# Family Size Demographic in Family Size
family_size_numbers <- data |> group_by(`Family size`) |> summarize(count = n()) |> mutate(proportion = round(count / sum(count), 2)) 
bar_graph_4 <- family_size_numbers |> 
  ggplot(aes(x = `Family size`, y = proportion, fill = `Family size`)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = count), vjust = 0, color = "black") + scale_y_continuous(limit = c(0, 0.7)) +
  labs(x = "Family Size", y = "Proportion", title = "Family Size Demographic Among Customers") +
  theme_classic()

bar_graph_4

# Educational Qualifications Demographic in Educational Qualifications
educational_qualifications_numbers <- data |> group_by(`Educational Qualifications`) |> summarize(count = n()) |> mutate(proportion = round(count / sum(count), 2)) 
bar_graph_5 <- educational_qualifications_numbers |> 
  ggplot(aes(x = `Educational Qualifications`, y = proportion, fill = `Educational Qualifications`)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = count), vjust = 0, color = "black") + scale_y_continuous(limit = c(0, 0.7)) +
  labs(x = "Educational Qualifications", y = "Proportion", title = "Educational Qualification Demographic Among Customers") +
  theme_classic()

bar_graph_5

grid.arrange(bar_graph_2, bar_graph_3, bar_graph_4, bar_graph_5, ncol = 2)

# Set the y-axis of the different bar graphs to be of the same scale



#######################
# Age Count Histogram
age_histogram <- data |> ggplot(aes(x = Age, fill = Gender)) +
  geom_histogram(position = "stack", binwidth = 2, color = "white") +
  labs(x = "Age", y = "Frequency", title = "Age Distribution of Customers") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  theme_classic()
age_histogram # Instead of stack, we can overlap male, female, and all genders

#Violin Plot of Age Vs Monthly Income
violin_plot <- data |> group_by(Gender) |> ggplot(aes(x = `Monthly Income`, y = Age, fill = Gender)) +
  geom_violin() + 
  labs(x = "Monthly Income", y = "Age", title = "Age Distribution by Monthly Income") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) 
violin_plot

####################### 
# Basic Feedback Analysis
feedbacks <- data |> group_by(Feedback) |> summarize(count = n()) |> mutate(percentage = round(count/sum(count) * 100, 2))
feedback_bar_graph <- feedbacks |> ggplot(aes(x = Feedback, y = count, fill = Feedback)) +
  geom_bar(stat = "identity") +  
  labs(x = "Feedback", y = "Frequency") + scale_y_continuous(limit = c(0, sum(feedbacks$count))) + 
  geom_text(aes(label = paste(percentage, "%", sep="")), vjust = -1) + 
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
  theme_classic()
feedback_bar_graph

# Feedback by Monthly Income
feedback_by_monthlyincome_graph <- data |> group_by(Feedback) |> ggplot(aes(x = `Monthly Income`, fill = Feedback)) +
  geom_bar(position = "dodge") +
  labs(x = "Monthly Income", y = "Number of Feedbacks", title = "Monthly Income & Feedback Analysis") + 
  scale_y_continuous(limit = c(0, sum(feedbacks$count) / 2)) +
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
  theme_classic()
feedback_by_monthlyincome_graph


########################
# Feedback Vs Ordered-Again Ratio
data_used <- data |> group_by(Feedback, `Ordered Again`) |> summarize(count = n(), .groups = 'drop') |> 
  group_by(Feedback) |> mutate(percentage = round((count / sum(count)) * 100, 1))
data_used <- data_used |> arrange(Feedback, desc(`Ordered Again`)) |> group_by(Feedback) |> mutate(label_position = cumsum(count) - (0.5* count))
feedback_v_orderedagain <- data_used |> ggplot(aes(x = Feedback, y = count, fill = `Ordered Again`)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste(percentage, "%"), y = label_position), vjust = 0.5) +
  labs(x = "Feedback", y = "Frequency", title = "Feedback - Order Again Relationship") + 
  scale_fill_manual(values = c("Yes" = "green", "No" = "red")) +
  theme_classic()
  
feedback_v_orderedagain

#Ordering Behavior by Monthly Income and Age
monthlyincome_age_orderingbehavior <- data |> ggplot(aes(x = Age, y = `Monthly Income`, fill = `Ordered Again`)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red")) +
  labs(x = "Age", y = "Monthly Income", title = "Order-Again Behavior by Monthly Income and Age", fill = "Ordered Again")
monthlyincome_age_orderingbehavior


#########################

# Correlation Matrix Among Numeric Data
color <- colorRampPalette(c("white", "red"))(100)

correlation_matrix_graph <- data |> mutate(`Family size` = as.numeric(`Family size`)) |> 
  select(Age, `Family size`, latitude, longitude) |> 
  cor() |> corrplot(method = "color", , tl.col = "black", tl.srt = 45, 
                    addCoef.col = "black", col = color, title = "Correlation Matrix between Numeric Variables") 
correlation_matrix_graph


#Correlation Matrix Among Ordinal Data
correlation_matrix_graph2 <- data |>  mutate(`Family size` = as.numeric(`Family size`), `Monthly Income` = as.numeric(`Monthly Income`), 
  `Educational Qualifications` = as.numeric(`Educational Qualifications`)) |>
  select(`Family size`, `Monthly Income`, `Educational Qualifications`) |> cor(method = "kendall") |> 
  corrplot(method = "color", tl.col = "black", tl.cex = 0.8, tl.srt = 45, 
           addCoef.col = "black", col = color, title = "Correlation Matrix between Ordinal Variables") 
correlation_matrix_graph2


###########################

# Random Forest Model
# First, we convert all nominal data to binary values
data <- data |> mutate(`Marital Status` = as.integer(factor(`Marital Status`, levels = c("Prefer not to say", "Single", "Married"))),
                       Gender = as.integer(factor(Gender, levels = c("Male", "Female"))), Occupation = as.integer(factor(Occupation, 
                       levels = c("Employee", "House wife", "Self Employeed", "Student"))), 
                       Feedback = as.integer(factor(Feedback, levels = c("Negative", "Positive")))- 1)
data <- data |> select(-latitude, -longitude, -`Pin code`) 
# removes unnecessary variables - we cannot ascertain the correlation between the location of the customer and whether they will order again, 
# as the dataset does not give us the location of the customer RELATIVE to the restaurant location (This is one flaw of the dataset)
data <- data |> rename(MaritalStatus = `Marital Status`, MonthlyIncome = `Monthly Income`, EducationalQualifications = `Educational Qualifications`, FamilySize = `Family size`)
data$`Ordered Again` <- factor(data$`Ordered Again`, levels = c("Yes", "No"))
#This line of code above is very important - 
# Make note that if the customer did not order again, the equivalent nominal value is 0. If they did order again, the equivalent nominal value is 1.
str(data)

set.seed(2024) # For reproducibility of results
split_data <- createDataPartition(y=1:nrow(data), p = 0.7, list = FALSE) # splits our existing dataset 7:3
train_set <- data[split_data, ] # 70% of our data is used for training our model
test_set <- data[-split_data, ] # 30% of our data is used for testing our model

randomforest_model <- randomForest(`Ordered Again`~ ., data = train_set, ntree = 100)
print(randomforest_model)

predictions <- predict(randomforest_model, test_set) #generate predictions with our test set

cm_yes <- confusionMatrix(predictions, test_set$`Ordered Again`, positive = "Yes") # positive class is Yes
print(cm_yes)

#Extract accuracy, sensitivity, and specificity from the matrix
accuracy_yes <- cm_yes$overall[["Accuracy"]]
sensitivity_yes <- cm_yes$byClass['Sensitivity']
specificity_yes <- cm_yes$byClass['Specificity']

#Generate f1
f1_score <- F_meas(predictions, test_set$`Ordered Again`, positive = "Yes")

summary_table <- pivot_longer(data.frame(`Overall Accuracy` = accuracy_yes, Sensitivity = sensitivity_yes, 
                                         Specificity = specificity_yes, `F1 Score` = f1_score), cols = everything())
print(summary_table)
summary_graph <- summary_table |> ggplot(aes(x = name, y = value)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Metrics", title = "Random Forest Model")
print(summary_graph)

varImpPlot(randomforest_model, main = paste("Variable Importance in Order: "))

# Creating the average customer and predicting whether they will reorder
average_customer <- data.frame(
  Age = median(data$Age),
  MaritalStatus = as.integer(median(as.integer(data$MaritalStatus))),
  Gender = as.integer(median(as.integer(data$Gender))),
  Occupation = as.integer(median(as.integer(data$Occupation))),
  Feedback = as.integer(median(as.integer(data$Feedback))),
  MonthlyIncome = factor(as.integer(median(as.integer(data$MonthlyIncome)))),
  EducationalQualifications = factor(as.integer(median(as.integer(data$EducationalQualifications)))),
  FamilySize = factor(as.integer(median(as.integer(data$FamilySize))))
)
print(average_customer)
# Predicting with the random forest model
average_customer_prediction <- predict(randomforest_model, newdata = average_customer)
print(average_customer_prediction)

# A look into the importance of the different variables in predicting customer reordering behavior

# As can be seen, the specificity is much lower compared to the other metrics

### Improving Accuracy, Sensitivity and Specificity of the model

## Method Cross-Validation & Emphasizing on Optimizing Specificity
train_control <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary) # Standard 5-fold cross valdiation

# Train the Random Forest model with cross-validation this time
randomforest_model_cv <- train(`Ordered Again` ~ ., data = train_set, method = "rf", trControl = train_control, metric = "Spec", tuneLength = 20)
# now using train() instead of randomForest() because I find that this function offers greater flexibility for parameter tuning
print(randomforest_model_cv)


predictions_cv <- predict(randomforest_model_cv, newdata = test_set)

# Generate Confusion Matrix
cm_cv <- confusionMatrix(predictions_cv, test_set$`Ordered Again`, positive = "Yes")
print(cm_cv)

accuracy_v2 <- cm_cv$overall[["Accuracy"]]
sensitivity_v2 <- cm_cv$byClass['Sensitivity']
specificity_v2 <- cm_cv$byClass['Specificity']

#Generate f1
f1_score_v2 <- F_meas(predictions_cv, test_set$`Ordered Again`, positive = "Yes")

summary_table_v2 <- pivot_longer(data.frame(`Overall Accuracy` = accuracy_v2, Sensitivity = sensitivity_v2, 
                                            Specificity = specificity_v2, `F1 Score` = f1_score_v2), cols = everything())
print(summary_table_v2)
summary_graph_v2 <- summary_table_v2 |> ggplot(aes(x = name, y = value)) + geom_bar(stat = "identity", fill = "red", alpha = 0.5) + 
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Metrics", title = "Improved Model")
print(summary_graph_v2)

summary_table$Model <- "Original"
summary_table_v2$Model <- "Improved"

# Combine summary tables
combined_summary <- rbind(summary_table, summary_table_v2) |> mutate(Model = factor(Model, levels = c("Original", "Improved")))

# Plot combined summary tables
combined_summary_plot <- ggplot(combined_summary, aes(x = name, y = value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), alpha = 0.5) +
  scale_fill_manual(values = c("Original" = "blue", "Improved" = "red")) +
  labs(title = "Comparison of Original Vs Improved", x = "Metric", y = "Value") +
  theme(legend.position = "top") +
  theme_classic() 


# Print the combined plot
print(combined_summary_plot)




