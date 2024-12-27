library(Hmisc)
library(ggplot2)
library(psych)
library(GGally)
library(DescTools)
library(tidyverse)
library(caret)
library(e1071)
library(rattle)
library(stats)
library(MASS)
library(car)
library(tidyverse)
library(data.table)
library(ROSE)
library(tuneRanger)
library(foreach)
library(doParallel)

raw <- read.csv("/Users/rpravin/Downloads/Crime_Data_from_2010_to_2019_20241122.csv") 

# at glance view of the data
summary(raw)

# Getting rid of Records Number, Location(Street Address of Crime), 
# Cross Street(Cross Street of Rounded Address), Latitude, Longitude
raw_subset <- raw[2:24]

# setting variable types
raw_subset$AREA <- as.factor(raw_subset$AREA)
raw_subset$AREA.NAME <- as.factor(raw_subset$AREA.NAME)
raw_subset$Part.1.2 <- as.factor(raw_subset$Part.1.2)
raw_subset$Vict.Sex <- as.factor(raw_subset$Vict.Sex)
raw_subset$Vict.Descent <- as.factor(raw_subset$Vict.Descent)
raw_subset$Status <- as.factor(raw_subset$Status)
raw_subset$Status.Desc <- as.factor(raw_subset$Status.Desc)

# at glance view of categorical data
table(raw$Status.Desc)

# Re-Coding Response:Status of Case ---------------------------------------
# Setting rows marked as UNK(unclear) to NA
raw_subset$Status.Desc[raw_subset$Status.Desc == "UNK"] <- NA

# Creating new Variable with our proposed Binary Outcome Legal Actions vs. No Legal Action(Status Case)
raw_subset$Legal_Action <- raw_subset$Status.Desc 
raw_subset$Legal_Action <- as.character(raw_subset$Legal_Action)
raw_subset$Legal_Action <- ifelse(raw_subset$Legal_Action == "Invest Cont", 0, 1)

# counts of response variable
table(raw_subset$Legal_Action)

# ReCoding Date & Coding Difference in Report Time ----------------------------------------
raw_subset$date_report <- raw_subset$Date.Rptd
raw_subset$date_report <- as.POSIXct(raw_subset$date_report, format = "%m/%d/%Y %I:%M:%S %p")
raw_subset$date_report <- as.Date(raw_subset$date_report)

# Occurence Date
raw_subset$date_occur <- raw_subset$DATE.OCC
raw_subset$date_occur <- as.POSIXct(raw_subset$date_occur, format = "%m/%d/%Y %I:%M:%S %p")
raw_subset$date_occur <- as.Date(raw_subset$date_occur)

#Creating New Column: Difference Between Report vs Occurence
raw_subset$date_occur_report_difference <- as.numeric(difftime(raw_subset$date_report, 
                                                               raw_subset$date_occur, 
                                                               units = "days"))
# Categorizing Time Occurred --------------------------------------------------
# Convert military time to string
raw_subset$military_time <- raw_subset$TIME.OCC 

#Time Ranges:
# Morning 5 am to 12 pm (noon)
# Afternoon 12 pm to 5 pm.
# Evening 5 pm to 9 pm.
# Night 9 pm to 4 am.
military_times_str <- sprintf("%04d", raw_subset$military_time)

hours <- as.integer(substr(military_times_str, 1, 2))

categories <- ifelse(hours >= 5 & hours < 12, "Morning",
                     ifelse(hours >= 12 & hours < 17, "Afternoon",
                            ifelse(hours >= 17 & hours < 21, "Evening", "Night")))

raw_subset$time_occur_cat <- categories

# Creating dictionaries to store descriptions of unique values and frequencies of each category -------------------------------------------------------
summary_tables_top20 <- function(key_input,value_input) {
  dict <- setNames(key_input,value_input)
  df <- data.frame(
    key = names(dict),
    value = unname(dict)
  ) %>%
    dplyr::group_by(key, value) %>%
    dplyr::summarize(frequency = n(), .groups = "drop") %>%
    arrange(desc(frequency))
  df_name <- paste0(gsub(".*\\$", "",deparse(substitute(value_input))))  
  assign(df_name, df, envir = .GlobalEnv)
  
  #Frequency Counts
  row_counts <- c(10,15, 20, 25, 50)
  total_rows <- nrow(df)
  
  # printing out top cateogries
  cat("Cumulative sums of frequencies for the top categories:\n")
  for (n in row_counts) {
      if (n <= total_rows) {
          cat(paste0("Top ", n, " categories: ", sum(df$frequency[1:n]), "\n"))
      } else {
          cat(paste0("Top ", n, " categories: Not enough categories (only ", total_rows, " categories available).\n"))
      }
  }
  return(head(df,20))
}

# view summaries of top categories
summary_tables_top20(raw_subset$AREA.NAME, raw_subset$AREA)
summary_tables_top20(raw_subset$Crm.Cd.Desc,raw_subset$Crm.Cd)
summary_tables_top20(raw_subset$Premis.Desc, raw_subset$Premis.Cd)
summary_tables_top20(raw_subset$Weapon.Desc, raw_subset$Weapon.Used.Cd)

# Weapons NA Recode -------------------------------------------------------
#Creating new category None instead of NA for no weapon used
raw_subset$Weapon.Used.Cd <- as.character(raw_subset$Weapon.Used.Cd)
raw_subset$Weapon.Used.Cd[is.na(raw_subset$Weapon.Used.Cd) == T] <- "None"

# Subsetting Columns needed readying data for cleaning -----------------------------------------------
columns_to_subset <- c("AREA", "Rpt.Dist.No", "Part.1.2", "Crm.Cd", "Mocodes", 
                       "Vict.Age", "Vict.Sex", "Vict.Descent", "Premis.Cd", 
                       "Weapon.Used.Cd", "Status", "Legal_Action", 
                       "date_occur_report_difference", "time_occur_cat")

subset1 <- raw_subset[,columns_to_subset]

#Only including rows whose Crm.Cd is in top 50
crime_top_50_string_vec <- Crm.Cd$key[1:50]
filtered_subset2 <- subset1[subset1$Crm.Cd %in% crime_top_50_string_vec, ]

#Only including rows whose crime took place in Premise in top 50
premise_top_50_string_vec <- Premis.Cd$key[1:50]
filtered_subset3 <- filtered_subset2[filtered_subset2$Premis.Cd %in% premise_top_50_string_vec, ]

#Only including rows if weapon Used in top 10
summary_tables_top20(raw_subset$Weapon.Desc, raw_subset$Weapon.Used.Cd)

weapon_top_10_string_vec <- Weapon.Used.Cd$key[1:10]
filtered_subset4 <- filtered_subset3[filtered_subset3$Weapon.Used.Cd %in% weapon_top_10_string_vec, ]

#Dropping Mocodes
filtered_subset5 <- filtered_subset4[, !(colnames(filtered_subset4) %in% "Mocodes")]

#Changing Column types
filtered_subset5$Rpt.Dist.No <- as.factor(filtered_subset5$Rpt.Dist.No)
filtered_subset5$Crm.Cd <- as.factor(filtered_subset5$Crm.Cd)
filtered_subset5$Premis.Cd <- as.factor(filtered_subset5$Premis.Cd)
filtered_subset5$Weapon.Used.Cd <- as.factor(filtered_subset5$Weapon.Used.Cd)
filtered_subset5$Legal_Action <- as.factor(filtered_subset5$Legal_Action)
filtered_subset5$time_occur_cat <- as.factor(filtered_subset5$time_occur_cat)

# Cleaning Data -----------------------------------------------------------
# Identified and cleaning Negative Ages, one age of 118, and sex:X
filtered_subset6 <- filtered_subset5[filtered_subset5$Vict.Age > 0,]
filtered_subset7 <- filtered_subset6[filtered_subset6$Vict.Age <= 100, ]
filtered_subset8 <- filtered_subset7[filtered_subset7$Vict.Sex == "F" | filtered_subset7$Vict.Sex == "M", ]

#Identified and cleaning Null Race and "-" Race entering Race
filtered_subset9 <- filtered_subset8[filtered_subset8$Vict.Descent != "-", ]
filtered_subset9$Vict.Descent.Description <- ifelse(
    filtered_subset9$Vict.Descent == "A",
    "Other Asian",ifelse(filtered_subset9$Vict.Descent == "B",
    "Black", ifelse(filtered_subset9$Vict.Descent == "C",
    "Chinese",ifelse(filtered_subset9$Vict.Descent == "D",
    "Cambodian",ifelse(filtered_subset9$Vict.Descent == "F",
     "Filipino",ifelse(filtered_subset9$Vict.Descent == "G",
      "Guamanian",ifelse(filtered_subset9$Vict.Descent == "H",
       "Hispanic/Latin/Mexican",
       ifelse(filtered_subset9$Vict.Descent == "I",
       "American Indian/Alaskan Native",
       ifelse(filtered_subset9$Vict.Descent == "J","Japanese",
        ifelse(filtered_subset9$Vict.Descent == "K","Korean",
        ifelse(filtered_subset9$Vict.Descent == "L","Laotian",
        ifelse(filtered_subset9$Vict.Descent == "O","Other",
        ifelse(filtered_subset9$Vict.Descent == "P",
        "Pacific Islander",
        ifelse(filtered_subset9$Vict.Descent == "S","Samoan",
        ifelse(filtered_subset9$Vict.Descent == "U",
        "Hawaiian",ifelse(filtered_subset9$Vict.Descent == "V",
        "Vietnamese",ifelse(filtered_subset9$Vict.Descent == "W",
         "White",ifelse(filtered_subset9$Vict.Descent == "X",NA,
         ifelse(filtered_subset9$Vict.Descent == "Z", "Asian Indian", NA)))))))))))))))))))

filtered_subset10 <- filtered_subset9[, !(colnames(filtered_subset9) %in% "Vict.Descent")]
filtered_subset10$Vict.Descent.Description <- as.factor(filtered_subset10$Vict.Descent.Description)

#Only Including those with sex M or F
filtered_subset11 <- filtered_subset10[filtered_subset10$Vict.Sex == "M" | filtered_subset10$Vict.Sex == "F",]

#Removing Sub-Areas as redundant to Geopgraphic Areas
filtered_subset12 <- filtered_subset11[, !(colnames(filtered_subset11) %in% "Rpt.Dist.No")]

#Removing Status as outcome coded into Legal Action
filtered_subset13 <- filtered_subset12[, !(colnames(filtered_subset12) %in% "Status")]

#Omitting Nulls
filtered_subset14 <- na.omit(filtered_subset13)

clean_data <- filtered_subset14

# EDA: Count Plots for Some Categorical -------------------------------------------------------------
# function to create plots
barchart_fcn <- function(variable, title, x, y) {
    ggplot(clean_data, aes(x = .data[[variable]]), title, x, y) +
        geom_bar(color = "black", fill = "skyblue") +
        labs(title = title, x = x, y = y) +
        theme_minimal()
}

# legal action barchart
barchart_fcn("Legal_Action",
             title = "Count of Ongoing Investions vs Closed",
             x = "Case Status", y = "Number of Cases")

# area barchart
barchart_fcn("AREA",
             title = "Number of Crimes Per Geographic Area",
             x = "Geographic Area", y = "Number of Crimes")

#Number of Crimes Per time_occur_cat
barchart_fcn("time_occur_cat", 
             title = "Number of Crimes Per Geographic Area",
             x = "Geographic Area", y = "Number of Crimes")

#Number of Crimes Per Vict.Descent.Description
ggplot(clean_data, aes(x = Vict.Descent.Description)) +
    geom_bar(color = "black", fill = "skyblue") +  
    theme_minimal() +    
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Number of Crimes Per Victim Race/Ethnicity", x = "Victim Race/Ethnicity", y = "Number of Crimes") 

# Number of Crimes Per Victim Sex by Case Status
ggplot(clean_data, aes(x = Vict.Sex, fill = Legal_Action)) +
    geom_bar(color = "black", position = "dodge") +
    labs(title = "Number of Crimes Per Victim Sex", x = "Victim Sex", y = "Number of Crimes") +
    theme_minimal()

# EDA: Box Plots/Histograms for Continuous -------------------------------------------------------------

# Victim Age histogram
ggplot(clean_data, aes(x = Vict.Age)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    labs(title = "Histogram", x = "Victim Age", y = "Count") +
    theme_minimal()

# Days Difference in Crime Occurrence and Crime Reported histogram
ggplot(clean_data, aes(x = date_occur_report_difference)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
    labs(title = "Histogram", x = "Days Difference in Crime Occurence and Crime Reported", y = "Count") +
    theme_minimal()

#EDA Reveals date_occur_report_difference should be categorized
clean_data$date_occur_report_difference <- cut(clean_data$date_occur_report_difference,
                             breaks = c(0, 30, 60, 90, 180, 365, Inf),
                             labels = c("0-30", "31-60", "61-90", "91-180", "181-365", "365+"),
                             right = FALSE)

#Number of Crimes Per Days Difference in Crime Occurrence and Crime Reported Count Plot
ggplot(clean_data, aes(x = date_occur_report_difference)) +
    geom_bar(position = position_dodge(width = 0.4),color = "black", fill = "skyblue") +
    theme_minimal() +    
    labs(title = "Number of Crimes Per Days Difference in Crime Occurrence and Crime Reported Count Plot", 
         x = "Days Difference in Crime Occurrence and Crime Reported", y = "Number of Crimes") 

# Subsetting bootstraps defining bootstrap function -------------------------------------------
set.seed(123)

# Randomly shuffling the data and dividing into train/test
clean_data <- clean_data[sample(nrow(clean_data)), ]


clean_data_indexes <- sample(2, nrow(clean_data),
                             replace = TRUE, prob = c(0.8,0.2))
clean_data_train <- clean_data[clean_data_indexes==1,]
clean_data_test <- clean_data[clean_data_indexes==2,]


#Subsetting Train into 31 datasets
clean_data_train$Group <- sample(1:31, size = nrow(clean_data_train), replace = T)
df_subsets_train <- split(clean_data_train, clean_data_train$Group)

#Subsetting Test into 31 datasets
clean_data_test$Group <- sample(1:31, size = nrow(clean_data_test), replace = T)
clean_data_test_list <- split(clean_data_test, clean_data_test$Group)

# Define oversampling function
oversample_data <- function(data) {
    return(ovun.sample(Legal_Action ~ ., data = data, p=0.5)$data)
}

# Intializing Parallel and Bootstrapping ------------------------------------------------------------
unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}
unregister_dopar()

#initializing parallel processing
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

oversampled_data_list <- foreach(data = df_subsets_train, .packages = c("ROSE")) %dopar% {
    oversample_data(data)
}

#Calling 31st dataset
extra_clean_train <- oversampled_data_list[[31]]

# Random Forest ---------------------------------------------------
#Define the grid of hyper-parameters to tune
tune_grid <- expand.grid(mtry = c(3,7,11))
tr_control <- trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE)

rf_gridsearch <- caret::train(Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                              data = extra_clean_train,
                              tuneGrid = tune_grid,
                             method = "rf",
                             trControl = tr_control,
                              importance = TRUE,
                             ntree = 100)

rf_elbow <- plot(rf_gridsearch)
rf_elbow

#Creating empty lists
accuracy_vector_rf <- numeric(length(1:30))
conf_mat_list_rf <- vector("list",length(1:30))
variable_importance_list_rf <- vector("list",length(1:30))

tune_grid2 <- expand.grid(mtry = 11)

tr_control2 <- trainControl(
    method = "none",
    allowParallel = TRUE)

results_rf <- foreach (i = 1:30,
                    .packages = c("caret", "dplyr")) %dopar% {
                        # Training the Random Forest model 30 times w/optimal parameters
                        rf_model <- caret::train(
                            Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                            data = oversampled_data_list[[i]],
                            method = "rf",
                            tuneGrid = tune_grid2,
                            trControl = tr_control2,
                            importance = TRUE)

                        #Confusion Matrix of final model predicting Resolved Case
                        predictions_rf <- predict(rf_model, newdata = clean_data_test_list[[i]])
                        confusion_mat_rf <- confusionMatrix(predictions_rf, clean_data_test_list[[i]]$Legal_Action)

                        accuracy_vector_rf[i] <- confusion_mat_rf$overall['Accuracy']

                        var_importance_rf <- varImp(rf_model, type = 2)
                        variable_importance_list_rf[[i]] <- var_importance_rf

                        list(
                            confusion_matrix = confusion_mat_rf,
                            accuracy = confusion_mat_rf$overall['Accuracy'],
                            variable_importance = var_importance_rf
                        )
                    }

# setting results
for (i in 1:length(results_rf)){
    conf_mat_list_rf[[i]] <- results_rf[[i]]$confusion_matrix
    accuracy_vector_rf[i] <- results_rf[[i]]$accuracy
    variable_importance_list_rf[[i]] <- results_rf[[i]]$variable_importance

}

accuracy_vector_rf <- unlist(accuracy_vector_rf)

cat("Creating 95% Confidence Interval for Accuracy of Model
    predicting if case was resolved")
mean_rf_vec  <- mean(accuracy_vector_rf)

#standard error
std_error_rf <- sd(accuracy_vector_rf) / sqrt(30)

#critical t value for 95% CI
critical_value_rf <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_rf <- mean_rf_vec - (critical_value_rf * std_error_rf)
upper_ci_rf <- mean_rf_vec + (critical_value_rf * std_error_rf)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_rf, ", ", upper_ci_rf, "]\n")

#Finding Index of accuracy value closest to mean
closest_index_rf <- which.min(abs(accuracy_vector_rf - mean_rf_vec))

#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_rf[closest_index_rf])

#Variable top 20 Importance Plot of model
var_imp <- varImp(rf_gridsearch, type = 2)
var_imp_df <- as.data.frame(var_imp$importance)
var_imp_df$Variable <- rownames(var_imp_df)
top_20_vars <- var_imp_df[order(-var_imp_df$Overall), ][1:20, ]

ggplot(top_20_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Variables Ranked by Gini Impurity",
        x = "Variable",
        y = "Importance"
    ) +
    theme_minimal()

# Neural Net  ---------------------------------------------------------
library(NeuralNetTools)
library(nnet)
library(NeuralSens)
###          ###
###Neural Net###
###          ###

tr_control_nnet <- trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE)

#Neural Net
fit.nnet <- caret::train(Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                         data = extra_clean_train,
                         method = "nnet",
                         trControl = tr_control_nnet,
                         tuneGrid = expand.grid(size=c(1:5),
                                                 decay=c(0,0.1,0.01)),
                         skip = TRUE)
pred.nnet <- predict(fit.nnet,clean_data_test_list[[31]])
confusionMatrix(table(clean_data_test_list[[31]]$Legal_Action, pred.nnet))

#Optimal Parameters
fit.nnet$bestTune


#Creating empty lists
accuracy_vector_nnet <- numeric(length(1:30))
conf_mat_list_nnet <- vector("list",length(1:30))
variable_importance_list_nnet <- vector("list",length(1:30))


tr_control2_nnet <- trainControl(
    method = "none",
    allowParallel = TRUE)

results_nnet <- foreach (i = 1:30,
                       .packages = c("caret", "dplyr", "nnet")) %dopar% {
                           num_neurons <- fit.nnet$bestTune$size
                           decay_rate <- fit.nnet$bestTune$decay

                           # Training the neural net model 30 times w/1 neuron
                           nnet_model <- caret::train(
                               Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                               data = oversampled_data_list[[i]],
                               method = "nnet",
                               tuneGrid = expand.grid(size=num_neurons,decay=decay_rate),
                               trControl = tr_control2_nnet,
                               skip = TRUE
                           )

                           #Confusion Matrix of final model predicting Resolved Case
                           predictions_nnet <- predict(nnet_model, newdata = clean_data_test_list[[i]])
                           confusion_mat_nnet <- confusionMatrix(predictions_nnet, clean_data_test_list[[i]]$Legal_Action)

                           accuracy_vector_nnet[i] <- confusion_mat_nnet$overall['Accuracy']

                           var_importance_nnet <- varImp(nnet_model)
                           variable_importance_list_nnet[[i]] <- var_importance_nnet

                           list(
                               confusion_matrix = confusion_mat_nnet,
                               accuracy = confusion_mat_nnet$overall['Accuracy'],
                               variable_importance = var_importance_nnet
                           )
                       }

for (i in 1:length(results_nnet)){
    conf_mat_list_nnet[[i]] <- results_nnet[[i]]$confusion_matrix
    accuracy_vector_nnet[i] <- results_nnet[[i]]$accuracy
    variable_importance_list_nnet[[i]] <- results_nnet[[i]]$variable_importance

}
accuracy_vector_nnet <- unlist(accuracy_vector_nnet)

cat("Creating 95% Confidence Interval for Accuracy of Model
    predicting if case was resolved")
mean_nnet_vec  <- mean(accuracy_vector_nnet)

#standard error
std_error_nnet <- sd(accuracy_vector_nnet) / sqrt(30)

#critical t value for 95% CI
critical_value_nnet <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_nnet <- mean_nnet_vec - (critical_value_nnet * std_error_nnet)
upper_ci_nnet <- mean_nnet_vec + (critical_value_nnet * std_error_nnet)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_nnet, ", ", upper_ci_nnet, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_nnet <- which.min(abs(accuracy_vector_nnet - mean_nnet_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_nnet[closest_index_nnet])

# Keras:DNN ---------------------------------------------------------------
###     ###
###KERAS###
###     ###

library(keras)
#Scaling Age in Train
# Preprocess only the 4th column (centering and scaling)
preprocessed_data <- preProcess(clean_data_train[, 4, drop = FALSE], method = c("center", "scale"))

# Apply the transformation to the same column
clean_data_train2 <- clean_data_train
clean_data_train2[, 4] <- predict(preprocessed_data, clean_data_train2[, 4, drop = FALSE])

#Scaling Age in Test
# Preprocess only the 4th column (centering and scaling)
preprocessed_data2 <- preProcess(clean_data_test[, 4, drop = FALSE], method = c("center", "scale"))

# Apply the transformation to the same column
clean_data_test2 <- clean_data_test
clean_data_test2[, 4] <- predict(preprocessed_data2, clean_data_test2[, 4, drop = FALSE])



# Create dummy variables for train dataset
dummy_model_clean_train2 <- dummyVars( ~ AREA + Part.1.2 + Crm.Cd + Legal_Action + Vict.Age + Vict.Sex + Premis.Cd +
                                          Weapon.Used.Cd + date_occur_report_difference + time_occur_cat +
                                          Vict.Descent.Description,
                                      data = clean_data_train2)

# Apply the transformation
dummy_clean_train2 <- predict(dummy_model_clean_train2, newdata = clean_data_train2)

# Convert to a data frame
dummy_clean_train2 <- as.data.frame(dummy_clean_train2)




# Create dummy variables for test dataset
dummy_model_clean_test2 <- dummyVars( ~ AREA + Part.1.2 + Crm.Cd + Legal_Action + Vict.Age + Vict.Sex + Premis.Cd +
                                          Weapon.Used.Cd + date_occur_report_difference + time_occur_cat +
                                          Vict.Descent.Description,
                                      data = clean_data_test2)

# Apply the transformation
dummy_clean_test2 <- predict(dummy_model_clean_test2, newdata = clean_data_test2)

# Convert to a data frame
dummy_clean_test2 <- as.data.frame(dummy_clean_test2)



#Converting data to proper input for Keras DNN
X_train <- as.matrix(dummy_clean_train2[,-c(74,75)])
Y_train <- as.matrix(dummy_clean_train2[,c(74,75)])
Y_train <- apply(Y_train, 1, function(x) which.max(x) - 1)


X_test <- as.matrix(dummy_clean_test2[,-c(74,75)])
Y_test <- as.matrix(dummy_clean_test2[,c(74,75)])
Y_test <- apply(Y_test, 1, function(x) which.max(x) - 1)




#Activation Function
activation_function <- function() {
    dnn_class_model <- keras_model_sequential() %>%
        layer_dense(units = 50, activation = 'relu',
                    input_shape = c(ncol(X_train))) %>%
        layer_dense(units = 50, activation = 'relu') %>%
        layer_dense(units = 1, activation = 'sigmoid') %>%
        compile(optimizer = 'adam',
                loss = 'binary_crossentropy',
                metrics = 'accuracy')
}

#Running Model
dnn_class_model <- activation_function()
results_dnn <- dnn_class_model %>%
    keras::fit(x = X_train, y = Y_train,
        epochs = 30,
        validation_split = 0.2,
        verbose = 0,
        batch_size = 128)

#Visualizing Model Performance
plot(results_dnn,
     smooth = F)

#Obtaining Predictions
Y_test_pred <- predict(object = dnn_class_model, x = X_test)

#Computing AUC
library(pROC)
Test_dnn <- cbind(Y_test_pred, Y_test)
Test_dnn <- as.data.frame(Test)

#AUC:
Test_dnn_auc <- roc(Test_dnn$Y_test, Test_dnn$V1)

#Converting probabilities to Predictions using threshold of 0.5
Test_dnn$V1 <- ifelse(Test_dnn$V1 > 0.5,1,0)
Test_dnn$V1 <- as.factor(Test_dnn$V1)
Test_dnn$Y_test <- as.factor(Test_dnn$Y_test)


#Confusion Matrix DNN
cm_dnn <- confusionMatrix(Test_dnn$Y_test, Test_dnn$V1)
cm_dnn


# Fitting Naive Bayes -----------------------------------------------------
library(naivebayes)
extra_clean_train_nb <- extra_clean_train
extra_clean_train_nb <- extra_clean_train_nb[,-12]

nb_grid <- expand.grid(usekernel = c(TRUE, FALSE),
                       laplace = c(0, 0.5, 1),
                       adjust = c(0.75, 1, 1.25, 1.5))

tr_control_nb <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    allowParallel = TRUE
)

nb_fit <- caret::train(
    Legal_Action ~ .,
    data = extra_clean_train_nb,
    method = "naive_bayes",
    trControl = tr_control_nb,
    tuneGrid = nb_grid
)

#Checking model optimal parameters
nb_fit$finalModel$tuneValue

#Visualizing tuning process
nb_fit_plot <- plot(nb_fit)
nb_fit_plot

# Performing classification
predictions_nb1 <- predict(nb_fit, newdata = clean_data_test_list[[31]])
confusion_mat_nb1 <- confusionMatrix(predictions_nb1, clean_data_test_list[[31]]$Legal_Action)

#Creating accuracy and confusion matrices vectors
accuracy_vector_nb <- numeric(length(1:30))
conf_mat_list_nb <- vector("list",length(1:30))

results_nb <- foreach (i = 1:30,
                        .packages = c("caret", "dplyr", "naivebayes")) %dopar% {
                            #Optimal Parameters
                            laplace_param <- nb_fit$finalModel$tuneValue$laplace
                            usekernel_param <- nb_fit$finalModel$tuneValue$usekernel
                            adjust_param <- nb_fit$finalModel$tuneValue$adjust

                            # Training the Naive Bayes model 30 times w/optimal parameters
                            nb_model <- caret::train(
                                Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                                data = oversampled_data_list[[i]],
                                method = "naive_bayes",
                                trControl = trainControl(method = "none"),
                                tuneGrid = expand.grid(.laplace = laplace_param, .usekernel = usekernel_param, .adjust = adjust_param)
                            )

                            #Confusion Matrix of final model predicting Resolved Case
                            predictions_nb <- predict(nb_model, newdata = clean_data_test_list[[i]])
                            confusion_mat_nb <- confusionMatrix(predictions_nb, clean_data_test_list[[i]]$Legal_Action)

                            accuracy_vector_nb[i] <- confusion_mat_nb$overall['Accuracy']

                            list(confusion_matrix = confusion_mat_nb,
                                 accuracy = confusion_mat_nb$overall['Accuracy']
                            )
                        }

for (i in 1:length(results_nb)){
    conf_mat_list_nb[[i]] <- results_nb[[i]]$confusion_matrix
    accuracy_vector_nb[i] <- results_nb[[i]]$accuracy
}
accuracy_vector_nb <- unlist(accuracy_vector_nb)


cat("Creating 95% Confidence Interval for Accuracy of Model
    predicting if case was resolved\n")
mean_nb_vec  <- mean(accuracy_vector_nb)

#standard error
std_error_nb <- sd(accuracy_vector_nb) / sqrt(30)

#critical t value for 95% CI
critical_value_nb <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_nb <- mean_nb_vec - (critical_value_nb * std_error_nb)
upper_ci_nb <- mean_nb_vec + (critical_value_nb * std_error_nb)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_nb, ", ", upper_ci_nb, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_nb <- which.min(abs(accuracy_vector_nb - mean_nb_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_nb[closest_index_nb])

# Fitting Logistic Regression ---------------------------------------------
library(pROC)
trControl_log <- trainControl(method = 'repeatedcv',
                          number = 5,
                          repeats =  5,
                          search = 'random')

logit_gridsearch <- caret::train( Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + 
                               Vict.Sex + Premis.Cd + Weapon.Used.Cd + 
                               date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                           data = extra_clean_train,
                           method = 'glmnet',
                           trControl = trControl_log,
                           family = 'binomial',
                           metric = 'Accuracy')
#Optimal Parameters
logit_gridsearch$bestTune

#Creating empty lists
accuracy_vector_logit <- numeric(length(1:30))
conf_mat_list_logit <- vector("list",length(1:30))
auc_list_logit <- numeric(length(1:30))

results_logit <- foreach (i = 1:30, 
                       .packages = c("caret", "dplyr", "pROC")) %dopar% {
                           logit_alpha <- logit_gridsearch$bestTune$alpha
                           logit_lambda<- logit_gridsearch$bestTune$lambda
                           
                           # Training the Random Forest model 30 times w/optimal parameters
                           logit_model <- caret::train(
                               Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                               data = oversampled_data_list[[i]],
                               method = "glmnet",
                               trControl = trainControl(method = "none"),  
                               tuneGrid = expand.grid(alpha = logit_alpha, lambda = logit_lambda))
                           
                           #Confusion Matrix of final model predicting Resolved Case
                           predictions_logit <- predict(logit_model, newdata = clean_data_test_list[[i]])
                           confusion_mat_logit <- confusionMatrix(predictions_logit, clean_data_test_list[[i]]$Legal_Action)
                           
                           accuracy_vector_logit[i] <- confusion_mat_logit$overall['Accuracy']

                           #Predictions with probabilities
                           logit_auc_pred <- predict(logit_model, clean_data_test_list[[i]], 'prob')
                           logit_auc_pred <- cbind(logit_auc_pred[,1],as.character(clean_data_test_list[[i]]$Legal_Action))
                           logit_auc_pred <- as.data.frame(logit_auc_pred)
                           logit_auc_pred$V2 <- as.factor(logit_auc_pred$V2)
                           logit_auc_pred$V1 <- as.numeric(logit_auc_pred$V1)
                           
                           #AUC
                           logit_auc_roc <- roc(logit_auc_pred$V2, logit_auc_pred$V1)
                           auc_list_logit <- logit_auc_roc$auc[1] 
                           
                           list(
                               confusion_matrix = confusion_mat_logit,
                               accuracy = confusion_mat_logit$overall['Accuracy'],
                               auc = auc_list_logit
                           )
                       }

for (i in 1:length(results_logit)){
    conf_mat_list_logit[[i]] <- results_logit[[i]]$confusion_matrix
    accuracy_vector_logit[i] <- results_logit[[i]]$accuracy
    auc_list_logit[[i]] <- results_logit[[i]]$auc
    
}

accuracy_vector_logit <- unlist(accuracy_vector_logit)

cat("Creating 95% Confidence Interval for Accuracy of Model 
    predicting if case was resolved\n")
mean_logit_vec  <- mean(accuracy_vector_logit)

#standard error
std_error_logit <- sd(accuracy_vector_logit) / sqrt(30)

#critical t value for 95% CI
critical_value_logit <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_logit <- mean_logit_vec - (critical_value_logit * std_error_logit)
upper_ci_logit <- mean_logit_vec + (critical_value_logit * std_error_logit)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_logit, ", ", upper_ci_logit, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_logit <- which.min(abs(accuracy_vector_logit - mean_logit_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_logit[closest_index_logit])

#AUC of Model closest to mean
print(auc_list_logit[closest_index_logit])

# Fitting KNN -------------------------------------------------------------

#Train the kNN model with automatic tuning of k using tuneLength
tr_control_knn <- trainControl(method = "repeatedcv",
                               repeats = 10,
                               allowParallel = TRUE)

#Creating empty lists
accuracy_vector_knn <- numeric(length(1:30))
conf_mat_list_knn <- vector("list",length(1:30))


results_knn <- foreach (i = 1:30,
                       .packages = c("caret", "dplyr")) %dopar% {
                           # Training the Random Forest model 30 times w/optimal parameters
                           knn_model <- caret::train(
                               Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                               data = oversampled_data_list[[i]],
                               method = "knn",
                               trControl = tr_control_knn,
                               tuneLength = 20)

                           #Confusion Matrix of final model predicting Resolved Case
                           predictions_knn <- predict(knn_model, newdata = clean_data_test_list[[i]])
                           confusion_mat_knn <- confusionMatrix(predictions_knn, clean_data_test_list[[i]]$Legal_Action)

                           accuracy_vector_knn[i] <- confusion_mat_knn$overall['Accuracy']

                           list(confusion_matrix = confusion_mat_knn,
                                accuracy = confusion_mat_knn$overall['Accuracy']
                                )
                       }

for (i in 1:length(results_knn)){
    conf_mat_list_knn[[i]] <- results_knn[[i]]$confusion_matrix
    accuracy_vector_knn[i] <- results_knn[[i]]$accuracy
}
accuracy_vector_knn <- unlist(accuracy_vector_knn)


cat("Creating 95% Confidence Interval for Accuracy of Model
    predicting if case was resolved\n")
mean_knn_vec  <- mean(accuracy_vector_knn)

#standard error
std_error_knn <- sd(accuracy_vector_knn) / sqrt(30)

#critical t value for 95% CI
critical_value_knn <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_knn <- mean_knn_vec - (critical_value_knn * std_error_knn)
upper_ci_knn <- mean_knn_vec + (critical_value_knn * std_error_knn)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_knn, ", ", upper_ci_knn, "]\n")


#Finding Index of accuracy value closest to mean
closest_index_knn <- which.min(abs(accuracy_vector_knn - mean_knn_vec))


#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_knn[closest_index_knn])


# Fitting SVM -------------------------------------------------------------

#SVM Linear Classifier
svm_grid <- expand.grid(C = seq(0, 2, length = 20))
train_control_svm <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel = TRUE)


# Fit the model
svm_grid_search <- caret::train(Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + 
                             Vict.Sex + Premis.Cd + Weapon.Used.Cd + 
                             date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                         data = extra_clean_train, 
                         method = "svmLinear",
                         trControl = train_control_svm,
                         tuneGrid = svm_grid)
# View grid search result
sorted_svm_results <- svm_grid_search$results[order(-svm_grid_search$results$Accuracy), ]
head(sorted_svm_results)
plot(svm_grid_search)


#Checking SVM Performance with Radial Kernel
svm_grid_radial <- expand.grid(C = seq(0, 2, length = 10), sigma = seq(0, 2, length = 10))


svm_grid_search_radial <- caret::train(Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + 
                             Vict.Sex + Premis.Cd + Weapon.Used.Cd + 
                             date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                         data = extra_clean_train, 
                         method = "svmRadial",
                         trControl = train_control_svm,
                         tuneGrid = svm_grid_radial)

#Viewing Radial non-linear Kernel result
sorted_svm_results_radial <- svm_grid_search_radial$results[order(-svm_grid_search_radial$results$Accuracy), ]
head(sorted_svm_results_radial)
plot(svm_grid_search_radial)


cat("Going with the linear kernel as returned higher accuracies during cross-validation")

#Best model optimal parameters
sorted_svm_results[1,1]

#Creating empty lists
accuracy_vector_svm_linear <- numeric(length(1:30))
conf_mat_list_svm_linear <- vector("list",length(1:30))

svm_grid_final <- expand.grid(C = sorted_svm_results[1,1])


results_svm_linear <- foreach (i = 1:30, 
                       .packages = c("caret", "dplyr")) %dopar% {
                           # Training the Random Forest model 30 times w/optimal parameters
                           svm_linear_model <- caret::train(
                               Legal_Action ~ AREA + Part.1.2 + Crm.Cd + Vict.Age + Vict.Sex + Premis.Cd + Weapon.Used.Cd + date_occur_report_difference + time_occur_cat + Vict.Descent.Description,
                               data = oversampled_data_list[[i]],
                               method = "svmLinear",
                               tuneGrid = svm_grid_final,
                               trControl = trainControl(method = "none",allowParallel = TRUE),
                               )
                           
                           #Confusion Matrix of final model predicting Resolved Case
                           predictions_svm_linear <- predict(svm_linear_model, newdata = clean_data_test_list[[i]])
                           confusion_mat_svm_linear <- confusionMatrix(predictions_svm_linear, clean_data_test_list[[i]]$Legal_Action)
                           accuracy_vector_svm_linear[i] <- confusion_mat_svm_linear$overall['Accuracy']
                           
                           list(
                               confusion_matrix = confusion_mat_svm_linear,
                               accuracy = confusion_mat_svm_linear$overall['Accuracy']
                           )
                       }

for (i in 1:length(results_svm_linear)){
    conf_mat_list_svm_linear[[i]] <- results_svm_linear[[i]]$confusion_matrix
    accuracy_vector_svm_linear[i] <- results_svm_linear[[i]]$accuracy
}
accuracy_vector_svm_linear <- unlist(accuracy_vector_svm_linear)

cat("Creating 95% Confidence Interval for Accuracy of Model 
    predicting if case was resolved")
mean_svm_linear_vec  <- mean(accuracy_vector_svm_linear)

#standard error
std_error_svm_linear <- sd(accuracy_vector_svm_linear) / sqrt(30)

#critical t value for 95% CI
critical_value_svm_linear <- qt(0.975, df = 30 - 1)

#confidence interval
lower_ci_svm_linear <- mean_svm_linear_vec - (critical_value_svm_linear * std_error_svm_linear)
upper_ci_svm_linear <- mean_svm_linear_vec + (critical_value_svm_linear * std_error_svm_linear)

# 95% CI
cat("95% Confidence Interval Predicting if Case Resolved: [", lower_ci_svm_linear, ", ", upper_ci_svm_linear, "]\n")

#Finding Index of accuracy value closest to mean
closest_index_svm_linear <- which.min(abs(accuracy_vector_svm_linear - mean_svm_linear_vec))

#Confusion Matrix of Model closest to mean accuracy
print(conf_mat_list_svm_linear[closest_index_svm_linear])

stopCluster(cl)
unregister_dopar()

