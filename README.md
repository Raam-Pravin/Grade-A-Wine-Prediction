# Wine Quality Prediction Using Random Forest
This repository contains a machine learning project focused on predicting Grade A wine quality for both red and white wines using Random Forest classifiers. The goal is to identify the important factors contributing to Grade A wine quality in both red and white wines and compare the differences between the two. The project involves data preprocessing, feature selection, model evaluation, and hyperparameter tuning using grid search and cross-validation.

## Contents


## Overview
### 1. Data Preprocessing
- Bootstrapping is applied to address class imbalance, specifically focusing on Grade A wines (quality >= 8)
- Min-Max Scaling is applied to scale all numerical features to a range between 0 and 1, ensuring that no feature dominates due to differences in their original scales. This helps improve the performance of machine learning algorithms

### 2. Model Training
- Random Forest models are trained for both red and white wines separately using 10-fold cross-validation and grid search to optimize hyperparameters
- Hyperparameters were tuned separately for each wine type to ensure optimal performance for both red and white wines

### 3. Feature Importance
- Variable importance is calculated for both red and white wines using the Gini Impurity method, and key predictors for Grade A wine quality are compared between the two datasets

### 4. Model Evaluation
- The models’ performances are assessed using accuracy and confusion matrices
- A 95% confidence interval is calculated for model accuracy to understand prediction stability

### 5. Comparison
- Key differences in the most important features for predicting Grade A wines are analyzed for red and white wines


## Requirements
To run this project, you must have the following libraries installed:

- caret: For model training, cross-validation, and hyperparameter tuning
- dplyr: For data manipulation, including feature selection and preprocessing
- randomForest: For building the Random Forest models
- foreach: To parallelize model training for efficiency
- doParallel: For parallel processing during model training
- ggplot2: For visualizing model performance, feature importance, and comparison between red and white wines


## Data
**Red Wine Dataset**: Contains various chemical features and the quality ratings of red wines
**White Wine Dataset**: Similar to the red wine dataset contains the same chemical features and quality ratings of white wines
The datasets are publicly available and can be found here: https://archive.ics.uci.edu/dataset/186/wine+quality 


## Results
### Red Wine Model
- The Random Forest model for predicting Grade A red wine achieved an accuracy of 0.8949, with a 95% Confidence Interval of [0.8569, 0.9257]
- Most important features: alcohol, sulphates, volatile_acidity, citric_acid, and chlorides

### White Wine Model
- The Random Forest model for predicting Grade A white wine achieved an accuracy of 0.8613, with a 95% Confidence Interval of [0.8379, 0.8825]
- Most important features: alcohol, density, chlorides, total_sulfur_dioxide, and residual_sugar

By identifying these crucial factors, the models can help winemakers focus on optimizing these characteristics to improve wine quality.

## Future work
- Feature refinement: Investigating additional factors or interactions between variables to further improve prediction accuracy
- Data balance techniques: Addressing class imbalances, especially in the Grade A categories, using techniques like SMOTE to improve prediction performance
- Cross-algorithm evaluation: Exploring other algorithms, such as XGBoost or Gradient Boosting, to compare performance and robustness across models

Leveraging these insights into important variables will not only enhance predictive modeling but also provide actionable recommendations for winemakers to focus on the most influential factors that drive wine quality
