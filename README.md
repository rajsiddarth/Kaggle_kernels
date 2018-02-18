# Kaggle_kernels

This repository contains practice kernels based on popular kernels published in Kaggle.
This reposirtory also contains submissions for Kaggle Competitions.

## Project-1 :Data Exploration using Plotly 
  
  This kernal is based on the dataset provided by https://www.kaggle.com/iliassekkaf/computerparts. The data set taken into consideration is All-GPU's.csv.The link provided below shows visualizations in plotly for the exploration of the data set.The jupyter notebook for exploration is also provided in the repository named GPU_regression.
  
http://nbviewer.jupyter.org/github/rajsiddarth/Kaggle_kernels/blob/master/GPU_Regression.ipynb

## Project-2: House Price Prediction using Stacked Regression 

This kernel is based on the data https://www.kaggle.com/c/house-prices-advanced-regression-techniques. The train and test data sets are used to build a stacked regression model.This data set involves extensive data exploration to fill in the missing values.The base learners used to build the stacked regression model are Lasso,Elastic net,Kernel ridge regression,Gradient boosting and xgboost.The ideas for building a stack net are borrowed from  https://github.com/kaz-Anova/StackNet. The stacked regression approach involves the following steps.

### Stacked Regression

1. Split the train data sets into two sets A and B which already have the predictions.

2. Build k-fold cross validation models using each of the base learners.For example train the model 1 on A using base learner 1 and cross validaton.Then use model 1 to predict on B. These predictions will be the first column of a new data set B'.

3. Similary predict on B using different base models trained on A.These predictions will be the columns of data set B'.

4. Similarly predict on the test data set C,and build a new data set C' containing the predictions from the base models trained on A.

5. Now as we know the true values of B.Train a new meta learnerusing data set B'

6. Finally we can now predict on data set C' using the meta learner. 

# Project 3
Text Mining
https://www.kaggle.com/c/mercari-price-suggestion-challenge


