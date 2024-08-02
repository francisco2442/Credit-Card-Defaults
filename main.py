# Credit Card Defaults in Taiwan

# import modules
import matplotlib.pyplot as plt
import scipy.stats as stats
import seaborn as sns
import pandas as pd
import numpy as np
import matplotlib
import warnings
import sklearn
import scipy
import json
import sys
import csv
import os

# Setup working directory
os.chdir("C:/Users/franc/OneDrive/Documents/Data Science/WK5/GitHub5/")

# Import data
credit = pd.read_csv("creditcard.csv")
print(credit)

# data summary
credit.info()
credit.describe()

# removing ID variable
credit1 = credit.drop("ID",axis=1)  # axis=0 --> row; axis=1 --> column
print(credit1)
credit1.head()

# checking for missing values
missing_values = credit1.isnull().sum()
print(missing_values)
# no missing values in the data set

# data cleaning, merging categorical variables with few observation
print(credit1)
# recording new education column
credit1['EDUCATION'] = credit1['EDUCATION'].replace({0:4, 5:4, 6:4})
# recording new marriage column
credit1['MARRIAGE'] = credit1['MARRIAGE'].replace({0:3})
print(credit1)

# making some numeric variables into factors
credit1.info()
# converting 'default.payment.next.month', 'SEX', 'EDUCATION', and 'MARRIAGE' into factors
credit1['default.payment.next.month'] = credit1['default.payment.next.month'].astype('category')
credit1['SEX'] = credit1['SEX'].astype('category')
credit1['EDUCATION'] = credit1['EDUCATION'].astype('category')
credit1['MARRIAGE'] = credit1['MARRIAGE'].astype('category')
# check
credit1.info()

# Necessary modules for creating models.
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import cross_val_score, StratifiedKFold
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import mean_squared_error
from sklearn.ensemble import BaggingClassifier
from sklearn.metrics import accuracy_score,classification_report, precision_recall_curve, confusion_matrix
from sklearn.model_selection import train_test_split

# data visualization, and exploratory data analytics # todo finish the data visuals


# splitting the data set
credit1.info()
x = credit1[['PAY_0', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6', 'BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5', 'PAY_AMT6']]
y = credit1['default.payment.next.month']

# splitting the data into training and test sets
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=42)

# Running a Logistical Model
model = LogisticRegression()

# fit the model to the training data
model.fit(x_train, y_train)

# making predictions on the test data
y_pred = model.predict(x_test)
y_pred

# confusion matrix, for analyzing how well the model preformed
# Print accuracy score
print("Accuracy Score:", accuracy_score(y_test, y_pred))

# Print classification report
print("\nClassification Report:\n", classification_report(y_test, y_pred))



