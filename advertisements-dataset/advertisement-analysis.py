# -*- coding: utf-8 -*-
#Analysis of a kaggle dataset of advertisements using various classification methods
"""
Created on Sat Dec 23 21:11:59 2017

@author: Sandesh
"""

import numpy as np
import pandas as pd

#Part 1 - Data Preprocessing
            
df = pd.read_csv("add.csv")

df = df.applymap(lambda val: np.nan if str(val).strip() == '?' else val)
df = df.dropna()

X = df.iloc[:,1:1559].values
y = df.iloc[:,1559].values

from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cross_validation import train_test_split
label_encoder_y = LabelEncoder()
y = label_encoder_y.fit_transform(y)

sc = StandardScaler()
X = sc.fit_transform(X)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

def evaluate_classifier(classifier, X_train, y_train):
    classifier.fit(X_train, y_train)
    y_pred = classifier.predict(X_test)
    cm = confusion_matrix(y_test, y_pred)
    return cm
    
# Fit different classification models.
#1. SVM
from sklearn.svm import SVC
from sklearn.metrics import confusion_matrix
classifier = SVC(kernel="linear",random_state = 0)
cm_svm = evaluate_classifier(classifier, X_train, y_train)

#2. Logistic Regression
from sklearn.linear_model import LogisticRegression
classifier = LogisticRegression(random_state = 0)
cm_logistic_regression = evaluate_classifier(classifier, X_train, y_train)

#3. Decision Tree
from sklearn.tree import DecisionTreeClassifier
classifier = DecisionTreeClassifier(criterion = "entropy", random_state = 0)
cm_decision_tree = evaluate_classifier(classifier, X_train, y_train)

#4. Random Forest Classifier
from sklearn.ensemble import RandomForestClassifier
classifier = RandomForestClassifier(criterion="gini", random_state = 0)
cm_random_forest = evaluate_classifier(classifier, X_train, y_train)

#5. Naive Bayes Classifier
from sklearn.naive_bayes import GaussianNB
classifier= GaussianNB()
cm_naive_bayes = evaluate_classifier(classifier, X_train, y_train)

#6. K Nearest Neighbors
from sklearn.neighbors import KNeighborsClassifier
classifier = KNeighborsClassifier(n_neighbors = 10)
cm_knn = evaluate_classifier(classifier, X_train, y_train)

#7. Kernel SVM
classifier = SVC(kernel="rbf",random_state = 0)
evaluate_classifier(classifier, X_train, y_train)

#Apply Principal Components Analysis
pca = PCA(n_components = 50)
X_train_pca = pca.fit_transform(X_train)
X_test_pca = pca.transform(X_test)

#Train the truncated data using a Neural Network
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout

classifier = Sequential()
classifier.add(Dense(input_dim = 50, output_dim = 25, init = "uniform", activation = "relu"))
classifier.add(Dropout(p=0.1))
classifier.add(Dense(output_dim = 25, init = "uniform", activation = "relu"))
classifier.add(Dropout(p=0.1))
classifier.add(Dense(output_dim = 1, init = "uniform", activation = "sigmoid"))
classifier.compile(optimizer = "adam", loss = "binary_crossentropy", metrics = ["accuracy"])
classifier.fit(X_train_pca, y_train, batch_size=25, epochs=500)

y_pred = classifier.predict(X_test_pca)
y_pred = (y_pred > 0.5)
cm_neuralnet = confusion_matrix(y_test, y_pred)
