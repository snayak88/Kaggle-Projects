# -*- coding: utf-8 -*-
"""
Created on Mon Dec 25 13:18:01 2017

@author: Sandesh
"""

import numpy as np
import pandas as pd
from numpy import array

#Data Preprocessing
train_data = pd.read_csv("train.csv")
test_data = pd.read_csv("test.csv")

X_train = train_data.iloc[:,0:561]
X_test = test_data.iloc[:,0:561]
y_train = train_data.iloc[:,562]
y_test = test_data.iloc[:,562]

from sklearn.decomposition import PCA
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder

labelencoder_train = LabelEncoder()
values = array(y_train)
y_train = labelencoder_train.fit_transform(values)
labelencoder_test = LabelEncoder()
values = array(y_test)
y_test = labelencoder_test.fit_transform(values)
onehot_encoder = OneHotEncoder(sparse=False)
y_train = y_train.reshape(len(y_train),1)
y_test = y_test.reshape(len(y_test),1)
y_train = onehot_encoder.fit_transform(y_train)
y_test = onehot_encoder.fit_transform(y_test)
def evaluate_classifier(classifier, X_train, y_train):
    classifier.fit(X_train, y_train)
    y_pred = classifier.predict(X_test)
    cm = confusion_matrix(y_test, y_pred)
    return cm

#Principal Components Analysis
from sklearn.decomposition import PCA
components = 100
pca = PCA(n_components=components)
X_train_pca = pca.fit_transform(X_train)
X_test_pca = pca.transform(X_test)

#Random Forest Classifier - Accuracy 91.1% on the test set
from sklearn.ensemble import RandomForestClassifier
classifier = RandomForestClassifier(criterion="gini", random_state = 0)
cm_random_forest = evaluate_classifier(classifier, X_train, y_train)

#Kernel SVM Classifier - RBF - 94% accuracy on test set. Linear - 96% accuracy on test set
from sklearn.svm import SVC
classifier = SVC(kernel="linear",random_state=0)
cm_svm = evaluate_classifier(classifier, X_train, y_train)


#Neural Network
from keras.layers import Dense
from keras.models import Sequential
from keras.layers import Dropout

classifier = Sequential()
classifier.add(Dense(input_dim=100, output_dim=50, activation="relu", init="uniform"))
classifier.add(Dropout(p=0.1))
classifier.add(Dense(output_dim=50, activation="relu", init="uniform"))
classifier.add(Dropout(p=0.1))
classifier.add(Dense(output_dim=6, activation="softmax", init="uniform"))
classifier.compile(optimizer="adam", loss="categorical_crossentropy", metrics=["accuracy"])
classifier.fit(X_train_pca, y_train, batch_size=25, epochs=100)
y_pred = classifier.predict(X_test_pca)
y_prediction = np.argmax(y_pred, axis=1)
y_test = np.argmax(y_test, axis=1)

#y_prediction = np.array([1,2,3,4,5,6])

from sklearn.metrics import confusion_matrix
cm_nn = confusion_matrix(y_test,y_prediction)

