# Non funziona...


import os, sys
from google.colab import drive
drivedir='/content/drive'
drive.mount(drivedir)
os.chdir(drivedir)
datadir=drivedir+'/MyDrive/'
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler, PolynomialFeatures
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
import tensorflow as tf
import pandas as pd
import matplotlib.pyplot as plt
import copy

def cost_function(X, y, w, b): 
    m = X.shape[0]
    cost = 0.0
    for i in range(m):                                
        f_wb_i = np.dot(X[i], w) + b    
        cost_temp = cost + (f_wb_i - y[i])**2     
    cost = cost_temp / (2 * m)                      
    return cost

# Per fare il gradiente in più dimensioni
def gradient(X, y, w, b): 
    m,n = X.shape  ## (number of examples, number of features)
    dj_dw = np.zeros((n,))
    dj_db = 0.
    for i in range(m):                             
        err = (np.dot(X[i], w) + b) - y[i]   
        for j in range(n):                         
            dj_dw[j] = dj_dw[j] + err * X[i, j]    
        dj_db = dj_db + err                        
    dj_dw = dj_dw / m                                
    dj_db = dj_db / m                                
    return dj_db, dj_dw

# Per fare il GD in più dimensioni
def gradient_descent(X, y, w_in, b_in, cost_function, gradient, alpha, num_iters): 
    w = copy.deepcopy(w_in)  ## avoid modifying global w within function
    b = b_in
    for i in range(num_iters):
        # Calculate the gradient and update the parameters
        dj_db,dj_dw = gradient(X, y, w, b)   
        # Update Parameters using w, b, alpha and gradient
        w = w - alpha * dj_dw               
        b = b - alpha * dj_db                 
    return w, b 



df = pd.read_csv(datadir + "prova_nonlinear_relationships.csv")
x1 = df['value']; x2 = df['gws_anomalies10']; x3 = df['gws_std10']; x4 = df['CV10']; y = df['count']
X1 = x1; X2 = x1**2; X3 = x2; X4 = x2**2; X5 = x4; X6 = x4**2
X = np.array([X1, X2, X3, X4, X5, X6])
X = X.T
