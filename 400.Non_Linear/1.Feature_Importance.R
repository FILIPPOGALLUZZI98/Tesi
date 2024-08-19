# Montaggi GDrive
import os, sys
from google.colab import drive
drivedir='/content/drive'
drive.mount(drivedir)
os.chdir(drivedir)
datadir=drivedir+'/MyDrive/'

import numpy as np
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error,  r2_score
from sklearn.metrics import accuracy_score
from xgboost import XGBClassifier
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
from sklearn.feature_selection import RFE
from sklearn.inspection import PartialDependenceDisplay


gm = pd.read_csv(datadir+'gws_migr.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']


#############################################################################################################
#############################################################################################################
df_1year = gm[gm['interval'] == 1]; df_5year = gm[gm['interval'] == 5]
X_1year = df_1year[features]; X_5year = df_5year[features]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']

X_train_1, X_test_1, y_train_1, y_test_1 = train_test_split(X_1year, y_1year, test_size=0.2, random_state=42)
X_train_5, X_test_5, y_train_5, y_test_5 = train_test_split(X_5year, y_5year, test_size=0.2, random_state=42)

# 1-year
rf_1year = RandomForestRegressor(random_state=42)
rf_1year.fit(X_train_1, y_train_1)
y_pred_1 = rf_1year.predict(X_test_1)
mse_1 = mean_squared_error(y_test_1, y_pred_1)
r2_1 = r2_score(y_test_1, y_pred_1)

# 5-years
rf_5year = RandomForestRegressor(random_state=42)
rf_5year.fit(X_train_5, y_train_5)
y_pred_5 = rf_5year.predict(X_test_5)
mse_5 = mean_squared_error(y_test_5, y_pred_5)
r2_5 = r2_score(y_test_5, y_pred_5)

# Results
print("1 year interval:")
print(f"Mean Squared Error (MSE): {mse_1}")
print(f"R² Score: {r2_1}")
print("\n 5 years interval:")
print(f"Mean Squared Error (MSE): {mse_5}")
print(f"R² Score: {r2_5}")


#############################################################################################################
#############################################################################################################
####  FEATURE IMPORTANCE  ####

importances_1year = rf_1year.feature_importances_
importances_5year = rf_5year.feature_importances_
sorted_indices_1year = importances_1year.argsort()
sorted_indices_5year = importances_5year.argsort()
plt.figure(figsize=(8, 5))

# 1-year
plt.subplot(2, 1, 1)
plt.barh([features[i] for i in sorted_indices_1year], importances_1year[sorted_indices_1year], color='skyblue')
plt.xlabel('Importance'); plt.ylabel('Variables'); plt.title('Interval 1 Year')

# 5-years
plt.subplot(2, 1, 2)
plt.barh([features[i] for i in sorted_indices_5year], importances_5year[sorted_indices_5year], color='lightgreen')
plt.xlabel('Importance'); plt.ylabel('Variables'); plt.title('Interval 5 Years')

plt.tight_layout(); plt.show()


#############################################################################################################
#############################################################################################################
####  PARTIAL DEPENDENCE PLOTS  ####

features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']

# 1-Y
fig, ax = plt.subplots(figsize=(12, 12))  
display = PartialDependenceDisplay.from_estimator(rf_1year, X_train_1, features, ax=ax, n_cols=3)
plt.suptitle('Partial Dependence Plots - 1 year', fontsize=16)
plt.subplots_adjust(top=0.9)
plt.show()

# 5-Y
fig, ax = plt.subplots(figsize=(12, 12))  
PartialDependenceDisplay.from_estimator(rf_5year, X_train_5, features, grid_resolution=50, ax=ax, n_cols=3)
plt.suptitle('Partial Dependence Plots - 5 year', fontsize=16)
plt.subplots_adjust(top=0.95)
plt.show()


#############################################################################################################
#############################################################################################################
####  PARAMETER OPTIMIZATION  ####

top_6_indices_1year = sorted_indices_1year[-6:][::-1]
top_6_indices_5year = sorted_indices_5year[-6:][::-1]
variables1 = [features[i] for i in top_6_indices_1year]
variables5 = [features[i] for i in top_6_indices_5year]
print("First 6 variables by importance - 1 year:"); print(variables1)
print("\nFirst 6 variables by importance - 5 years:"); print(variables5)

# Select the first 6 variables 
df_1year = gm[gm['interval'] == 1]; df_5year = gm[gm['interval'] == 5]
X_1year = df_1year[feature_1year]; X_5year = df_5year[feature_5year]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']
X_train_1, X_test_1, y_train_1, y_test_1 = train_test_split(X_1year, y_1year, test_size=0.2, random_state=42)
X_train_5, X_test_5, y_train_5, y_test_5 = train_test_split(X_5year, y_5year, test_size=0.2, random_state=42)

# 1-Y
param_dist = {
    'n_estimators': [100, 500, 1000, 1500],  
    'max_depth': [None], 
    'min_samples_split': [5,7,9],  
    'min_samples_leaf': [1, 2, 4],  
    'bootstrap': [True, False]}
rf = RandomForestRegressor(random_state=42)
random_search = RandomizedSearchCV(
    estimator=rf,
    param_distributions=param_dist,
    n_iter=50,  # Numero di combinazioni da testare
    cv=3,  # Numero di fold per la cross-validation
    scoring='r2',  # Metodologia di scoring
    verbose=2,  # Livello di verbosità
    random_state=42,  # Per riproducibilità
    n_jobs=-1 )
random_search.fit(X_train_1, y_train_1)
print(f"Best parameters: {random_search.best_params_}")
print(f"Best R² Score: {random_search.best_score_}")

# 5-Y
param_dist = {
    'n_estimators': [100, 500, 1000, 1500], 
    'max_depth': [None], 
    'min_samples_split': [5, 7, 9],  
    'min_samples_leaf': [1, 2, 4], 
    'bootstrap': [True, False]}
rf = RandomForestRegressor(random_state=42)
random_search = RandomizedSearchCV(
    estimator=rf,
    param_distributions=param_dist,
    n_iter=50,  # Numero di combinazioni da testare
    cv=3,  # Numero di fold per la cross-validation
    scoring='r2',  # Metodologia di scoring
    verbose=2,  # Livello di verbosità
    random_state=42,  # Per riproducibilità
    n_jobs=-1 )
random_search.fit(X_train_5, y_train_5)
print(f"Best parameters: {random_search.best_params_}")
print(f"Best R² Score: {random_search.best_score_}")




          
