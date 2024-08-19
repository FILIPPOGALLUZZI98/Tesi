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
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error,  r2_score
from sklearn.metrics import accuracy_score
from xgboost import XGBClassifier
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
from sklearn.feature_selection import RFE
from xgboost import XGBRegressor

gm = pd.read_csv(datadir+'gws_migr.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']
df_1year = gm[gm['interval'] == 1]; df_5year = gm[gm['interval'] == 5]
X_1year = df_1year[features]; X_5year = df_5year[features]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']

X_train_1, X_test_1, y_train_1, y_test_1 = train_test_split(X_1year, y_1year, test_size=0.2, random_state=42)
X_train_5, X_test_5, y_train_5, y_test_5 = train_test_split(X_5year, y_5year, test_size=0.2, random_state=42)


xgb_model = XGBRegressor(random_state=42)
xgb_model.fit(X_train_1, y_train_1)
y_pred_xgb = xgb_model.predict(X_test_1)
mse_xgb = mean_squared_error(y_test_1, y_pred_xgb)
r2_xgb = r2_score(y_test_1, y_pred_xgb)
print(f"XGBoost - MSE: {mse_xgb}, R²: {r2_xgb}")

from scipy.stats import uniform
param_dist = {
    'n_estimators': [100, 200, 300, 400, 500],
    'learning_rate':  uniform(0.01, 0.1),  # Distribuzione uniforme per la ricerca casuale
    'max_depth': [None],
    'subsample': [0.8, 1],
    'colsample_bytree': [0.8, 1]}

# RandomizedSearchCV
random_search = RandomizedSearchCV(
    estimator=XGBRegressor(random_state=42),
    param_distributions=param_dist,
    n_iter=30,  
    cv=3,
    scoring='r2',
    verbose=1,
    random_state=42)
random_search.fit(X_train_1, y_train_1)

print(f"Best parameters: {random_search.best_params_}")
print(f"Best R² Score: {random_search.best_score_}")

















