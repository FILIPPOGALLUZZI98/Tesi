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

df = pd.read_csv(datadir+'gws_migr.csv')
df = df.dropna(subset=['gws_logret10'])

X = df[['n_value', 'n_gws_avg1', 'n_gws_avg5', 'n_gws_avg10', 'gws_logret', 
        'gws_logret5', 'gws_logret10', 'gws_anomalies', 'gws_anomalies5', 
        'gws_anomalies10', 'CV1', 'CV5', 'CV10']]
y = df['n_migr']

# Suddivisione in training e test set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Inizializzazione e addestramento del modello
rf = RandomForestRegressor(n_estimators=100, random_state=42)
rf.fit(X_train, y_train)

# Previsione e valutazione del modello
y_pred = rf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print(f'MSE: {mse}, R^2: {r2}')

# Importanza delle feature
importances = rf.feature_importances_
feature_importance = pd.DataFrame({'feature': X.columns, 'importance': importances})
feature_importance = feature_importance.sort_values(by='importance', ascending=False)
print(feature_importance)

