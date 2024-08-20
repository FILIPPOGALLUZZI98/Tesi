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


ge = pd.read_csv(datadir+'gws_events.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']


#############################################################################################################
#############################################################################################################

X = ge[features]; y = ge['count']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)


# Random Forest
rf = RandomForestRegressor(random_state=42)
rf.fit(X_train, y_train)
y_pred = rf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Results
print("Results:")
print(f"Mean Squared Error (MSE): {mse}")
print(f"R² Score: {r2}")


#############################################################################################################
#############################################################################################################
####  FEATURE IMPORTANCE  ####

importances = rf.feature_importances_
sorted_indices = importances.argsort()
plt.figure(figsize=(8, 5))

# Feature Importance
plt.subplot(2, 1, 1)
plt.barh([features[i] for i in sorted_indices], importances[sorted_indices], color='skyblue')
plt.xlabel('Importance'); plt.ylabel('Variables'); plt.title('Feature Importance')
plt.tight_layout(); plt.show()


#############################################################################################################
#############################################################################################################
####  PARTIAL DEPENDENCE PLOTS  ####

features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']

# PDPs
fig, ax = plt.subplots(figsize=(12, 12))  
display = PartialDependenceDisplay.from_estimator(rf, X_train, features, ax=ax, n_cols=3)
plt.suptitle('Partial Dependence Plots', fontsize=16)
plt.subplots_adjust(top=0.9)
plt.show()


#############################################################################################################
#############################################################################################################
####    ####

# Lista 5 variabili più importanti
top_5_indices = sorted_indices[-5:][::-1]
variables = [features[i] for i in top_5_indices]
print("First variables by importance:"); print(variables)

































