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
from sklearn.preprocessing import PolynomialFeatures
from sklearn.decomposition import PCA


# Montaggi GDrive
import os, sys
from google.colab import drive
drivedir='/content/drive'
drive.mount(drivedir)
os.chdir(drivedir)
datadir=drivedir+'/MyDrive/'

gm = pd.read_csv(datadir+'gws_migr.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']

df_1year = gm[gm['interval'] == 1]; df_5year = gm[gm['interval'] == 5]
X_1year = df_1year[features]; X_5year = df_5year[features]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']


# scaler_1year = StandardScaler()
# X_1year = scaler_1year.fit_transform(X_1year)
# scaler_5year = StandardScaler()
# X_5year = scaler_5year.fit_transform(X_5year)

X_train_1, X_test_1, y_train_1, y_test_1 = train_test_split(X_1year, y_1year, test_size=0.2, random_state=31)
X_train_5, X_test_5, y_train_5, y_test_5 = train_test_split(X_5year, y_5year, test_size=0.2, random_state=31)


# 1- year
rf_1year = RandomForestRegressor(random_state=31)
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

print("1 year interval:")
print(f"Mean Squared Error (MSE): {mse_1}")
print(f"R² Score: {r2_1}")
print("5 years interval:")
print(f"Mean Squared Error (MSE): {mse_5}")
print(f"R² Score: {r2_5}")


################################################################################
####  FEATURE ENGINEERING  ####

gm = pd.read_csv(datadir+'gws_migr.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']
            
gm['interaction_1'] = gm['n_value'] * gm['gws_anomalies']
gm['interaction_2'] = gm['n_gws_avg5'] / (gm['gws_logret5'] + 1e-5)
gm['interaction_3'] = gm['n_gws_avg5'] * gm['gws_anomalies5']
gm['interaction_4'] = gm['n_gws_avg10'] * gm['gws_anomalies10']
gm['interaction_5'] = gm['n_gws_avg10'] - gm['n_gws_avg5']
gm['interaction_6'] = gm['gws_anomalies10'] - gm['gws_anomalies5']
gm['interaction_7'] = gm['CV5'] * gm['gws_anomalies5']
gm['interaction_8'] = gm['CV10'] * gm['gws_anomalies10']
gm['interaction_9'] = gm['CV5'] / (gm['n_gws_avg5'] + 1e-5)
gm['interaction_10'] = gm['CV10'] / (gm['n_gws_avg10'] + 1e-5)
gm['interaction_11'] = gm['gws_logret'] * gm['gws_anomalies']
gm['interaction_12'] = gm['gws_logret5'] * gm['gws_anomalies5']
gm['interaction_13'] = gm['gws_logret10'] * gm['gws_anomalies10']
gm['interaction_14'] = gm['gws_logret10'] - gm['gws_logret5']
gm['interaction_15'] = gm['n_value'] * gm['n_gws_avg5']
gm['interaction_16'] = (gm['n_value'] ** 2) * (gm['gws_anomalies'] ** 2)
gm['interaction_17'] = gm['n_gws_avg5'] * gm['n_gws_avg10']
gm['interaction_18'] = gm['CV1'] * gm['CV10']
gm['interaction_19'] = gm['n_gws_avg10'] - gm['n_gws_avg5']
gm['interaction_20'] = gm['gws_logret5'] - gm['gws_logret']

poly = PolynomialFeatures(degree=2, include_bias=False)
X_poly = poly.fit_transform(gm[features])
poly_features = poly.get_feature_names_out(features)
df_poly = pd.DataFrame(X_poly, columns=poly_features)
df_poly = df_poly.iloc[:, 12:]
gm = pd.concat([gm.reset_index(drop=True), df_poly.reset_index(drop=True)], axis=1)

features = gm.columns.tolist()
for item in ['year', 'country', 'region', 'value', 'population', 'interval','flow', 'pop','migrants', 'value_t', 'n_migr',
              'n_gws_avg1','gws_std1','gws_std5','gws_std10','mean_region','std']:
    features.remove(item)

df_1year = gm[gm['interval'] == 1]; df_5year = gm[gm['interval'] == 5]
X_1year = df_1year[features]; X_5year = df_5year[features]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']

scaler_1year = StandardScaler()
X_1year = scaler_1year.fit_transform(X_1year)
scaler_5year = StandardScaler()
X_5year = scaler_5year.fit_transform(X_5year)

X_train_1, X_test_1, y_train_1, y_test_1 = train_test_split(X_1year, y_1year, test_size=0.2, random_state=31)
X_train_5, X_test_5, y_train_5, y_test_5 = train_test_split(X_5year, y_5year, test_size=0.2, random_state=31)

# 1- year
rf_1year = RandomForestRegressor(random_state=31)
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

print("1 year interval:")
print(f"Mean Squared Error (MSE): {mse_1}")
print(f"R² Score: {r2_1}")
print("5 years interval:")
print(f"Mean Squared Error (MSE): {mse_5}")
print(f"R² Score: {r2_5}")

################################################################################
####  FEATURE IMPORTANCE  ####

importances_1year = rf_1year.feature_importances_
indices_1year = np.argsort(importances_1year)[::-1]
top_6_features_1year = [features[i] for i in indices_1year[:10]]
top_6_importances_1year = importances_1year[indices_1year[:10]]
print(f"Top 6 variabili importanti per il dataset di 1 anno: {top_6_features_1year}")
importances_5year = rf_5year.feature_importances_
indices_5year = np.argsort(importances_5year)[::-1]
top_6_features_5year = [features[i] for i in indices_5year[:10]]
top_6_importances_5year = importances_5year[indices_5year[:10]]
print(f"Top 6 variabili importanti per il dataset di 5 anni: {top_6_features_5year}")































