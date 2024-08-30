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
            
gm['interaction_1'] = gm['n_gws_avg5'] / (gm['gws_logret5'] + 1e-5)
gm['interaction_2'] = gm['n_gws_avg10'] - gm['n_gws_avg5']
gm['interaction_3'] = gm['gws_anomalies10'] - gm['gws_anomalies5']
gm['interaction_4'] = gm['CV5'] / (gm['n_gws_avg5'] + 1e-5)
gm['interaction_5'] = gm['CV10'] / (gm['n_gws_avg10'] + 1e-5)
gm['interaction_6'] = gm['gws_logret10'] - gm['gws_logret5']
gm['interaction_7'] = (gm['n_value'] ** 2) * (gm['gws_anomalies'] ** 2)
gm['interaction_8'] = gm['n_gws_avg10'] - gm['n_gws_avg5']
gm['interaction_9'] = gm['gws_logret5'] - gm['gws_logret']

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


################################################################################
####  OTTIMIZZAZIONE  ####

X_1year = df_1year[top_features_1year]; X_5year = df_5year[top_features_5year]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']

scaler_1year = StandardScaler()
X_1year = scaler_1year.fit_transform(X_1year)
scaler_5year = StandardScaler()
X_5year = scaler_5year.fit_transform(X_5year)

X_train_1, X_test_1, y_train_1, y_test_1 = train_test_split(X_1year, y_1year, test_size=0.2, random_state=17)
X_train_5, X_test_5, y_train_5, y_test_5 = train_test_split(X_5year, y_5year, test_size=0.2, random_state=17)


# Definizione dello spazio dei parametri
# Definizione dello spazio dei parametri
param_grid = {
    'n_estimators': [50, 100, 150, 200],
    'max_depth': [None],
    'min_samples_split': [10, 12, 14],
    'min_samples_leaf': [4, 5, 6],
    'max_features': ['sqrt']}

# GridSearchCV per il dataset di 1 anno
rf_1year = RandomForestRegressor(random_state=17)
grid_search_1year = GridSearchCV(
    estimator=rf_1year,
    param_grid=param_grid,
    cv=5,  # 5-fold cross-validation
    n_jobs=-1,  # Usa tutti i core della CPU disponibili
    scoring='neg_mean_squared_error')
grid_search_1year.fit(X_train_1, y_train_1)
best_params_1year = grid_search_1year.best_params_
print(f"Migliori parametri per il dataset di 1 anno: {best_params_1year}")
y_pred_optimized_1year = grid_search_1year.predict(X_test_1)
mse_optimized_1year = mean_squared_error(y_test_1, y_pred_optimized_1year)
r2_optimized_1year = r2_score(y_test_1, y_pred_optimized_1year)
print("1 year interval after optimization:")
print(f"Mean Squared Error (MSE): {mse_optimized_1year}")
print(f"R² Score: {r2_optimized_1year}")


# GridSearchCV per il dataset di 5 anni
rf_5year = RandomForestRegressor(random_state=17)
grid_search_5year = GridSearchCV(
    estimator=rf_5year,
    param_grid=param_grid,
    cv=5,
    n_jobs=-1,
    scoring='neg_mean_squared_error')
grid_search_5year.fit(X_train_5, y_train_5)
best_params_5year = grid_search_5year.best_params_
print(f"Migliori parametri per il dataset di 5 anni: {best_params_5year}")
y_pred_optimized_5year = grid_search_5year.predict(X_test_5)
mse_optimized_5year = mean_squared_error(y_test_5, y_pred_optimized_5year)
r2_optimized_5year = r2_score(y_test_5, y_pred_optimized_5year)
print("5 years interval after optimization:")
print(f"Mean Squared Error (MSE): {mse_optimized_5year}")
print(f"R² Score: {r2_optimized_5year}")

from sklearn.model_selection import cross_val_score
rf_1year = RandomForestRegressor(random_state=31)

# Esecuzione della cross-validation per il dataset a 1 anno
scores_1year = cross_val_score(rf_1year, X_train_1, y_train_1, cv=5, scoring='neg_mean_squared_error')

# Calcolo della media e della deviazione standard delle metriche di valutazione
mean_score_1year = np.mean(scores_1year)
std_score_1year = np.std(scores_1year)

print(f"Cross-Validation Scores per il dataset di 1 anno (neg MSE): {scores_1year}")
print(f"Media dei Cross-Validation Scores (neg MSE): {mean_score_1year}")
print(f"Deviazione standard dei Cross-Validation Scores (neg MSE): {std_score_1year}")

# Ripetere la stessa procedura per il dataset a 5 anni
rf_5year = RandomForestRegressor(random_state=42)
scores_5year = cross_val_score(rf_5year, X_train_5, y_train_5, cv=5, scoring='neg_mean_squared_error')

mean_score_5year = np.mean(scores_5year)
std_score_5year = np.std(scores_5year)

print(f"Cross-Validation Scores per il dataset di 5 anni (neg MSE): {scores_5year}")
print(f"Media dei Cross-Validation Scores (neg MSE): {mean_score_5year}")
print(f"Deviazione standard dei Cross-Validation Scores (neg MSE): {std_score_5year}")




















