import pandas as pd
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.decomposition import PCA
from sklearn.preprocessing import PolynomialFeatures
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np


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

scaler_1year = StandardScaler()
X_1year_scaled = scaler_1year.fit_transform(X_1year)
scaler_5year = StandardScaler()
X_5year_scaled = scaler_5year.fit_transform(X_5year)

X_1year_train, X_1year_test, y_1year_train, y_1year_test = train_test_split(X_1year_scaled, y_1year, test_size=0.2, random_state=31)
X_5year_train, X_5year_test, y_5year_train, y_5year_test = train_test_split(X_5year_scaled, y_5year, test_size=0.2, random_state=31)

# Senza StandardScaler()
# X_1year_train, X_1year_test, y_1year_train, y_1year_test = train_test_split(X_1year, y_1year, test_size=0.2, random_state=42)
# X_5year_train, X_5year_test, y_5year_train, y_5year_test = train_test_split(X_5year, y_5year, test_size=0.2, random_state=42)

# Definizione del modello SVM con kernel RBF 
svm_model = SVR(kernel='rbf')
param_grid = {
    'C': [0.1, 0.5, 1, 3, 5, 7, 10],
    'gamma': ['scale',0.1, 0.5, 1, 3, 5, 7, 10]}

# Interval == 1
grid_search_1year = GridSearchCV(svm_model, param_grid, cv=5, scoring='r2', verbose=1)
grid_search_1year.fit(X_1year_train, y_1year_train)
best_model_1year = grid_search_1year.best_estimator_
y_1year_pred = best_model_1year.predict(X_1year_test)
mse_1year = mean_squared_error(y_1year_test, y_1year_pred)
r2_1year = r2_score(y_1year_test, y_1year_pred)
print(f'Intervallo di 1 anno - Mean Squared Error: {mse_1year}')
print(f'Intervallo di 1 anno - R^2 Score: {r2_1year}')
print(f'Intervallo di 1 anno - Best Hyperparameters: {grid_search_1year.best_params_}')

# Interval == 5
grid_search_5year = GridSearchCV(svm_model, param_grid, cv=5, scoring='r2', verbose=1)
grid_search_5year.fit(X_5year_train, y_5year_train)
best_model_5year = grid_search_5year.best_estimator_
y_5year_pred = best_model_5year.predict(X_5year_test)
mse_5year = mean_squared_error(y_5year_test, y_5year_pred)
r2_5year = r2_score(y_5year_test, y_5year_pred)
print(f'Intervallo di 5 anni - Mean Squared Error: {mse_5year}')
print(f'Intervallo di 5 anni - R^2 Score: {r2_5year}')
print(f'Intervallo di 5 anni - Best Hyperparameters: {grid_search_5year.best_params_}')


##########################################################################################
##########################################################################################

gm = pd.read_csv(datadir+'gws_migr.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']
scaler = StandardScaler()
gm[features] = scaler.fit_transform(gm[features])

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

lista = ['interaction_1', 'interaction_2', 'interaction_3', 'interaction_4', 'interaction_5', 'interaction_6', 
 'interaction_7', 'interaction_8', 'interaction_9', 'interaction_10', 'interaction_11', 'interaction_12', 
 'interaction_13', 'interaction_14', 'interaction_15', 'interaction_16', 'interaction_17', 'interaction_18', 
 'interaction_19', 'interaction_20']
features = features + lista

pca = PCA(n_components=0.90)
X_pca = pca.fit_transform(gm[features + list(df_poly.columns)])
df_pca = pd.DataFrame(X_pca, columns=[f'PC{i+1}' for i in range(X_pca.shape[1])])
df = pd.concat([df_pca, gm[['n_migr', 'interval']].reset_index(drop=True)], axis=1)

# Separazione dei dati per intervallo temporale
df_1year = df[df['interval'] == 1]
df_5year = df[df['interval'] == 5]

X_1year = df_1year.drop(['n_migr', 'interval'], axis=1); y_1year = df_1year['n_migr']
X_5year = df_5year.drop(['n_migr', 'interval'], axis=1); y_5year = df_5year['n_migr']

X_1year_train, X_1year_test, y_1year_train, y_1year_test = train_test_split(X_1year, y_1year, test_size=0.2, random_state=31)
X_5year_train, X_5year_test, y_5year_train, y_5year_test = train_test_split(X_5year, y_5year, test_size=0.2, random_state=31)

# Definizione del modello SVM con kernel RBF 
svm_model = SVR(kernel='rbf')
param_grid = {
    'C': [0.1, 0.5, 1, 3, 5, 7, 10],
    'gamma': ['scale',0.1, 0.5, 1, 3, 5, 7, 10]}

# Interval == 1
grid_search_1year = GridSearchCV(svm_model, param_grid, cv=5, scoring='r2', verbose=1)
grid_search_1year.fit(X_1year_train, y_1year_train)
best_model_1year = grid_search_1year.best_estimator_
y_1year_pred = best_model_1year.predict(X_1year_test)
mse_1year = mean_squared_error(y_1year_test, y_1year_pred)
r2_1year = r2_score(y_1year_test, y_1year_pred)
print(f'Intervallo di 1 anno - Mean Squared Error: {mse_1year}')
print(f'Intervallo di 1 anno - R^2 Score: {r2_1year}')
print(f'Intervallo di 1 anno - Best Hyperparameters: {grid_search_1year.best_params_}')

# Interval == 5
grid_search_5year = GridSearchCV(svm_model, param_grid, cv=5, scoring='r2', verbose=1)
grid_search_5year.fit(X_5year_train, y_5year_train)
best_model_5year = grid_search_5year.best_estimator_
y_5year_pred = best_model_5year.predict(X_5year_test)
mse_5year = mean_squared_error(y_5year_test, y_5year_pred)
r2_5year = r2_score(y_5year_test, y_5year_pred)
print(f'Intervallo di 5 anni - Mean Squared Error: {mse_5year}')
print(f'Intervallo di 5 anni - R^2 Score: {r2_5year}')
print(f'Intervallo di 5 anni - Best Hyperparameters: {grid_search_5year.best_params_}')

























