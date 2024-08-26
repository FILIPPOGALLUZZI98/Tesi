import pandas as pd
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR
from sklearn.metrics import mean_squared_error, r2_score


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


#############################################################################################################
#############################################################################################################
df_1year = gm[gm['interval'] == 1]; df_5year = gm[gm['interval'] == 5]
X_1year = df_1year[features]; X_5year = df_5year[features]
y_1year = df_1year['n_migr']; y_5year = df_5year['n_migr']

## scaler_1year = StandardScaler()
## X_1year_scaled = scaler_1year.fit_transform(X_1year)
## scaler_5year = StandardScaler()
## X_5year_scaled = scaler_5year.fit_transform(X_5year)

X_1year_train, X_1year_test, y_1year_train, y_1year_test = train_test_split(X_1year, y_1year, test_size=0.2, random_state=42)
X_5year_train, X_5year_test, y_5year_train, y_5year_test = train_test_split(X_5year, y_5year, test_size=0.2, random_state=42)

# Definizione del modello SVM con kernel RBF 
svm_model = SVR(kernel='rbf')
param_grid = {
    'C': [0.1, 1, 10, 100],
    'gamma': ['scale', 0.1, 1, 10]}


# Interval == 1
grid_search_1year = GridSearchCV(svm_model, param_grid, cv=5, scoring='r2', verbose=2)
grid_search_1year.fit(X_1year_train, y_1year_train)
best_model_1year = grid_search_1year.best_estimator_
y_1year_pred = best_model_1year.predict(X_1year_test)
mse_1year = mean_squared_error(y_1year_test, y_1year_pred)
r2_1year = r2_score(y_1year_test, y_1year_pred)
print(f'Intervallo di 1 anno - Mean Squared Error: {mse_1year}')
print(f'Intervallo di 1 anno - R^2 Score: {r2_1year}')
print(f'Intervallo di 1 anno - Best Hyperparameters: {grid_search_1year.best_params_}')

# Interval == 5
grid_search_5year = GridSearchCV(svm_model, param_grid, cv=5, scoring='r2', verbose=2)
grid_search_5year.fit(X_5year_train, y_5year_train)
best_model_5year = grid_search_5year.best_estimator_
y_5year_pred = best_model_5year.predict(X_5year_test)
mse_5year = mean_squared_error(y_5year_test, y_5year_pred)
r2_5year = r2_score(y_5year_test, y_5year_pred)
print(f'Intervallo di 5 anni - Mean Squared Error: {mse_5year}')
print(f'Intervallo di 5 anni - R^2 Score: {r2_5year}')
print(f'Intervallo di 5 anni - Best Hyperparameters: {grid_search_5year.best_params_}')


























