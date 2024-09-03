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


ge = pd.read_csv(datadir+'gws_events.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']

X = ge[features]; y = ge['count']

# scaler = StandardScaler()
# X = scaler.fit_transform(X)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=31)



rf = RandomForestRegressor(random_state=31)
rf.fit(X_train, y_train)
y_pred = rf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print(f"Mean Squared Error (MSE): {mse}")
print(f"R² Score: {r2}")

################################################################################
####  FEATURE ENGINEERING  ####

ge = pd.read_csv(datadir+'gws_events.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']

ge['interaction_1'] = ge['n_gws_avg5'] / (ge['gws_logret5'] + 1e-5)
ge['interaction_2'] = ge['n_gws_avg10'] - ge['n_gws_avg5']
ge['interaction_3'] = ge['gws_anomalies10'] - ge['gws_anomalies5']
ge['interaction_4'] = ge['CV5'] / (ge['n_gws_avg5'] + 1e-5)
ge['interaction_5'] = ge['CV10'] / (ge['n_gws_avg10'] + 1e-5)
ge['interaction_6'] = ge['gws_logret10'] - ge['gws_logret5']
ge['interaction_7'] = (ge['n_value'] ** 2) * (ge['gws_anomalies'] ** 2)
ge['interaction_8'] = ge['n_gws_avg10'] - ge['n_gws_avg5']
ge['interaction_9'] = ge['gws_logret5'] - ge['gws_logret']

poly = PolynomialFeatures(degree=2, include_bias=False)
X_poly = poly.fit_transform(ge[features])
poly_features = poly.get_feature_names_out(features)
df_poly = pd.DataFrame(X_poly, columns=poly_features)
df_poly = df_poly.iloc[:, 12:]
ge = pd.concat([ge.reset_index(drop=True), df_poly.reset_index(drop=True)], axis=1)

features = ge.columns.tolist()
for item in ['year', 'country', 'region', 'type','conflicts','n_confl', 'n_count','value', 'pop','count', 'value_t',
              'n_gws_avg1','gws_std1','gws_std5','gws_std10','mean_region','std']:
    features.remove(item)

X = ge[features]; y = ge['count']
scaler = StandardScaler()
X = scaler.fit_transform(X)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=16)

rf = RandomForestRegressor(random_state=31)
rf.fit(X_train, y_train)
y_pred = rf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print("1 year interval:")
print(f"Mean Squared Error (MSE): {mse}")
print(f"R² Score: {r2}")


################################################################################
####  FEATURE IMPORTANCE  ####
importances = rf.feature_importances_
indices = np.argsort(importances)[::-1]
top_features = [features[i] for i in indices[:6]]
top_importances = importances[indices[:6]]
print(f"Top X variabili importanti per il dataset di 1 anno: {top_features}")

X = ge[top_features]; y = ge['count']
scaler = StandardScaler()
X = scaler.fit_transform(X)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=16)


################################################################################
####  OTTIMIZZAZIONE E CV  ####

# Definizione dello spazio dei parametri
param_grid = {
    'n_estimators': [50, 100, 150, 200],
    'max_depth': [None],
    'min_samples_split': [10, 12, 14],
    'min_samples_leaf': [4, 5, 6],
    'max_features': ['sqrt']}

# GridSearchCV
rf = RandomForestRegressor(random_state=17)
grid_search = GridSearchCV(
    estimator=rf,
    param_grid=param_grid,
    cv=5,  # 5-fold cross-validation
    n_jobs=-1,  # Usa tutti i core della CPU disponibili
    scoring='neg_mean_squared_error')
grid_search.fit(X_train, y_train)
best_params = grid_search.best_params_
print(f"Migliori parametri: {best_params}")
y_pred_optimized = grid_search.predict(X_test)
mse_optimized = mean_squared_error(y_test, y_pred_optimized)
r2_optimized = r2_score(y_test, y_pred_optimized)
print("1 year interval after optimization:")
print(f"Mean Squared Error (MSE): {mse_optimized}")
print(f"R² Score: {r2_optimized}")


from sklearn.model_selection import cross_val_score
rf = RandomForestRegressor(random_state=31)

# Esecuzione della cross-validation per il dataset a 1 anno
scores = cross_val_score(rf, X_train, y_train, cv=5, scoring='neg_mean_squared_error')

# Calcolo della media e della deviazione standard delle metriche di valutazione
mean_score = np.mean(scores)
std_score = np.std(scores)

print(f"Cross-Validation Scores per il dataset di 1 anno (neg MSE): {scores}")
print(f"Media dei Cross-Validation Scores (neg MSE): {mean_score}")
print(f"Deviazione standard dei Cross-Validation Scores (neg MSE): {std_score}")


from sklearn.inspection import PartialDependenceDisplay
features_to_plot = top_features[:6] 

rf = RandomForestRegressor(random_state=31)
rf.fit(X_train, y_train)

# PDPs for the 1-year dataset
fig, ax = plt.subplots(2, 3, figsize=(15, 10))

# Use the 'features' parameter to specify the feature names or indices
PartialDependenceDisplay.from_estimator(rf, X_train, features_to_plot, ax=ax, feature_names=top_features) 

plt.suptitle('Partial Dependence Plots')
plt.show()










































