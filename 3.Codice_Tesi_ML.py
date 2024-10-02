# Import the libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.model_selection import train_test_split, RandomizedSearchCV, KFold, cross_val_score, StratifiedKFold
from sklearn.metrics import mean_squared_error, r2_score, accuracy_score, make_scorer, mean_absolute_error, classification_report, roc_auc_score, f1_score, confusion_matrix
import xgboost
from xgboost import XGBClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import RFE
from imblearn.over_sampling import RandomOverSampler
from sklearn.utils.class_weight import compute_sample_weight
import seaborn as sns

# Mount GDrive
import os, sys
from google.colab import drive
drivedir='/content/drive'
drive.mount(drivedir)
os.chdir(drivedir)
datadir=drivedir+'/MyDrive/'


# Upload datasets

gm = pd.read_csv(datadir+'gws_migr.csv')  ## Migrations
ge = pd.read_csv(datadir+'gws_events.csv')  ## Conflicts

gov = pd.read_csv(datadir+'Govern.csv')  ## Level of governance
inc = pd.read_csv(datadir+'income.csv')  ## Income

# Operations on the governance dataset
gov = gov.iloc[:-5]
gov = gov.drop(gov.columns[[0, 1, 3]], axis=1)
gov.columns = ['country', 'govern']
gov['govern'] = pd.to_numeric(gov['govern'], errors='coerce')
gov = gov.sort_values(by='govern', ascending=False)
gov = gov.dropna()

# Select features
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']
scaler = StandardScaler()

# Create 3 classes for high, medium, and low governance
gov1 = gov.iloc[:71]; name_gov1 = gov1['country'].unique()  ## High Governance
gov2 = gov.iloc[71:142] ; name_gov2 = gov2['country'].unique()  ## Medium Governance
gov3 = gov.iloc[142:213]; name_gov3 = gov3['country'].unique()  ## Low Governance

# Create 4 classes for high, upper-middle, middle-low, low income
inc1 = inc[inc['income']=='High income']
inc2 = inc[inc['income']=='Upper middle income']
inc3 = inc[inc['income']=='Lower middle income']
inc4 = inc[inc['income']=='Low income']
name_inc1 = inc1['country'].unique(); name_inc2 = inc2['country'].unique()
name_inc3 = inc3['country'].unique(); name_inc4 = inc4['country'].unique()





################################################################################
####  MIGRATIONS  ####
################################################################################


# Classi con nomi per ciclo for
anni = [1, 5]
name_global=gm['country'].unique()
class_names = {'name_global':name_global,'name_inc1': name_inc1,'name_inc2': name_inc2,'name_inc3': name_inc3,
    'name_inc4': name_inc4,'name_gov1': name_gov1,'name_gov2': name_gov2,'name_gov3': name_gov3}
classi = [(name_global, 'Global'),(name_inc1, 'High Income'), (name_inc2, 'Upper Middle Income'), (name_inc3, 'Middle Low Income'),
          (name_inc4, 'Low Income'), (name_gov1, 'High Governance'), (name_gov2, 'Medium Governance'),
          (name_gov3, 'Low Governance')]
gm[features] = gm[features].apply(pd.to_numeric, errors='coerce')

####  RANDOM FOREST  ####
results = []

for anno in anni:
    for classe, class_name in classi:
        gm_y = gm[gm['interval'] == anno]  ## Select the interval of migrations
        gm_y = gm_y[gm_y['country'].isin(classe)]  ## Select the class
        gm_y = gm_y.reset_index(drop=True)
        X = gm_y[features]; y = gm_y['n_migr']  ## Select the features and the target

        if X.shape[0] < 50:
            print(f"Pochi dati per {anno} e {class_name}: {X.shape[0]}")
            continue

        # Division into train-test-validation (30% for feature selection, 60% for hyperparameter tuning, 10% for validation).
        X_train, X_t, y_train, y_t = train_test_split(X, y, test_size=0.4, random_state=10)
        X_test, X_cv, y_test, y_cv = train_test_split(X_t, y_t, test_size=0.15, random_state=68)
        train = X_train.shape[0]; test = X_test.shape[0]; cv = X_cv.shape[0]
        train_test_valid = f'{train} / {test} / {cv}'  ## Ratio among data

        # Best 6 variables
        rf = RandomForestRegressor(random_state=4)
        rf.fit(X_train, y_train)
        importances = rf.feature_importances_
        indices = np.argsort(importances)[::-1]
        top_features = [features[i] for i in indices[:6]]

        # Tuning Hyperparameters
        parametri = {
            'n_estimators': [i for i in range(10, 400, 10)],
            'max_depth': [i for i in range(2, 10)],
            'min_samples_split': [i for i in range(3, 10)],
            'min_samples_leaf': [i for i in range(3, 10)],
            'max_features': ['sqrt'],
            'bootstrap': [True]}

        # Randomized SearchCV
        cv = KFold(n_splits=10, shuffle=True, random_state=43)
        random_search = RandomizedSearchCV(estimator=rf, param_distributions=parametri,
                                           n_iter=50, cv=cv, n_jobs=-1, scoring='r2',
                                           verbose=0, random_state=16)
        X_test = X_test[top_features]
        random_search.fit(X_test, y_test)
        rf_best = random_search.best_estimator_

        # Predictions on the validation set
        X_cv = X_cv[top_features]
        rf_best.fit(X_test, y_test)
        y_pred_cv = rf_best.predict(X_cv)
        mse_cv = mean_squared_error(y_cv, y_pred_cv)
        r2_cv = r2_score(y_cv, y_pred_cv)

        # Creation of a dictionary for the results table
        results.append({'Anno': anno,'Classe': class_name,'MSE Validation': mse_cv,
                        'R2 Validation': r2_cv,'Migliori Variabili': top_features,
                        'Train/Test/Validation':train_test_valid})

# Table of results
results_df = pd.DataFrame(results)
results_df[' '] = results_df['Classe'] + ', ' + results_df['Anno'].astype(str) + '-year'
df_summary = results_df[[' ', 'MSE Validation', 'R2 Validation',  'Train/Test/Validation']]
df_summary = df_summary.set_index(' ')
df_summary = df_summary.rename(columns={'MSE Validation': 'MSE','R2 Validation': 'R2'})
table = df_summary.style.set_properties(**{'text-align': 'center'})
table = table.format(precision=4)
table


###############################################################################
####  CONFLICTS  ####
################################################################################
# Classes with names for cycle
name_global=ge['country'].unique()
class_names = {'name_global':name_global, 'name_inc1': name_inc1,'name_inc2': name_inc2,'name_inc3': name_inc3,'name_inc4': name_inc4,
               'name_gov1': name_gov1,'name_gov2': name_gov2,'name_gov3': name_gov3}
classi = [(name_global, 'Global'),(name_inc1, 'High Income'), (name_inc2, 'Upper Middle Income'), (name_inc3, 'Middle Low Income'),
          (name_inc4, 'Low Income'), (name_gov1, 'High Governance'), (name_gov2, 'Medium Governance'),
          (name_gov3, 'Low Governance')]

# Conversion of 'count' into a binary variable
ge['count'] = (ge['count'] > 0).astype(int)

# Select the last three years to evaluate the goodness of the model in time
ge_prima = ge[ge['year'] < 2017]
ge_dopo = ge[ge['year'] >= 2017]

####  RANDOM FOREST  ####
results = []
for classe, class_name in classi:
    ge_y = ge_prima[ge_prima['country'].isin(classe)]  ## Select the class
    ge_y = ge_y.reset_index(drop=True)
    ge_w = ge_dopo[ge_dopo['country'].isin(classe)]  ## Select the class
    ge_w = ge_w.reset_index(drop=True)

    # Subdivision into train-test-validation
    X = ge_y[features]; y = ge_y['count']
    X_train, X_t, y_train, y_t = train_test_split(X, y, test_size=0.7, random_state=17)  ## (30% for feature selection, 50% for tuning, and 20% validation)
    X_test, X_cv, y_test, y_cv = train_test_split(X_t, y_t, test_size=0.28, random_state=80)
    train_size = X_train.shape[0]; test_size = X_test.shape[0]; cv_size = X_cv.shape[0]; prev = ge_w.shape[0]
    data_size = f'{train_size} / {test_size} / {cv_size} / {prev}'  ## Ratio among sets

    # Weight calculation for different classes
    y_train_1 = (y_train == 1).sum(); y_test_1 = (y_test == 1).sum()
    y_train_0 = (y_train == 0).sum(); y_test_0 = (y_test == 0).sum()
    count_1 = y_train_1 + y_test_1; count_0 = y_train_0 + y_test_0
    ratio = (count_1 / count_0)
    rapp = (count_1 / count_0)*100  ## Percentage of occurrences in the selected class

    # Selection of the best 6 variables
    rf = RandomForestClassifier(class_weight={0: 1, 1: 1/ratio}, random_state=29)
    rf.fit(X_train, y_train)
    importances = rf.feature_importances_
    indices = np.argsort(importances)[::-1]
    top_features = [features[i] for i in indices[:6]]

    # Hyperparameter tuning
    parametri = {
        'n_estimators': [i for i in range(100, 1001, 100)],
        'max_depth': [i for i in range(6, 21, 1)],
        'min_samples_split': [i for i in range(2, 16, 1)],
        'min_samples_leaf': [i for i in range(2, 16, 1)],
        'bootstrap': [True]}

    # RandomSearchCV
    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=36)
    random_search = RandomizedSearchCV(estimator=rf, param_distributions=parametri,
        cv=cv, scoring='average_precision', n_jobs=-1, verbose=0, n_iter=20,
        random_state=2)
    X_test = X_test[top_features]
    random_search.fit(X_test, y_test)
    best_rf = random_search.best_estimator_

    # Predictions on the validation set
    X_cv = X_cv[top_features]
    best_rf.fit(X_test,y_test)
    y_pred_cv = best_rf.predict(X_cv)
    report_cv = classification_report(y_cv, y_pred_cv, output_dict=True)

    # Predictions over 2017-2019 data
    X_w = ge_w[top_features]; y_w = ge_w['count']
    y_pred_w = best_rf.predict(X_w)
    report_w = classification_report(y_w, y_pred_w, output_dict=True)

    # Results
    results.append({'Classe': class_name, 'Validation': report_cv,'2017-2019': report_w,
                    'Migliori Variabili': top_features,'Migliori Parametri': random_search.best_params_,
                    'Rapp': rapp,'Data Size': data_size})


# Table of the resulst
def extract_metrics_combined(results):
    classi = []; dataset_type = []; precision_0 = []; recall_0 = []
    f1_score_0 = []; precision_1 = []; recall_1 = []; f1_score_1 = []
    weight_precision = []; weight_recall = []; weight_f1_score = []; accuracy = []
    rapp_list = []; data_size_list = []

    for result in results:
        for set_name in ['Validation', '2017-2019']:
            classi.append(result['Classe'])
            dataset_type.append(set_name)
            precision_0.append(result[set_name]['0']['precision'])
            precision_1.append(result[set_name]['1']['precision'])
            weight_precision.append(result[set_name]['weighted avg']['precision'])
            f1_score_0.append(result[set_name]['0']['f1-score'])
            f1_score_1.append(result[set_name]['1']['f1-score'])
            weight_f1_score.append(result[set_name]['weighted avg']['f1-score'])
            recall_0.append(result[set_name]['0']['recall'])
            recall_1.append(result[set_name]['1']['recall'])
            weight_recall.append(result[set_name]['weighted avg']['recall'])
            accuracy.append(result[set_name]['accuracy'])
            rapp_list.append(result['Rapp'])
            data_size_list.append(result['Data Size'])

    df = pd.DataFrame({'Classe': classi,'Dataset': dataset_type,'Precision No Conflicts': precision_0,
                       'Precision Conflicts': precision_1, 'Weighted Precision':weight_precision,
                       'F1-score No Conflicts': f1_score_0, 'F1-score Conflicts': f1_score_1,'Weighted F1-score':weight_f1_score,
                       'Recall No Conflicts': recall_0, 'Recall Conflicts': recall_1, 'Weighted Recall':weight_recall,
                       'Rapp': rapp_list,'Data Size': data_size_list})
    return df

df_combined = extract_metrics_combined(results)
df_combined[' '] = df_combined['Classe'] + ', ' + df_combined['Dataset']
df_combined = df_combined.drop(columns=['Classe', 'Dataset'])
df_combined.set_index(' ', inplace=True)
df_combined = df_combined.round(4)
df_combined = df_combined.rename(columns={
    'Precision No Conflicts': 'Precision (No Conflicts)',
    'Precision Conflicts': 'Precision (Conflicts)',
    'Weighted Precision': 'Weighted Precision',
    'F1-score No Conflicts': 'F1-score (No Conflicts)',
    'F1-score Conflicts': 'F1-score (Conflicts)',
    'Weighted F1-score': 'Weighted F1-score',
    'Recall No Conflicts': 'Recall (No Conflicts)',
    'Recall Conflicts': 'Recall (Conflicts)',
    'Weighted Recall': 'Weighted Recall',
    'Rapp': 'Class Ratio (%)',
    'Data Size': 'Data Size (Train/Test/CV/Prev)'})

table = df_combined.style.set_properties(**{'text-align': 'center'})
table = table.set_table_styles([{'selector': 'th', 'props': [('text-align', 'center')] }])
table = table.format(precision=2)
table











