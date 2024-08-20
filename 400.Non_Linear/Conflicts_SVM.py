e = pd.read_csv(datadir+'gws_events.csv')
features = ['n_value', 'n_gws_avg5', 'n_gws_avg10', 'gws_anomalies', 'gws_anomalies5', 'gws_anomalies10',
            'CV1', 'CV5', 'CV10','gws_logret', 'gws_logret5', 'gws_logret10']


X = ge[features]; y = ge['count']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

