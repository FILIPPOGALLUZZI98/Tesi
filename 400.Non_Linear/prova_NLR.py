import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split, cross_val_score, KFold
from sklearn.metrics import mean_squared_error

# Montaggio GDrive
import os, sys
from google.colab import drive
drivedir='/content/drive'
drive.mount(drivedir)
os.chdir(drivedir)
datadir=drivedir+'/MyDrive/'

file_path = datadir +'data.csv'
df = pd.read_csv(file_path)


# Plot dei grafici
features = ['value','gws_anomalies10', 'gws_std10', 'CV10']  ## Sono i nomi delle colonne del dataset
titles = ['Value', 'Anomalies','STD', 'Coeff. Variazione']
plt.figure(figsize=(12,10))
for i, feature in enumerate(features):
    plt.subplot(2, 2, i + 1)
    plt.scatter(df[feature], df['count'], s = 1, color = 'k')
    plt.title(titles[i]); plt.ylabel('numero conflitti')
plt.tight_layout(); plt.show()


# Creiamo i vettori dei dati
X = df[['value', 'gws_anomalies10']].values
y = df['count'].values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1)  ## 90-10,  random_state=42

# Creazione del modello
model = Sequential([
Dense(units = 10, activation='relu'),
Dense(units = 10, activation='relu'),
Dense(units = 1,  activation='softplus')])

# Compilazione del modello
model.compile(optimizer='adam', loss='poisson', metrics=['mae'])
# Ottimizzazione = Adam

# Addestramento del modello
model.fit(X_train, y_train, epochs=500, batch_size=64, validation_split=0.2)
# epochs = numero di iterazioni
# batch_size = numero di campioni da utilizzare per aggiornare i pesi del modello durante l'addestramento

# Valutazione del modello sul set di test
loss, mae = model.evaluate(X_test, y_test)
print(f'Loss: {loss}, MAE: {mae}')













































