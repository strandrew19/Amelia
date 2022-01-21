import os
import numpy as np
import pandas as pd
import feather
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor

#Read in Sample
AMELIA = feather.read_dataframe("01_sample.feather")

#Split x and y vars (also drop personal ID identifier)
x_var = AMELIA.iloc[:,2:].drop(columns = "Personal_ID")
y_var = AMELIA.iloc[:,0]

#Creat Train/Test split for x and y
x_train, x_test, y_train, y_test = train_test_split(x_var, y_var, test_size = 0.25, random_state = 420)

#Scale x var for train and test
sc = StandardScaler()

x_train = sc.fit_transform(x_train)
x_test = sc.fit_transform(x_test)
x_train = pd.DataFrame(x_train, columns=x_var.columns)
x_test = pd.DataFrame(x_test, columns=x_var.columns)

#Run Multi-layer Perceptron regressor
regr = MLPRegressor(random_state=420, max_iter=500, activation = "relu", solver='lbfgs').fit(x_train, y_train)
predictions = regr.predict(x_test)

#Replace negative outputs with 0
predictions = np.where(predictions < 0, 0, predictions)

#Generate Score Value
print(regr.score(x_test, y_test))

# import seaborn as sns
# import matplotlib.pyplot as plt

#display distribution in kernel density plot
#sns.kdeplot(data=predictions, bw_adjust=.2)

#display first 30 predictions vs acutual values
# df_temp = pd.DataFrame({'Actual': y_test, 'Predicted': predictions})
# df_temp = df_temp.head(30)

# df_temp.plot(kind='bar',figsize=(10,6))
# plt.grid(which='major', linestyle='-', linewidth='0.5', color='green')
# plt.grid(which='minor', linestyle=':', linewidth='0.5', color='black')
# plt.show()
