import os
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPRegressor
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestRegressor

top_wd = os.getcwd()
print(top_wd)
feather_dir = top_wd + "\\..\\data\\Feather Samples"

os.chdir(feather_dir)
AMELIA = pd.read_feather("01_SRS_Base_sample.feather")

def cata_encode(dataframe):
    """Since get_dummies has not dropped the extra column, it must be done explicitly (Avoids Multicolinearity)"""
    #Cities will be dropped since there are too many and they are not considered relevant for our analysis
    #4: Other inactive person 
    Work_Columns = {"Work_Status_1.0":"At Work", "Work_Status_2.0":"Unemployed", "Work_Status_3.0":"Retired"}
    #5: ISCED5 or ISCED6
    Highest_ISCED = {"Highest_ISCED_1.0":"ISCED 1", "Highest_ISCED_2.0":"ISCED 2", "Highest_ISCED_3.0":"ISCED 3", "Highest_ISCED_4.0":"ISCED 4"}
    #5: Divorced
    Martial_Status = {"Martial_Status_1.0":"Never Married", "Martial_Status_2.0":"Married","Martial_Status_3.0":"Separated","Martial_Status_4.0":"Widowed"}
    #4: Region 4
    Region_ID = {"Regional_ID_1":"Region_1", "Regional_ID_2":"Region_2", "Regional_ID_3":"Region_3"}
    #11: Province 11
    #40: District 40

    dataframe = pd.get_dummies(dataframe, columns=['Work_Status', "Highest_ISCED", "Martial_Status", "Regional_ID", "Province", "District"]).rename(columns=Work_Columns).rename(columns=Highest_ISCED).rename(columns=Martial_Status).rename(columns=Region_ID).drop(columns=["Work_Status_4.0", "Highest_ISCED_5.0", "Martial_Status_5.0", "City.Community", "Regional_ID_4", "Province_11", "District_40"])
    return dataframe

def to_numeric(dataframe):
    sex = {"Male":0,"Female":1}
    dataframe = dataframe.replace({"Sex": sex})
    return dataframe

AMELIA = to_numeric(AMELIA)
AMELIA = cata_encode(AMELIA)

y_var = AMELIA["Person_Income"]
x_var = AMELIA.drop(columns = ["index", "Person_Income", "Personal_ID"])

x_train, x_test, y_train, y_test = train_test_split(x_var, y_var, test_size = 0.25, random_state = 420)

sc = StandardScaler()

x_train = sc.fit_transform(x_train)
x_test = sc.fit_transform(x_test)
x_train = pd.DataFrame(x_train, columns=x_var.columns)
x_test = pd.DataFrame(x_test, columns=x_var.columns)
y_train = y_train

param_grid_mlpr = [
        {
            'hidden_layer_sizes': [(100,), (500, 500,),(750, 250,),(333, 333, 333,),(500, 300, 200,),(700, 200, 100,)],
            'activation' : ['relu'],
            'solver' : ['lbfgs'],
            'alpha': [0.0001, 0.001, 0.01],
            'batch_size':['auto'],
            'max_iter':[500],
            'random_state':[420],
            'verbose':[True]
        }
       ]

clf = GridSearchCV(MLPRegressor(), param_grid_mlpr, cv=10, scoring='neg_mean_squared_error', n_jobs = -1, verbose = 10)
clf.fit(x_train,y_train.ravel())

print("Best parameters set found on development set:")
print(clf.best_params_)

param_grid_rf = [
        {
            'n_estimators': ([1250, 1000, 750, 500]),
            'max_depth': ([1000, 750, 500, 250]),
            'min_samples_split': ([200, 100, 50]),
            'min_samples_leaf': ([5, 10, 15]),
            'verbose':[True]
        }
       ]

regr = GridSearchCV(RandomForestRegressor(), param_grid_rf, cv=10, n_jobs = -1)
regr.fit(x_train,y_train)


print("Best parameters set found on development set:")
print(regr.best_params_)