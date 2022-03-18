import os

import feather
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPRegressor
from sklearn.preprocessing import StandardScaler

# Set top level directory, path for reading feather and output file for the results
top_wd = os.getcwd()
print(top_wd)
feather_dir = top_wd + "\\data\\feather_samples"
os.makedirs(top_wd + "\\results", exist_ok=True)
results_dir = top_wd + "\\results"
holdout_dir = top_wd + "\\data"

# Function for numeric conversion of sex variable


def to_numeric(dataframe):
    sex = {"Male": 0, "Female": 1}
    dataframe = dataframe.replace({"Sex": sex})
    return dataframe

# Function to hotcode all catagorical variables


def cata_encode(dataframe):
    """Since get_dummies has not dropped the extra column, it must be done explicitly (Avoids Multicolinearity)"""
    # Cities will be dropped since there are too many and they are not considered relevant for our analysis
    # 4: Other inactive person
    Work_Columns = {"Work_Status_1.0": "At Work",
                    "Work_Status_2.0": "Unemployed", "Work_Status_3.0": "Retired"}
    # 5: ISCED5 or ISCED6
    Highest_ISCED = {"Highest_ISCED_1.0": "ISCED 1", "Highest_ISCED_2.0": "ISCED 2",
                     "Highest_ISCED_3.0": "ISCED 3", "Highest_ISCED_4.0": "ISCED 4"}
    # 5: Divorced
    Martial_Status = {"Martial_Status_1.0": "Never Married", "Martial_Status_2.0": "Married",
                      "Martial_Status_3.0": "Separated", "Martial_Status_4.0": "Widowed"}
    # 4: Region 4
    Region_ID = {"Regional_ID_1": "Region_1",
                 "Regional_ID_2": "Region_2", "Regional_ID_3": "Region_3"}
    # 11: Province 11
    # 40: District 40

    dataframe = pd.get_dummies(dataframe, columns=['Work_Status', "Highest_ISCED", "Martial_Status", "Regional_ID", "Province", "District"]).rename(columns=Work_Columns).rename(columns=Highest_ISCED).rename(
        columns=Martial_Status).rename(columns=Region_ID).drop(columns=["Work_Status_4.0", "Highest_ISCED_5.0", "Martial_Status_5.0", "City.Community", "Regional_ID_4", "Province_11", "District_40"], errors='ignore')
    return dataframe

# Create a function for handeling train test split


def t_t_split(dataframe):
    # Split x and y vars (also drop personal ID identifier and index)
    y_var = dataframe["Person_Income"]
    x_var = dataframe.drop(columns=["index", "Person_Income", "Personal_ID"])

    # Ensure that our Sample has the same # of columns and position as the Holdout set (Will create 0's if the column doesn't exsist)
    x_var = x_var.reindex(Holdout_x.columns, axis="columns", fill_value=0)

    # Creat Train/Test split for x and y
    x_train, x_test, y_train, y_test = train_test_split(
        x_var, y_var, test_size=0.25, random_state=420)

    # Keep a copy of unscaled x_test for later comparison
    x_test_unscaled = x_test

    # Scale x var for train and test
    sc = StandardScaler()

    # Done now for convience regarding column reassignment(rather than above when x_var created)
    x_train = sc.fit_transform(x_train)
    x_test = sc.fit_transform(x_test)
    x_train = pd.DataFrame(x_train, columns=x_var.columns)
    x_test = pd.DataFrame(x_test, columns=x_var.columns)

    return x_train, x_test, y_train, y_test, x_test_unscaled

# Loop through Sampling methods and run our NN on each one, storing the score and results


def results(df_dict, reg_methods, S_Methods, s_names):

    # Create a list of names for recording scores(i.e. SRS_Base, SRS_Importance, etc.)
    scores = pd.DataFrame()
    scores_holdout = pd.DataFrame()
    predicts_dict = {}
    holdout_p_dict = {}

    for reg_method in reg_methods:
        print(reg_method)
        for S_Method in S_Methods:
            print(S_Method)
            for Method in s_names:
                print(Method)
                for i in range(10):
                    print(i)
                    # Keep track of current iteration step
                    x_train, x_test, y_train, y_test, x_test_unscaled = t_t_split(
                        df_dict[f"{S_Method}_{Method}_{i+1}"])
                    # Check if running RF or MLPR
                    if reg_method == "RF":
                        regr = RandomForestRegressor(max_depth=500, random_state=420, min_samples_leaf=15,
                                                     min_samples_split=200, n_estimators=1000, n_jobs=-1).fit(x_train, y_train)
                    if reg_method == "MLPR":
                        regr = MLPRegressor(hidden_layer_sizes=(100,), random_state=420, max_iter=500,
                                            activation="relu", solver='lbfgs', alpha=0.01).fit(x_train, y_train)

                    """Write a scoring and predictions function for holdout set"""
                    # Generate Score Value
                    s_v = regr.score(x_test, y_test)
                    h_s_v = regr.score(Holdout_x, Holdout_y)

                    # Add score to scores DataFrame (First line checks if both regression methods are run or just one)
                    if (len(reg_method) == 1) or (len(reg_method) > 1 and reg_method == "RF"):
                        if (S_Method == "SRS" and Method == "Base"):
                            scores = scores.append(
                                {f"{reg_method}_{S_Method}_{Method}": s_v}, ignore_index=True)
                            scores_holdout = scores_holdout.append(
                                {f"{reg_method}_{S_Method}_{Method}": h_s_v}, ignore_index=True)

                    # Once RF SRS Base is done, scores are updated with this function (To avoid indexing errors)
                    if (S_Method != "SRS" or Method != "Base") or (len(reg_method) > 1 and reg_method == "MLPR"):
                        scores.loc[i,
                                   f"{reg_method}_{S_Method}_{Method}"] = s_v
                        scores_holdout.loc[i,
                                           f"{reg_method}_{S_Method}_{Method}"] = h_s_v

                    # Generate Predictions
                    predictions = regr.predict(x_test)
                    holdout_p = regr.predict(Holdout_x)

                    # Replace negative outputs with 0
                    predictions = np.where(predictions < 0, 0, predictions)
                    holdout_p = np.where(holdout_p < 0, 0, holdout_p)

                    # Convert Predicitons from Ndarray to Dataframe for Concat
                    predictions = pd.DataFrame(
                        data=predictions, columns=["Predictions"])
                    holdout_p = pd.DataFrame(
                        data=holdout_p, columns=["Predictions"])

                    # Store Y_test, Predictions for X_test and data for X_test (For Comparision)
                    fused_df = pd.concat([y_test.reset_index(
                        drop=True), predictions, x_test_unscaled.reset_index(drop=True)], axis=1)
                    fused_h_df = pd.concat([Holdout_y.reset_index(
                        drop=True), holdout_p, Holdout_unscaled.reset_index(drop=True)], axis=1)
                    predicts_dict[f"{reg_method}_{S_Method}_{Method}_{i+1}"] = fused_df
                    holdout_p_dict[f"{reg_method}_{S_Method}_{Method}_{i+1}"] = fused_h_df

    return scores, predicts_dict, scores_holdout, holdout_p_dict


# Store Holdout set for testing
os.chdir(holdout_dir)
Holdout = feather.read_dataframe("Holdout.feather")
Holdout_y = Holdout["Person_Income"]
Holdout_x = Holdout.drop(
    columns=["index", "Person_Income", "Personal_ID", "City/Community"])

sc = StandardScaler()
Holdout_x_copy = Holdout_x.copy()
# Created unscaled for analysis after model is run
Holdout_unscaled = Holdout_x
# Copy is used for column reassignment in holdout
Holdout_x_copy = cata_encode(Holdout_x_copy)
# Change Sex to numeric and Catagorical variables to hotcoding
Holdout_x = to_numeric(Holdout_x)
Holdout_x = cata_encode(Holdout_x)
Holdout_x = sc.fit_transform(Holdout_x)
Holdout_x = pd.DataFrame(Holdout_x, columns=Holdout_x_copy.columns)

S_Methods = ["SRS", "Stratified", "Stratified Cluster"]
# Create a dictionary for accesing all dataframes
df_dict = {}
for j in S_Methods:
    for i in range(1, 11):
        os.chdir(feather_dir)
        index = str(i).rjust(2, '0')
        SRS = feather.read_dataframe(f"{index}_{j}_base_sample.feather")
        Importance = feather.read_dataframe(
            f"{index}_{j}_importance_sample.feather")
        Synthetic = feather.read_dataframe(
            f"{index}_{j}_synthetic_sample.feather")

        # Synthetic variables require integer conversion
        Synthetic.iloc[:, 1:] = Synthetic.iloc[:, 1:].round()

        # Testing (If you want to test a smaller subset of AMELIA, uncomment these three lines - 1:25 can be altered to change the number of observations included)
        # SRS = SRS.iloc[1:25,]
        # Importance = Importance.iloc[1:25,]
        # Synthetic = Synthetic.iloc[1:25,]

        # Apply numeric conversion (Male:1, Female:2) and create Dummy variables for Catagorical variables
        SRS = to_numeric(SRS)
        SRS = cata_encode(SRS)
        Importance = to_numeric(Importance)
        Importance = cata_encode(Importance)
        Synthetic = to_numeric(Synthetic)
        Synthetic = cata_encode(Synthetic)

        df_dict[f"{j}_Base_{i}"] = SRS
        df_dict[f"{j}_Importance_{i}"] = Importance
        df_dict[f"{j}_Synthetic_{i}"] = Synthetic

reg_methods = ["RF", "MLPR"]
s_names = ["Base", "Importance", "Synthetic"]
# This function does not currently have the capability to run single args (want to be able to pass one reg_method, s_method or s_name and still work)
scores, predicts_dict, scores_holdout, holdout_p_dict = results(
    df_dict, reg_methods, S_Methods, s_names)

os.chdir(results_dir)
scores.index += 1
scores_holdout.index += 1

scores.to_csv("scores.csv", sep=";")
scores_holdout.to_csv("scores_holdout.csv", sep=";")
feather.write_dataframe(scores, "scores.feather")
feather.write_dataframe(scores_holdout, "scores_holdout.feather")

S_Methods = ["SRS", "Stratified", "Stratified Cluster"]
reg_methods = ["RF", "MLPR"]
s_names = ["Base", "Importance", "Synthetic"]

for reg in reg_methods:
    for samp in S_Methods:
        for corr in s_names:
            for i in range(1, 11):
                feather.write_dataframe(
                    predicts_dict[f"{reg}_{samp}_{corr}_{i}"], f"{str(i).rjust(2, '0')}_{samp}_{corr.lower()}_{reg}_results.feather")
                feather.write_dataframe(
                    holdout_p_dict[f"{reg}_{samp}_{corr}_{i}"], f"{str(i).rjust(2, '0')}_{samp}_{corr.lower()}_{reg}_Holdout_results.feather")
