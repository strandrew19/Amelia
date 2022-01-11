import os
import pyreadr
import pandas as pd
from tqdm import tqdm

person_wd = os.getcwd() + "\\AMELIA\\AMELIA_P_level_v0.2.3 (Person-Level)\\AMELIA_P_level_v0.2.3\\"
<<<<<<< HEAD
# person_wd = r"C:\Users\David\Documents\Uni\02 Data Science\03 Wise 21-22\Research Case Studies\AMELIA\AMELIA_P_level_v0.2.3"
print(person_wd)
=======
>>>>>>> 382c9454c8f38ce05995d2923a1ac2178346ab96

AMELIA = pd.DataFrame()

def readr(inp_name, out_cname, wd = person_wd):
    """"
    Adds AMELIA Data from local RData File to global pandas DataFrame
    Since filenaming of AMELIA follows pattern, this function reduces the input effort and allows for iteration

    INPUT: AMELIA Data vectors, saved as RData
    RETURNS: void, added to global DataFrame
    """

    global AMELIA

    fname = f"PAML.{inp_name}_v0.2.3.RData"

    temp = pyreadr.read_r(f"{wd}/{fname}")
    AMELIA[out_cname] = temp[inp_name]

files = {
    "AGE": "AGE", 
    "BAS": "Work_Status", 
    "CIT": "City/Community", 
    "COB": "City of Birth", 
    "DIS": "District", 
    "DOU": "Degree_of_urbanisation", 
    "EDI": "Equivalised_diposable_income(house-hold)",
    "EDU": "Current_Education_Activity",
    "HHS": "Household_size",
    "HID": "Household_ID",
    "INC": "Person_Income",
    "ISCED": "Highest_ISCED",
    "MST": "Martial_Status",
    "PID": "Personal_ID",
    "PROV": "Province",
    "PWHI": "Highest_Income_person",
    "PY010": "Cash_Income",
    "PY020": "Non-Cash_Employee",
    "PY050": "Cash_Benefits_or_SE_loss",
    "PY070": "Goods_Value_Produced_For_Own_Consumption",
    "PY090": "Unemployment_Benfits",
    "PY100": "Old-Age_Benefits",
    "PY110": "Survivor_Benefits",
    "PY120": "Sickness_Benefits",
    "PY130": "Disability_Benefits",
    "PY140": "Education-Related_Allowances",
    "REG": "Regional_ID",
    "RES": "Residential Status",
    "SEM": "Self_Employment_Dummy",
    "SEX": "Sex",
    "SOC": "Social_Income",
    "SUP": "Managerial_Position",
    "UEP": "Unemployment_Profile"
}

def KNN(Target, Classes):
    """
    INPUT: Dataframe node's we want to find NN class, Dataframe: Classes for Comparison
    RETURNS: Dictionary of {Node of Interest: Nearest Neighbour}
    """
    NN_dict = {} 
    #Iterates through the target list, grabbing the closest class neighbor in the province based on Income and returns the corresponding Neasrest Neighbour City/Community 
    for i in tqdm(range(len(Target))):
        T_City = Target['City/Community'][i]
        NN = Classes.iloc[abs(Classes.loc[Classes['Province'] == Target['Province'][i]]['Person_Income'] - Target.iloc[i]['Person_Income']).idxmin(axis=0)]['City/Community']
        NN_dict[T_City] = round(NN)
    return NN_dict

print("### Reading Data ###")
for key, val in tqdm(files.items()):
    readr(inp_name = key, out_cname= val)

#Narrow Dataset to only include individuals 18-65 years old
sex = {1:"Male",2:"Female"}
Trimmed_Age = AMELIA.loc[(AMELIA["AGE"] > 17) & (AMELIA["AGE"] < 66)]
Trimmed_Age = Trimmed_Age.replace({"Sex": sex})

#Identify region Count
Check_Region = Trimmed_Age.groupby(["City/Community", "Province"]).size().reset_index().rename(columns={0:"Count"})

#Generate our Target and Class Dataframes for KNN
Target_list = Check_Region.loc[(Check_Region["Count"] < 300)]
Classes = Trimmed_Age.groupby(["City/Community", "Province"])["Person_Income"].agg(Person_Income='mean').reset_index().round(2)

#Create Dataframe for our Target Cities
Target = Classes.merge(Target_list, left_on='City/Community', right_on='City/Community').drop(columns="Province_y").rename(columns={"Province_x":"Province"})

#Remove Values to be reassigned from Classes Dataframe
Classes = Classes[~Classes.isin(Target_list)].dropna().reset_index()

print("### Redistributing ###")
Redistribution = KNN(Target,Classes)

#Create a Dictionary for redistricting Cities/Communities that have been changed
Districts = Trimmed_Age.groupby(['City/Community', 'District']).size().reset_index()
Districts = dict(zip(Districts['City/Community'], Districts['District']))
Redistrict = {}
for k,v in Redistribution.items():
    if Districts[k] != Districts[v]:
        Redistrict[k] = Districts[v]
print("### Reassignment ###")
#District Reassignment (Pandas has forced my hand, because replace doesn't work, this must be done)
for k,v in tqdm(Redistrict.items()):
    Trimmed_Age.loc[Trimmed_Age['City/Community'] == k, 'District'] = v

#City/Community Reassignment 
for k,v in Redistribution.items():
    Trimmed_Age.loc[Trimmed_Age['City/Community'] == k, 'City/Community'] = v

#Check to ensure all Cities/Communities have been reassigned
print(Trimmed_Age.groupby(["City/Community", "Province", 'District']).size().reset_index().rename(columns={0:"Count"}).head())

#Create Holdout data
Holdout = Trimmed_Age.sample(n = 20000, random_state = 420)

#Remove Holdout data from Amelia Dataset
Trimmed_Age = Trimmed_Age.drop(Holdout.index)

# Export to CSV file
print("### Exporting ###")
<<<<<<< HEAD

write_csv = False
if write_csv:
    Trimmed_Age.to_csv(f"{person_wd}/Cleaned_Amelia_Dataset.csv")
<<<<<<< HEAD
=======
    Holdout.to_csv(f"{person_wd}/Holdout_Amelia_Dataset.csv")
>>>>>>> 8535ee84833ad5060e140d119064554b4aad0b4c

# Export to Feather for faster R import
# Source: https://stackoverflow.com/questions/24094476/python-pandas-to-r-dataframe

# NOTE: This requires feather: pip-install feather-format as well as pandas >= v0.20.0

Trimmed_Age.reset_index().to_feather(person_wd + "AMELIA.feather") # Index reset is requirement for export
<<<<<<< HEAD
=======
Holdout.reset_index().to_feather(person_wd + "Holdout.feather")
>>>>>>> 8535ee84833ad5060e140d119064554b4aad0b4c
=======
Trimmed_Age.to_csv(f"{person_wd}/Cleaned_Amelia_Dataset.csv")
>>>>>>> 382c9454c8f38ce05995d2923a1ac2178346ab96
