# bool treatment

import pandas as pd

df = pd.read_csv("with_weather_and_power.csv")
treats = pd.read_csv("uncollapsed_PSEG_townships_conditions_with_names.csv")

beforemonths = ["May2017", "Jun2017", "Jul2017"]
months = ["Aug2017", "Sep2017"]
start, end = months

tups = []
for rowi, row in treats.iterrows():
    if not pd.isnull(row['Months Treated']):
        two = [(row['MunicipalCityCode'], start, row['Condition'], True),
               (row['MunicipalCityCode'], end, row['Condition'], True)]
        tups.extend(two)
    else:
        two = [(row['MunicipalCityCode'], start, row['Condition'], False),
               (row['MunicipalCityCode'], end, row['Condition'], False)]
        tups.extend(two)
    tups.extend([(row['MunicipalCityCode'], m, row['Condition'], False) for m in beforemonths])

just_summer = df.merge(pd.DataFrame(tups, columns=["MunicipalCityCode", "pseg month", "Condition", "Treated"]),
                       on=["MunicipalCityCode", "pseg month"])

just_summer.to_csv("summer_data.csv")

out = df.merge(pd.DataFrame(tups, columns=["MunicipalCityCode", "pseg month", "Condition", "Treated"]),
               on=["MunicipalCityCode", "pseg month"],
               how="outer")
out.to_csv("formatted_data.csv")
