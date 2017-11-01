# format data for regression etc
# goal output: csv with the following variables:
# if collapse_years: weather (2016 and 2017), power (2016 and 2017), municipality, month, treatment
# else: weather,  power, municipality, month, treatment

import pandas as pd
import numpy as np
from collections import Iterable

collapse_years = False

# Municipalities of interest, their waves, and their conditions
munis = pd.read_csv("uncollapsed_PSEG_townships_conditions_with_names.csv")

# Average power usage by month by municipality
p2016 = pd.read_csv("p2016.csv")
p2017 = pd.read_csv("p2017.csv")
if collapse_years == True:
    power = p2016.merge(p2017, on="Municipal City Code")
else:
    power = p2016.append(p2017)
power["MunicipalCityCode"] = power["Municipal City Code"].str.lower()
power.to_csv("power.csv")

# Weather by month (for naive joining)
monthly_weather = pd.read_csv("monthly_weather.csv")

# Weather by day (for joining based on meter read dates and percentage)
daily_weather = pd.read_csv("daily_weather.csv")

# Meter read percentages by municipality
percent_read = pd.read_csv("percent_meters.csv")
percent_read["MunicipalCityCode"] = percent_read["MunicipalCityCode"].str.lower()

# get average route for municipality
means = []
allroutes = [str(v) for v in range(1, 22)]
for muni in percent_read['MunicipalCityCode'].unique():
    r = percent_read[percent_read['MunicipalCityCode'] == muni]
    weights = r[allroutes].values.tolist()[0]
    vals = [v for v in range(1, 22)]
    m = sum([float(weights[i]) * vals[i] for i in range(len(vals))])
    means.append(m)
percent_read['mean read day'] = means
percent_read['mean read day int'] = [round(v) for v in means]
percent_read.to_csv("percent_meters.csv", index=False)

# actual meter read dates
m2016 = pd.read_csv("read_dates2016.csv")
m2017 = pd.read_csv("read_dates2017.csv")

# get average read day by municipality
tups = []
for i in range(2):
    yd = [m2016, m2017][i]
    y = ["2016", "2017"][i]
    for muni in percent_read['MunicipalCityCode'].unique():
        perc = percent_read.loc[percent_read['MunicipalCityCode'] == muni]['mean read day int'].iloc[0]
        for month in yd['Month'].unique():
            mname = month + y
            md = yd.loc[yd['Month'] == month]
            tups.append((muni, mname, md[str(perc)].iloc[0]))
average_read_day = pd.DataFrame(tups, columns=["MunicipalCityCode", "Month", "Read_Day"])
average_read_day.to_csv("average_read_day.csv")

# get durations (start + end) for each muni's average month in same form as the weather data
monthlens = pd.read_csv("days_per_month.csv")
monthlist = monthlens['Month'].tolist()
durations = []
durations_without_power = []
for i in range(len(tups)):
    t = tups[i]
    out = dict()
    out["MunicipalCityCode"] = t[0]
    if i == 0 or t[0] != tups[i - 1][0]:
        continue  # algorithm requires read day for same muni for previous and next month to run
    out["pseg month"] = t[1]  # align with energy format data. all other fields align with real / weather data.
    out["end year"] = str(int(out["pseg month"][-4:]))  # int cast fails if the data is misformatted
    out['start year'] = str(int(tups[i - 1][1][-4:]))
    out['end month'] = [m for m in monthlist if m.startswith(out["pseg month"][0:3])][0]
    try:
        out['end day'] = int(t[2])
    except ValueError:  # meter read day for one month is actually at the beginning of the following month
        monthi = monthlist.index(out['end month'])
        out['end day'] = int(t[2][-1])
        if monthi == len(monthlist) - 1:
            out['end month'] = monthlist[0]  # the next month after December is January of the next year
            out['end year'] = str(int(out['end year']) + 1)
        else:
            out['end month'] = monthlist[monthi + 1]
    try:
        m = [m for m in monthlist if m.startswith(tups[i - 1][1][0:3])][0]
        if int(tups[i - 1][2]) + 1 < int(monthlens.loc[monthlens['Month'] == m]['Days']):
            # ^ if last month has another day, then:
            out['start day'] = int(tups[i - 1][2]) + 1
            out['start month'] = m
            out['start year'] = tups[i - 1][1][-4:]
        else:  # case: last month's meter read period ended exactly on the last day of last month
            out['start day'] = 1
            out['start month'] = [m for m in monthlist if m.startswith(out["pseg month"][0:3])][0]
            out['start year'] = out['end year'][:]
    except ValueError:  # last month's meter read day was actually at the beginning of this month
        monthi = monthlist.index([m for m in monthlist if m.startswith(out["pseg month"][0:3])][0])
        out['start day'] = int(tups[i - 1][2][-1]) + 1
        out['start month'] = monthlist[monthi]
        if monthi == 0:  # Case of (previous month) December being read in (current month) January
            out['start year'] = out['end year'][:]

    try:
        int(out['start year'])  # asserts start year is int-able, otherwise throws value error
    except ValueError:
        print("fatal ValueError")
        print(out)
        exit()
    except KeyError:
        print("fatal KeyError")
        print(out)
        exit()

    # add power data while we're here
    try:
        out['avg power use'] = np.float64(
            power[power['MunicipalCityCode'] == out['MunicipalCityCode']][out["pseg month"]])
        durations.append(out)
    except KeyError:
        pass
    except ValueError:
        print("Unexpected non-number: ")
        print(power[power['MunicipalCityCode'] == out['MunicipalCityCode']][out["pseg month"]])

    durations_without_power.append(out)

pd.DataFrame(durations_without_power).to_csv("durations_without_power.csv")

durations = pd.DataFrame(durations)

cdd = []
monthlen = []
for rowindex, row in durations.iterrows():
    try:
        starti = int(daily_weather.loc[(daily_weather['Year'] == np.int64(row['start year'])) &
                                       (daily_weather['Month'] == row['start month']) &
                                       (daily_weather['Day'] == np.int64(row['start day']))
                                       ].index.tolist()[0])
        endi = int(daily_weather.loc[(daily_weather['Year'] == np.int64(row['end year'])) &
                                     (daily_weather['Month'] == row['end month']) &
                                     (daily_weather['Day'] == np.int64(row['end day']))
                                     ].index.tolist()[0])
        cdd.append(daily_weather['NewarkCDD'][starti:endi].sum())
        monthlen.append(endi - starti)
    except IndexError:
        cdd.append(np.nan)
        monthlen.append(np.nan)

assert np.nanmax(monthlen) < 35
assert np.nanmin(monthlen) > 20

durations['cdd'] = cdd
durations['pseg month length'] = monthlen
if not collapse_years:
    vect = list(durations['avg power use'])
    try:
        assert all([type(vect[i]) == type(vect[i+1]) for i in range(-1, len(vect) - 2)])
    except AssertionError:
        print("Inconsistent typing of vector values. Types: "+str(set(type(v) for v in vect)))
    p = []
    for v in list(durations['avg power use']):
        try:
            p.append(v[~np.isnan(v)][0])
        except IndexError:
            p.append(v)
    durations['avg power use'] = p
durations.to_csv("with_weather_and_power.csv")

print(durations['pseg month length'].describe())
