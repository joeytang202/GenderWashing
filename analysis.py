import pandas as pd
import numpy as np
import math
import statsmodels.api as sm
import pickle
#import Linearmodels
from linearmodels import PanelOLS
from stargazer.stargazer import Stargazer
import functions
import webbrowser
from scipy.stats.mstats import winsorize

df = pd.read_pickle(r'data_org.pkl')
#print(len(df['name']))
#print(df.head())

df = df[df['name'].str.contains("hema|FlowTraders") == False]


df['ex_board_perc'] = df['women_ex_board'] / df['total_ex_board']
df['sup_board_perc'] = df['women_sup_board'] / df['total_sup_board']
df['ex_board_token'] = np.where(df['women_ex_board'] > 1, 1, 0)
df['sup_board_token'] = np.where(df['women_sup_board'] > 1, 1, 0)
df['women_senior_manag'] = np.where(~pd.isnull(df['women_subtop']), df['women_subtop'], df['women_senior_manag'])
df['women_middle_manag'] = np.where(~pd.isnull(df['women_10_11']), df['women_10_11'], df['women_middle_manag'])
df['women_junior_manag'] = np.where(~pd.isnull(df['women_8_9']), df['women_8_9'], df['women_junior_manag'])
df['women_nonmanagement'] = np.where(~pd.isnull(df['women_7<']), df['women_7<'], df['women_nonmanagement'])
df['women_management'] = np.where(pd.isnull(df['women_management']), df['women_middle_manag'], df['women_management'])

def blauer(perc_f):
    r = 1 - perc_f**2 - (1 - perc_f)**2
    r = (r-0) / .5
    return(r)

df['ex_board_blau'] = [blauer(i) for i in df['ex_board_perc']]
df['sup_board_blau'] = [blauer(i) for i in df['sup_board_perc']]
df['women_management_blau'] = [blauer(i) for i in df['women_management']]
df['women_total_blau'] = [blauer(i) for i in df['women_total']]


#imputes tweet, management and women total values for missing years
idx = pd.MultiIndex.from_product(
    [df['name'].unique(), df['year'].unique()], names = ['name', 'year'])

df = df.set_index(['name', 'year']).reindex(idx)

df = df.sort_index().reset_index()

df['gender_tweets'] = np.where(pd.isnull(df['gender_tweets']), 0, df['gender_tweets'])
df['all_tweets'] = np.where(pd.isnull(df['all_tweets']), 0, df['all_tweets'])
df['gender_tweets_prop'] = np.where(df['all_tweets'] == 0, 0, df['gender_tweets_prop'])
df['gender_tweets_prop'] = np.where(pd.isnull(df['gender_tweets_prop']), df['gender_tweets'] / df['all_tweets'], df['gender_tweets_prop'])

def imputater_extra(column):
    t = df[['name', 'year', column]]
    t['value2'] = t.groupby(['name'])[column].apply(lambda g: g.interpolate(limit_area = 'inside')) #interpolate values between two non-missing values
    m = t['value2'].notna()
    group = (m & m.ne(m.shift())).cumsum() #create groups
    t['value3'] = (t.groupby(['name', group])['value2'] 
        .apply(lambda g: g.fillna(g.diff().interpolate().cumsum().add(g.iloc[0])))
        ) #extrapolate future values
    t = t.sort_values(by = ['name', 'year'], ascending = False)
    m = t['value3'].notna()
    group = (m & m.ne(m.shift())).cumsum()
    t['value4'] = (t.groupby(['name', group])['value3']
                  .apply(lambda g: g.fillna(g.diff().interpolate().cumsum().add(g.iloc[0])))
               ) #extrapolate past values
    t = t.sort_values(by = ['name', 'year'])
    t['value5'] = t.groupby(['name'])['value4'].apply(lambda g: g.fillna(g.mean())) #fill mean for companies with only one value
    t['value6'] = t.groupby(['year'])['value5'].apply(lambda g: g.fillna(g.mean())) #fill left over with mean of year

    return(t)

df['women_total_blau_imput'] = imputater_extra('women_total_blau')['value6']
df['women_management_blau_imput'] = imputater_extra('women_management_blau')['value6']
df['ex_board_blau_imput'] = imputater_extra('ex_board_blau')['value6']
df['account_followers'] = imputater_extra('account_followers')['value6']

df['gender_tweets_dummy'] = np.where(df['gender_tweets_prop'] > 0, 1, 0)

#below is to create dummy variable that checks if diversity information is disclosed or not by the company
df['disc_manag'] = np.where(pd.isnull(df['women_management_blau']), 0, 1)
df['disc_total'] = np.where(pd.isnull(df['women_total_blau']), 0, 1)
df['women_management_blau'] = np.where(df['disc_manag'] == 0, 0, df['women_management_blau'])
df['women_total_blau'] = np.where(df['disc_total'] == 0, 0, df['women_total_blau'])

print('after imputation')
print(len(df['name']))
print(len(df['name'].unique()))


accounts = pd.read_csv('accounts.csv')[['isin', 'twitter_name']]
df = df.merge(accounts, left_on = 'name', right_on = 'twitter_name')
#print(len(df['name']))
compustat = pd.read_csv('compustat_final.csv')
df = df.merge(compustat, left_on = ['isin', 'year'], right_on = ['isin', 'fyear'])
#print(len(df['gvkey']))

print('after compustat_merge')
print(len(df['name']))
print(len(df['name'].unique()))


securities = pd.read_csv('securities.csv')
securities['year'] = [functions.dater(str(date)) for date in securities['datadate']]
g = securities.groupby(['gvkey', 'year'])['datadate'].max().reset_index()
securities = securities.merge(g[['gvkey', 'datadate']], on = ['gvkey', 'datadate'])
securities = securities[~pd.isnull(securities['isin'])]
securities = securities[securities['isin'] != 'USN070592100'] #ASML
securities = securities[(securities['gvkey'] != 10846) | ((securities['gvkey'] == 10846) & (securities['isin'] == 'GB00B10RZP78'))] #unilever, standardise over GB stock
securities = securities[(securities['gvkey'] != 12384) | ((securities['gvkey'] == 12384) & (securities['isin'] == 'GB00BP6MXD84'))] #Shell
securities = securities[securities['isin'] != 'NL0000303709'] #AEGON
securities = securities[securities['isin'] != 'US8901382098'] #tomtom
securities = securities[securities['isin'] != 'US0091191082'] #klm

securities = securities[['gvkey', 'year', 'cshoc', 'prccd']]

names = df['name'].unique()

df = df.merge(securities, on = ['gvkey', 'year'], how = 'left')

print('after securities merge')
print(len(df['name']))
print(len(df['name'].unique()))

returns = pd.read_csv('returns.csv')
returns['year'] = [functions.dater(str(date)) for date in returns['datadate']]
g = returns.groupby(['gvkey', 'year'])['datadate'].max().reset_index()
g = g.append(returns.groupby(['gvkey', 'year'])['datadate'].min().reset_index())
returns = returns.merge(g, on = ['gvkey', 'year', 'datadate'])
returns = returns[~pd.isnull(returns['isin'])]
returns = returns[returns['isin'] != 'USN070592100'] #ASML
returns = returns[(returns['gvkey'] != 10846) | ((returns['gvkey'] == 10846) & (returns['isin'] == 'GB00B10RZP78'))] #unilever, standardise over GB stock
returns = returns[(returns['gvkey'] != 12384) | ((returns['gvkey'] == 12384) & (returns['isin'] == 'GB00BP6MXD84'))] #Shell
returns = returns[returns['isin'] != 'NL0000301752'] #aegon
returns = returns[returns['isin'] != 'US0091191082'] #klm
returns = returns[returns['isin'] != 'US8901382098'] #tomtom

returns = returns.sort_values(by = ['gvkey', 'year', 'datadate'])
returns['date'] = ['start', 'end'] * int((len(returns['gvkey']) / 2 ))
returns = returns.pivot(index = ['gvkey', 'year'], columns = 'date', values = ['ajexdi', 'prccd', 'trfd']).reset_index()
returns.columns = returns.columns.get_level_values(0) + '_' +  returns.columns.get_level_values(1)


returns['annual_returns'] = (((returns['prccd_end'] / returns['ajexdi_end']) * (1+returns['trfd_end']/100)) / 
    (returns['prccd_start'] / returns['ajexdi_start']) * (1+returns['trfd_start']/100))-1

returns = returns[['gvkey_', 'year_', 'annual_returns']]
df = df.merge(returns, left_on = ['gvkey', 'year'], right_on = ['gvkey_', 'year_'], how = 'left')


df['sic'] = np.where(df['sic'] % 10 == 0, df['sic']/10, df['sic'])
sic = pd.read_csv('sic-codes.csv')
df = df.merge(sic, left_on = ['sic'], right_on = 'SIC', how = 'left')
sic = pd.read_csv('industry-groups.csv')[['Industry Group', 'Description', 'Division']]
sic.rename(columns = {'Description': 'Description Group', 'Division': 'Division2'}, inplace = True)
df = df.merge(sic, left_on = 'sic', right_on = 'Industry Group', how = 'left')
df['Description'] = np.where(pd.isnull(df['Description']), df['Description Group'], df['Description'])
df['Division'] = np.where(pd.isnull(df['Division']), df['Division2'], df['Division'])
sic = pd.read_csv('divisions.csv')[['Division', 'Description']]
sic.rename(columns = {'Description': 'Description Division'}, inplace = True)
df = df.merge(sic, on = 'Division', how = 'left')

print('after division merge')
print(len(df['name']))
print(len(df['name'].unique()))

df = df.join(pd.get_dummies(df['year']))
df = df.join(pd.get_dummies(df['Division']))

df['revt'] = df['revt'] * 1000000
df['emp'] = df['emp'] * 1000
df['emp'] = np.log(df['emp'])
df['at'] = df['at'] * 1000000
df['ceq'] = df['ceq'] * 1000000

df['de'] = np.where((df['teq'] == 0 )|(pd.isnull(df['teq'])), 1, df['dltt'] / df['teq'])
df['de'] = [i + np.absolute(df['de'].min()) + 0.001 for i in df['de']]
df['de'] = np.log(df['de'])
df['tobinsq'] = (df['cshoc'] * df['prccd'] - df['ceq'] + df['at']) / df['at']
# df['tobinsq'] = np.log(df['tobinsq'])
#df['account_followers'] = np.log(df['account_followers'])

print('final')
print(len(df['name']))
print(len(df['name'].unique()))

df = df[~pd.isnull(df['likes_all'])]

print(len(df['name']))
print(len(df['name'].unique()))

import seaborn as sns
import matplotlib.pyplot as plt

from scipy.stats import pearsonr

def calculate_pvalues(df):
    df = df.dropna()._get_numeric_data()
    dfcols = pd.DataFrame(columns=df.columns)
    pvalues = dfcols.transpose().join(dfcols, how='outer')
    for r in df.columns:
        for c in df.columns:
            pvalues[r][c] = round(pearsonr(df[r], df[c])[1], 4)
    return pvalues

df['roa'] = df['revt'] / df['at']

df.rename(columns = {2018: '2018', 2019: '2019', 2020: '2020', 2021: '2021'}, inplace = True)

data = df[['name', 'year', 'tobinsq', 'annual_returns', 'disc_manag', 'disc_total', 'emp', 'roa', 'de', 'ex_board_blau_imput', 'account_followers']]
data = data.dropna()
print('unique companies', len(data['name'].unique()))
print('total obs', len(data['name']))


df.to_csv('dataR.csv')

exit()





def fixed_effectser(X, y):
    m = PanelOLS(y, X, entity_effects = True, drop_absorbed = True)
    m = m.fit(cov_type = 'clustered', cluster_entity = True)
    return(m)

def regressioner(X, y):
    X = sm.add_constant(X)
    m = sm.OLS(y, X).fit(#cov_type = 'cluster', cov_kwds = {'groups': data['name']}
    )
    return(m)

def regger_small(y, gdd, gdb, tw, #gdd: gender diversity disclosure, gdb: gender diversity blau, tw: gender tweets
    columns = ['emp', 'roa', 'de', 'ex_board_blau_imput', 'account_followers', '2018', '2019', '2020', '2021', 'D', 'E', 'H', 'G', 'I'],
    html_reg = 'reg_small.html'):
        columns_ = columns + [y, gdd, gdb, tw] + ['name']
        data = df[columns_].dropna()
        data['blau:tweet'] = data[gdb] * data[tw]

        t = {'X' : [[gdb], [gdb, tw], [gdb, tw, 'blau:tweet']],
            'y' : [y,y,y]}

        d = pd.DataFrame(data = t)
        d['X'] = [i + columns for i in d['X']] 
        
        data_ = data[data[gdd] == 1]

        reg = [regressioner(data_[X], data_[y]) for X, y in zip(d['X'], d['y'])]

        f = open(html_reg, 'w')
        s = Stargazer(reg)
        s.significance_levels([0.05, 0.01, 0.001])
        s.covariate_order([e for e in d['X'][2] if e not in ['2018', '2019', '2020', '2021', 'D', 'E', 'H', 'G', 'I']] + ['const'])
        s.add_line('Year Dummies', ['Yes'] * 3)
        s.add_line('Sector Dummies', ['Yes'] * 3)
        f.write(s.render_latex())
        f.close()

        webbrowser.open(html_reg)



def regger(y, gdd, gdb, tw, #gdd: gender diversity disclosure, gdb: gender diversity blau, tw: gender tweets 
    columns = ['emp', 'roa', 'de', 'ex_board_blau_imput', 'account_followers', '2018', '2019', '2020', '2021', 'D', 'E', 'H', 'G', 'I'],
    html_reg = 'reg.html'):
        columns_ = columns + [y, gdd, gdb, tw] + ['name']
        data = df[columns_].dropna()
        data['disc:blau'] = data[gdd] * data[gdb]
        data['disc:tweet'] = data[gdd] * data[tw]
        data['disc:blau:tweet'] = data['disc:blau'] * data[tw]
        
        data = data.drop(gdb, axis = 1)

        t = {'X' : [[gdd], [gdd, 'disc:blau'], [gdd, 'disc:blau', tw], [gdd, 'disc:blau', tw, 'disc:tweet', 'disc:blau:tweet']],
            'y': [y, y, y, y]}

        d = pd.DataFrame(data = t)
        d['X'] = [i + columns for i in d['X']] 

        reg = [regressioner(data[X], data[y]) for X, y in zip(d['X'], d['y'])]

        f = open(html_reg, 'w')
        s = Stargazer(reg)
        s.significance_levels([0.05, 0.01, 0.001])
        s.covariate_order([e for e in d['X'][3] if e not in ['2018', '2019', '2020', '2021', 'D', 'E', 'H', 'G', 'I']] + ['const'])
        s.add_line('Year Dummies', ['Yes'] * 4)
        s.add_line('Sector Dummies', ['Yes'] * 4)
        f.write(s.render_latex())
        f.close()

        webbrowser.open(html_reg)

        def fixed_effectser(X, y):
            m = PanelOLS(y, X, entity_effects = True, drop_absorbed = True)
            m = m.fit(cov_type = 'clustered', cluster_entity = True)
            return(m)

        columns_ = ['name', 'year', 'emp', 'roa', 'de', 'ex_board_blau_imput', 'account_followers'] + [y, gdd, gdb, tw]
        data = df[columns_]
        data['disc:blau'] = data[gdd] * data[gdb]
        data['disc:tweet'] = data[gdd] * data[tw]
        data['disc:blau:tweet'] = data['disc:blau'] * data[tw]
        data = data.drop(gdb, axis = 1)
        data = data.set_index(['name', 'year'])

        print(fixed_effectser(data.drop(y, axis = 1), data[y]))


#df['annual_returns'] = [i + np.absolute(df['annual_returns'].min()) + 0.001 for i in df['annual_returns']]
#df['annual_returns'] = np.log(df['annual_returns'])


df['gender_tweets_prop'] = np.log(df['gender_tweets_prop'] + 0.001)

#regger_small('tobinsq', 'disc_manag', 'women_management_blau', 'gender_tweets_dummy')
#regger('tobinsq', 'disc_manag', 'women_management_blau', 'gender_tweets_dummy')
regger_small('annual_returns', 'disc_manag', 'women_management_blau', 'gender_tweets_dummy')
regger('annual_returns', 'disc_manag', 'women_management_blau', 'gender_tweets_dummy')
#regger('tobinsq', 'disc_manag', 'women_management_blau', 'gender_tweets_prop')
#regger('annual_returns', 'disc_manag', 'women_management_blau', 'gender_tweets_prop')
'''

data = df[df['disc_manag'] == 1]

data['group'] = df['gender_tweets_dummy'].map({0: "Gender Equality Tweets = 0", 1:"Gender Equality Tweets > 0"})

ax = sns.lmplot(x = 'women_management_blau', y = 'tobinsq', col = 'group', data = data)
ax.set_axis_labels("Management Gender Diversity", "Tobin's Q (log)")
ax.set_titles('{col_name}')

plt.show()

ax = sns.lmplot(x = 'women_management_blau', y = 'annual_returns', col = 'group', data = data)
ax.set_axis_labels("Management Gender Diversity", "Annual Returns")
ax.set_titles('{col_name}')

plt.show()
'''
