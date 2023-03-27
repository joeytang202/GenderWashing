import pandas as pd
import numpy as np

df = pd.read_pickle(r'data.pkl')
accounts = pd.read_csv('accounts.csv')[['isin', 'twitter_name']]
df = df.merge(accounts, left_on = 'name', right_on = 'twitter_name')

lines = df['isin'].unique()
lines = [str(line) for line in lines]

with open('isin.txt', 'w') as f:
    for line in lines:
        f.write(line)
        f.write('\n')










'''

def compustater(f, df):
	compustat = pd.read_csv(f)[['gvkey', 'fyear', 'at', 'emp', 'revt', 'isin', 'sic']]
	compustat['isin'] = np.where(pd.isnull(compustat['isin']), 'None', compustat['isin'])
	df = df.merge(compustat, left_on = ['isin','year'], right_on = ['isin', 'fyear'])
	return(df)

nl = compustater('compustat_nl.csv', df)
gb = compustater('compustat_gb.csv', df)
fr = compustater('compustat_fr.csv', df)

df = pd.concat([nl,gb,fr])
'''

#lines = df['gvkey'].unique()
#lines = [str(line) for line in lines]

#with open('gvkey.txt', 'w') as f:
#    for line in lines:
#        f.write(line)
#        f.write('\n')

