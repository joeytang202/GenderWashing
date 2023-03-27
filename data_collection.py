from logging import error
from re import L
from typing import Text
from twarc import Twarc2, expansions
import json
import datetime
import numpy as np
import pandas as pd
import pickle
import mysql.connector
import sqlalchemy
import pymysql
from sqlalchemy.dialects.oracle import VARCHAR2
from keys import consumer_key, consumer_secret, bearer_token
import re
from nltk.corpus import stopwords
import itertools
import collections
import nltk
import functions
import matplotlib.pyplot as plt
import seaborn as sns
import time
engine = sqlalchemy.create_engine('mysql://root:Batavus202*@localhost/twitter')

def table_exists(engine, name):
    ins = sqlalchemy.inspect(engine)
    ret = ins.dialect.has_table(engine.connect(), name)
    return(ret)

#read company data from sustainability reports,
df = pd.read_csv('company_data3.csv')
#df['women_total_'] = np.where(np.isnan(df['women_total']), df['women_8_9'], df['women_total'])
#df['women_management_'] = np.where(np.isnan(df['women_management']), df['women_subtop'], df['women_management'])
companies = df.drop_duplicates('name')[['name', 'name2', 'name3']]
companies = companies.reset_index()
#companies = pd.DataFrame(df['name'].unique(), columns = ['companies'])


def loader(table, year = False):
    if table_exists(engine, table) == False:
        if year == False:
            for i in range(0, len(companies)):
                functions.loader(query = companies['query'][i],
                    start_time = datetime.datetime(2017, 1, 1, 0, 0, 0, 0, datetime.timezone.utc),
                    end_time = datetime.datetime(2021, 12, 31, 23, 59, 0, 0, datetime.timezone.utc),
                    table = table,
                    company = companies['name'][i]
                )
        elif year == True:
            for i in range(0, len(companies)):
                for j in range(2017, 2022):
                    print(j)
                    functions.loader(query = companies['query'][i],
                    start_time = datetime.datetime(j, 1, 1, 0, 0, 0, 0, datetime.timezone.utc),
                    end_time = datetime.datetime(j, 12, 31, 23, 59, 0, 0, datetime.timezone.utc),
                    table = table,
                    company = companies['name'][i]    
                    )

#read tweets by companies if table does not exists
'''
if table_exists(engine, 'tweets_companies') == False:
    companies['query'] = ["(gender OR loonkloof OR genderongelijkheid OR gendergelijkheid OR (gender ongelijkheid) OR (gender gelijkheid) OR (vrouwen ongelijkheid) OR (vrouwen gelijkheid) OR (gender neutraal) OR (meer vrouwen) OR (gender diversiteit))"+  " from: " + company + " lang:nl -is:retweet" for company in companies['companies']]
    for i in range(0, len(companies)):
            functions.loader(query = companies['query'][i],
                start_time = datetime.datetime(2017, 1, 1, 0, 0, 0, 0, datetime.timezone.utc),
                end_time = datetime.datetime(2021, 12, 31, 23, 59, 0, 0, datetime.timezone.utc),
                table = 'tweets_companies',
                company = companies['companies'][i]
            )
'''
companies['query'] = ["(gender OR loonkloof OR (gender wage gap) OR genderongelijkheid OR (gender inequality) OR gendergelijkheid OR (gender equality) OR (gender ongelijkheid) OR (gender gelijkheid) OR (vrouwen ongelijkheid) OR (women inequality) OR (women equality) OR (female inequality) OR (female equality) OR (vrouwen gelijkheid) OR (gender neutraal) OR (gender neutral) OR (more women) OR (meer vrouwen) OR (gender diversiteit) OR (gender diversity) OR (internationale vrouwendag) OR (internationalevrouwendag) OR (international women's day) OR internationalwomensday OR (gender balans) OR (gender balance))"+  " from: " + company + " -is:retweet" for company in companies['name']]
loader('tweets_companies4')


tweets = pd.read_sql_table(
    'tweets_companies4',
    con=engine
)

def clean_tweets(tweets, type_):
    tweets['account_bio'] = tweets['account_bio'].str.lower()
    tweets['account_username'] = tweets['account_username'].str.lower()
    #give tweets type 'company'
    tweets['type'] = type_
    return(tweets)

tweets = clean_tweets(tweets, 'company')

def printer(tweets):
    print("# of tweets: " + str(len(tweets)))
    print(tweets.head())

printer(tweets)



#fill df with query for tweets about the company,
'''
companies['query'] = ["(gender OR loonkloof OR genderongelijkheid OR gendergelijkheid OR (gender ongelijkheid) OR (gender gelijkheid) OR (vrouwen ongelijkheid) OR (vrouwen gelijkheid) OR (gender neutraal) OR (meer vrouwen) OR (gender diversiteit)) " + company + " lang:nl -is:retweet" for company in companies['companies']]
if(table_exists(engine, 'tweets_others') == False):
    for i in range(0, len(companies)):
        functions.loader(query = companies['query'][i],
            start_time = datetime.datetime(2017, 1, 1, 0, 0, 0, 0, datetime.timezone.utc),
            end_time = datetime.datetime(2021, 12, 31, 23, 59, 0, 0, datetime.timezone.utc),
            table = 'tweets_others',
            company = companies['companies'][i]
)

'''
companies['query'] = ["(gender OR loonkloof OR (gender wage gap) OR genderongelijkheid OR (gender inequality) OR gendergelijkheid OR (gender equality) OR (gender ongelijkheid) OR (gender gelijkheid) OR (vrouwen ongelijkheid) OR (women inequality) OR (women equality) OR (female inequality) OR (female equality) OR (vrouwen gelijkheid) OR (gender neutraal) OR (gender neutral) OR (more women) OR (meer vrouwen) OR (gender diversiteit) OR (gender diversity) OR (internationale vrouwendag) OR (internationalevrouwendag) OR (international women's day) OR internationalwomensday OR (gender balans) OR (gender balance)) (" + name + " OR " + name2 + ") -is:retweet" if name2 is not np.nan else "(gender OR loonkloof OR genderongelijkheid OR gendergelijkheid OR (gender ongelijkheid) OR (gender gelijkheid) OR (vrouwen ongelijkheid) OR (vrouwen gelijkheid) OR (gender neutraal) OR (meer vrouwen) OR (gender diversiteit)) " + name + " lang:nl -is:retweet" for name, name2 in zip(companies['name'], companies['name2'])]
loader('tweets_others4')


tweets_others = pd.read_sql_table(
    'tweets_others4',
    con=engine
)

tweets_others = clean_tweets(tweets_others, 'representative')
    
#select representatives by filtering on: account_bio contains company name AND username is not company OR list of known representatives

reps = '|'.join(companies['name'])
reps2 = companies[~pd.isnull(companies['name2'])]['name2']
reps = reps + '|' + '|'.join(reps2)
reps3 = companies[~pd.isnull(companies['name3'])]['name3']
reps = reps + '|' + '|'.join(reps3)



tweets_representatives = tweets_others[tweets_others['account_bio'].astype(str).str.contains(reps) & ~tweets_others['account_username'].isin(tweets['account_username'])] 
#tweets_representatives['type'] = 'representative'
printer(tweets_representatives)

#select stakeholders: account_username is not in representatives and not in company tweets
#tweets_stakeholders = tweets_others[(~tweets_others['account_username'].isin(tweets_representatives['account_username'])) & (~tweets_others['account_username'].isin(tweets['account_username']))]
#tweets_stakeholders['type'] = 'stakeholder'
#printer(tweets_stakeholders)

tweets = tweets.append(tweets_representatives)



#tweets.to_csv('test.csv')

#.append(tweets_stakeholders)
'''
import spacy
from spacy.lang.nl import Dutch

parser = Dutch()
nlp = spacy.load('nl_core_news_sm')
#account names of companies and representatives
comps_and_reps = tweets[tweets['type'] != 'stakeholder']['account_username'].unique()
comps_and_reps = ['@' + string for string in comps_and_reps]

def tokenize(text):
    lda_tokens = []
    tokens = parser(text)
    for token in tokens:
        if token.orth_.isspace():
            continue
        elif token.like_url:
            lda_tokens.append('URL')
        #keeps company mentions
        elif token.text in comps_and_reps:
            lda_tokens.append(token.text)
        elif token.orth_.startswith('@'):
            lda_tokens.append('SCREEN_NAME')
        else:
            lda_tokens.append(token.lower_)
        return lda_tokens

import nltk
#nltk.download('wordnet')
    
from nltk.corpus import wordnet as wn
def get_lemma(word):
    lemma = wn.morphy(word)
    if lemma is None:
        return word
    else:
        return lemma
    
from nltk.stem.wordnet import WordNetLemmatizer
def get_lemma2(word):
    return WordNetLemmatizer().lemmatize(word, lang = 'nld')
    
from nltk.corpus import stopwords
stopword_list = stopwords.words('dutch')
    
def prepare_text_for_lda(text):
    tokens = tokenize(text)
    tokens = [token for token in tokens if len(token) > 4]
    tokens = [token for token in tokens if token not in stopword_list]
    tokens = [get_lemma(token) for token in tokens]
    return tokens
 
    
tweets['text_clean'] = [prepare_text_for_lda(text) for text in tweets['text']]
'''
def dater(string):
    if '2021-' in string:
        return(2021)
    elif '2020-' in string: 
        return(2020)
    elif '2019-' in string:
        return(2019)
    elif '2018' in string:
        return(2018)
    elif '2017' in string:
        return(2017)
        
tweets['year'] = [dater(i) for i in tweets['created_at']]

reps = tweets[tweets['type'] == 'representative'][['account_username', 'year', 'account_bio']]
reps = reps.sort_values(by=['account_username', 'year'])

reps = reps.drop_duplicates(subset=['account_username', 'year'])

reps.to_csv('reps2.csv')


reps = pd.read_csv('reps2_edit.csv')
reps = list(reps[reps['keep'] == 1]['account_username'].unique())

df = pd.read_csv('company_data3.csv')

df['name'] = df['name'].str.lower()

keep = list(df['name'].unique()) + reps

print('comps and representative: ', len(keep))

print('amount of tweets gender: ', len(tweets['account_username']))

tweets = tweets[tweets['account_username'].isin(keep)]

import pickle
pickle.dump(tweets, open('tweets.pkl', 'wb'))

#ALL TWEETS

start = time.time()

companies['query'] = ["from:" + company + " -is:retweet" for company in companies['name']]
loader('all_tweets_companies4')


'''
if(table_exists(engine, 'all_tweets_companies') == False):
    for i in range(0, len(companies)):
        for j in range(2017, 2021):
            functions.loader(query = companies['query'][i],
                start_time = datetime.datetime(j, 1, 1, 0, 0, 0, 0, datetime.timezone.utc),
                end_time = datetime.datetime(j, 12, 31, 23, 59, 0, 0, datetime.timezone.utc),
                table = 'all_tweets_companies',
                company = companies['companies'][i]
)
''' 

end = time.time()
print("Time taken: " + str(end-start))


tweets_all = pd.read_sql_table(
    'all_tweets_companies4',
    con=engine,
)

tweets_all = clean_tweets(tweets_all, 'company')
printer(tweets_all)

#d = {}
#for company in companies['name']:
#    d[company] = tweets_others[tweets_others['company'] == company]['account_username'].unique()

#print(d)
#queries = ["(" + name + " OR " + name2 +") -is:retweet from: " + rep if name2 is not np.nan else name + " lang:nl - is:retweet " for name, name2 in zip(companies['name'], companies['name2']) for rep in tweets_representatives['account_username'].unique()]
#print(queries)


#queries = ["(" + name + " OR " + name2 +") -is:retweet from: " + rep if name2 is not np.nan else name + "  -is:retweet from: " + rep for name, name2 in zip(companies['name'], companies['name2']) for rep in d[name]]

#print(queries)

#s = companies['name'].map(d)
#from itertools import chain

#lens = s.str.len()
#companies = pd.DataFrame({
#    'name' : companies['name'].values.repeat(lens),
#    'name2' : companies['name2'].values.repeat(lens),
#    'name3' : companies['name3'].values.repeat(lens),
#    'query' : list(chain.from_iterable(s))
#    })

companies = pd.DataFrame()
companies['name'] = reps
companies['query'] = ["from:" + rep + " -is:retweet" for rep in companies['name']]
print(companies)
loader('all_tweets_companies4')

#companies['query'] = ["(" + name + " OR " + name2 +") -is:retweet from: " + rep if name2 is not np.nan else name + "  -is:retweet from:" + rep for name, name2, rep in zip(companies['name'], companies['name2'], companies['query'])]


start = time.time()

'''
if(table_exists(engine, 'all_tweets_others') == False):
    for i in range(0, len(companies)):
        for j in range(2017, 2021):
            functions.loader(query = companies['query'][i],
                start_time = datetime.datetime(j, 1, 1, 0, 0, 0, 0, datetime.timezone.utc),
                end_time = datetime.datetime(j, 12, 31, 23, 59, 0, 0, datetime.timezone.utc),
                table = 'all_tweets_others',
                company = companies['companies'][i]
)
'''

loader('all_tweets_others4')

end = time.time()
print("Time taken: " + str(end-start))

'''
tweets_representatives_all = pd.read_sql_table(
    'all_tweets_others3',
    con=engine,
)

tweets_representatives_all = clean_tweets(tweets_representatives_all, 'representative')

#select representatives by filtering on: account_bio contains company name AND username is not company OR list of known representatives
#tweets_representatives_all = tweets_others_all[(tweets_others_all["account_bio"].astype(str).str.contains("amro|hema|heijn|philips|unilever|kpn|klm|shell|aegon|heineken")) & (~tweets_all['account_username'].isin(tweets_others_all['account_username'])) | (tweets_others_all['account_username'] == 'huub_beurskens')]
#tweets_representatives_all['type'] = 'representative'
printer(tweets_representatives_all)

#select stakeholders: account_username is not in representatives and not in company tweets
#tweets_stakeholders_all = tweets_others_all[(~tweets_others_all['account_username'].isin(tweets_representatives_all['account_username'])) & (~tweets_others_all['account_username'].isin(tweets_all['account_username']))]
#tweets_stakeholders_all['type'] = 'stakeholder'
#printer(tweets_stakeholders_all)

tweets_all = tweets_all.append(tweets_representatives_all)

#comps_and_reps = tweets_all[tweets_all['type'] != 'stakeholder']['account_username'].unique()
comps_and_reps = tweets_representatives_all['account_username'].unique()
comps_and_reps = ['@' + string for string in comps_and_reps]
tweets_all['text_clean'] = [prepare_text_for_lda(text) for text in tweets_all['text']]
tweets_all['year'] = [dater(i) for i in tweets_all['created_at']]

#pickle.dump(tweets_all, open('tweets_all.pkl', 'wb'))


'''

