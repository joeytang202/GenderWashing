import pandas as pd
import numpy as np
from pattern.nl import sentiment as sentiment_nl
from pattern.en import sentiment as sentiment_en
import pickle
import functions
import sqlalchemy


engine = sqlalchemy.create_engine('mysql://root:Batavus202*@localhost/twitter')

tweets = pd.read_pickle(r'tweets.pkl')

print('Gender tweets of companies: ', len(tweets[tweets['type'] == 'company']['account_username']))
print('By # of companies: ', len(tweets[tweets['type'] == 'company']['account_username'].unique()))

#tweets['sentiment'] = functions.sentimenter(tweets)

print('Gender tweets of reps: ', len(tweets[tweets['type'] == 'representative']['account_username']))
print('By # of reps: ', len(tweets[tweets['type'] == 'representative']['account_username'].unique()))



'''
reps = tweets[tweets['type'] == 'representative'][['account_username', 'year', 'account_bio']]
reps = reps.sort_values(by=['account_username', 'year'])

reps = reps.drop_duplicates(subset=['account_username', 'year'])

reps.to_csv('reps.csv')

reps = pd.read_csv('reps_edit.csv')
reps = list(reps[reps['keep'] == 1]['account_username'].unique())

'''
'''
df = pd.read_csv('company_data3.csv')

df['name'] = df['name'].str.lower()

keep = list(df['name'].unique()) + reps

print('comps and representative: ', len(keep))

print('amount of tweets gender: ', len(tweets['account_username']))

tweets = tweets[tweets['account_username'].isin(keep)]

#print(tweets['account_username'].unique())

print('amount of tweets gender username checked: ', len(tweets['account_username']))


'''
tweets_all = pd.read_sql_table(
    'all_tweets_companies4',
    con=engine,
)

tweets_all = functions.clean_tweets(tweets_all, 'company')


tweets_representatives_all = pd.read_sql_table(
    'all_tweets_others4',
    con=engine,
)

tweets_representatives_all = functions.clean_tweets(tweets_representatives_all, 'representative')
tweets_all = tweets_all.append(tweets_representatives_all)

print('amount of tweets all: ', len(tweets_all['account_username']))
#tweets_all = tweets_all[tweets_all['account_username'].isin(keep)]
#print('amount of tweets all username checked: ', len(tweets_all['account_username']))

tweets_all['year'] = [functions.dater(i) for i in tweets_all['created_at']]
#tweets_all['sentiment'] = functions.sentimenter(tweets_all)

keep = pd.read_csv('reps2_edit.csv')
keep = keep[keep['keep'] == 1][['account_username', 'company']]
keep = keep.groupby('account_username')['company'].first().reset_index()
keep.rename(columns = {'company' : 'company2'}, inplace = True)

tweets = tweets.merge(keep, on = 'account_username', how = 'left')
tweets['company'] = np.where(~pd.isnull(tweets['company2']), tweets['company2'], tweets['company'])

print('For # of companies: ', len(tweets[tweets['type'] == 'representative']['company'].unique()))

tweets_all = tweets_all.merge(keep, on = 'account_username', how = 'left')
tweets_all['company'] = np.where(~pd.isnull(tweets_all['company2']), tweets_all['company2'], tweets_all['company'])

print('All tweets of companies: ', len(tweets_all[tweets_all['type'] == 'company']['account_username']))
print('By # of companies: ', len(tweets_all[tweets_all['type'] == 'company']['account_username'].unique()))

print('All tweets of reps: ', len(tweets_all[tweets_all['type'] == 'representative']['account_username']))
print('By # of reps: ', len(tweets_all[tweets_all['type'] == 'representative']['account_username'].unique()))




exit()



#print(tweets_all[tweets_all['type'] == 'representative'][['account_username', 'company']].head(20))


#three different options for data selection. 
#1. only the original tweets of companies
#2. only the tweets of companies
#3. only the original tweets of companies and representatives # for now
#4. all the tweets of companies and representatives

#REMEMBER: good results when companies without gender tweets were not in the sample by accident
#NEEDS WORK: company is now sometimes filled by username of representative, needs fixing.

def followers(tweets_all):
	g = tweets_all.groupby(['account_username', 'year'])[['account_followers', 'company']].last().reset_index() # also need work, does not take representatives into account, generally just wrong
	g = g.groupby(['company', 'year'])['account_followers'].sum().reset_index()
	return(g)

def sentiment_and_counter(tweets, tweets_all):
	def grouper(tweets, all = False):
		tweets_m = tweets.groupby(['company', 'year'])[['likes', 'retweets', 'replies']].mean().reset_index()
		tweets_s = tweets.groupby(['company', 'year'])[['likes', 'retweets', 'replies']].sum().reset_index()
		tweets_c = tweets.groupby(['company', 'year'])['account_username'].count().reset_index()
		if all == False:
			print('gender tweets: ', len(tweets['account_username']))
			tweets_m.rename(columns = {'likes': 'likes_mean', 'retweets': 'retweets_mean', 'replies': 'replies_mean'}, inplace = True)
			tweets_s.rename(columns = {'likes': 'likes_sum', 'retweets': 'retweets_sum', 'replies': 'replies_sum'}, inplace = True)
			tweets_c.rename(columns = {'account_username':'gender_tweets'}, inplace = True)
		else:
			print('all tweets: ', len(tweets_all['account_username']))
			tweets_m.rename(columns = {'likes': 'likes_all', 'retweets': 'retweets_all', 'replies': 'replies_all'}, inplace = True)
			tweets_s.rename(columns = {'likes': 'likes_sum_all', 'retweets': 'retweets_sum_all', 'replies': 'replies_sum_all'}, inplace = True)
			tweets_c.rename(columns = {'account_username':'all_tweets'}, inplace = True)
		tweets = tweets_m.merge(tweets_s, on = ['company', 'year']).merge(tweets_c, on = ['company', 'year'])
		return(tweets)
	g = grouper(tweets)
	g_all = grouper(tweets_all, all = True)
	df = g_all.merge(g, on = ['company', 'year'], how = 'left')
	cols = ['likes_mean', 'retweets_mean', 'replies_mean', 'likes_sum', 'retweets_sum', 'replies_sum', 'gender_tweets']
	
	def f(x):
		return([0 if pd.isnull(value) else value for value in x])
	
	df[cols] = df[cols].apply(f)
	df['gender_tweets_prop'] = df['gender_tweets'] / df['all_tweets']

	return(df)

df = pd.read_csv('company_data3.csv')
df = df.merge(followers(tweets_all), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
print('orginal tweets:')
df = df.merge(sentiment_and_counter(tweets[tweets['type_retweet'] == 'original_tweet'], tweets_all[tweets_all['type_retweet'] == 'original_tweet']), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
pickle.dump(df, open('data_org.pkl', 'wb'))
df.to_csv('data_org.csv')

df = pd.read_csv('company_data3.csv')
df = df.merge(followers(tweets_all), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
print('all tweets:')
df = df.merge(sentiment_and_counter(tweets, tweets_all), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
pickle.dump(df, open('data_all.pkl', 'wb'))
df.to_csv('data_all.csv')

df = pd.read_csv('company_data3.csv')
df = df.merge(followers(tweets_all), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
print('company only tweets:')
df = df.merge(sentiment_and_counter(tweets[tweets['type'] == 'company'], tweets_all[tweets_all['type'] == 'company']), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
pickle.dump(df, open('data_comp.pkl', 'wb'))
df.to_csv('data_comp.csv')

df = pd.read_csv('company_data3.csv')
df = df.merge(followers(tweets_all), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
print('original company only tweets:')
df = df.merge(sentiment_and_counter(tweets[(tweets['type'] == 'company') & (tweets['type_retweet'] == 'original_tweet')], tweets_all[(tweets_all['type'] == 'company') & (tweets_all['type_retweet'] == 'original_tweet')]), left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
pickle.dump(df, open('data_comp_org.pkl', 'wb'))
df.to_csv('data_comp_org.csv')

print('Success')

'''


#def sentiment_grouper(tweets, original_only = False):
#	if original_only == False:
#		r = tweets.groupby(['company', 'year'])['sentiment'].mean()
#	else:
#		r = tweets[tweets['type_retweet'] == 'original_tweet'].groupby(['company', 'year'])['sentiment'].mean()
#	r = pd.DataFrame(r).reset_index()
#	r.rename(columns = {'sentiment':'sentiment_gender'}, inplace = True)
#	return(r)

#t = sentiment_grouper(tweets)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#print(df.columns)
#t = sentiment_grouper(tweets, original_only = True)
#t.rename(columns = {'sentiment_gender': 'sentiment_gender_original'}, inplace = True)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#print(df.columns)
#t = sentiment_grouper(tweets_all)
#t.rename(columns = {'sentiment_gender':'sentiment_all'}, inplace = True)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#print(df.columns)
#t = sentiment_grouper(tweets_all, original_only = True)
#t.rename(columns = {'sentiment_gender': 'sentiment_all_original'}, inplace = True)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#print(df.columns)

#def tweet_counter(tweets, original_only = False):
#	if original_only == False:
#		r = tweets.groupby(['company', 'year'])['account_username'].count()
#	else:
#		r = tweets[tweets['type_retweet'] == 'original_tweet'].groupby(['company', 'year'])['account_username'].count()
#	r = pd.DataFrame(r).reset_index()
#	r.rename(columns = {'account_username':'tweets_gender'}, inplace = True)
#	return(r)

#t = tweet_counter(tweets)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#t = tweet_counter(tweets, original_only = True)
#t.rename(columns = {'tweets_gender':'tweets_gender_original'}, inplace = True)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#t = tweet_counter(tweets_all)
#t.rename(columns = {'tweets_gender':'tweets_all'}, inplace = True)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
#t = tweet_counter(tweets_all, original_only = True)
#t.rename(columns = {'tweets_gender':'tweets_all_original'}, inplace = True)
#df = df.merge(t, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')




def sentimenter(tweets):
	text_clean = tweets['text_clean']
	tweets['sentiment'] = [sentiment(text)[0] for text in text_clean]
	#tweets['type2'] = [i if i == 'stakeholder' else 'company' for i in tweets['type']] #combines the categories of company and representative
	#creates a dataframe of sentiment per company per year per type (company/stakeholder)
	tweets['sentiment_recode'] = np.where(tweets['sentiment'] < 0, -1, 
		np.where(tweets['sentiment'] > 0, 1, 0))
	tweets = tweets.groupby(['company', 'year'])[['sentiment', 'sentiment_recode']].mean()
	tweets = pd.DataFrame(tweets).reset_index()
	tweets = tweets[['company', 'year', 'sentiment', 'sentiment_recode']]
	return(tweets)


'''


'''


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
    



df = pd.read_csv('company_data3.csv')
#df['women_total_'] = np.where(np.isnan(df['women_total']), df['women_8_9'], df['women_total'])
#df['women_management_'] = np.where(np.isnan(df['women_management']), df['women_subtop'], df['women_management'])

tweets = pd.read_pickle(r'tweets.pkl')
#print(tweets.columns)

comps_and_reps = tweets['account_username'].unique()
comps_and_reps = ['@' + string for string in comps_and_reps]

tweets['text_clean'] = [prepare_text_for_lda(text) for text in tweets['text']]

print(tweets['text'])
print(tweets['text_clean'])

'''

'''


comps_and_reps = tweets_representatives_all['account_username'].unique()
comps_and_reps = ['@' + string for string in comps_and_reps]
tweets_all['text_clean'] = [prepare_text_for_lda(text) for text in tweets_all['text']]
tweets_all['year'] = [dater(i) for i in tweets_all['created_at']]


'''

'''
tweets['type2'] = [i if i == 'stakeholder' else 'company' for i in tweets['type']]
tweets_all['type2'] = [i if i == 'stakeholder' else 'company' for i in tweets_all['type']]
print(tweets.groupby(['company', 'type2'])['account_bio'].count())
print(tweets_all.groupby(['company', 'type2'])['account_bio'].count())
'''
'''
tweets = sentimenter(tweets)
df = df.merge(tweets, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')
tweets_all = sentimenter(tweets_all)
tweets_all.rename(columns = {'sentiment':'sentiment_all', 'sentiment_recode': 'sentiment_all_recode'}, inplace = True)
df = df.merge(tweets_all, left_on = ['name', 'year'], right_on = ['company', 'year'], how = 'left')


pickle.dump(df, open('data.pkl', 'wb'))
df.to_csv('data.csv')
print(df.head())
print(df.columns)
print('Success')



#select representatives by filtering on: account_bio contains company name AND username is not company OR list of known representatives
#tweets_representatives_all = tweets_others_all[(tweets_others_all["account_bio"].astype(str).str.contains("amro|hema|heijn|philips|unilever|kpn|klm|shell|aegon|heineken")) & (~tweets_all['account_username'].isin(tweets_others_all['account_username'])) | (tweets_others_all['account_username'] == 'huub_beurskens')]
#tweets_representatives_all['type'] = 'representative'
#printer(tweets_representatives_all)

#select stakeholders: account_username is not in representatives and not in company tweets
#tweets_stakeholders_all = tweets_others_all[(~tweets_others_all['account_username'].isin(tweets_representatives_all['account_username'])) & (~tweets_others_all['account_username'].isin(tweets_all['account_username']))]
#tweets_stakeholders_all['type'] = 'stakeholder'
#printer(tweets_stakeholders_all)



#tweets_all = tweets_all.append(tweets_representatives_all)

#comps_and_reps = tweets_all[tweets_all['type'] != 'stakeholder']['account_username'].unique()






'''
'''

tweets = pd.read_pickle(r'tweets.pkl')
text_clean = tweets['text_clean']

from pattern.nl import sentiment
tweets['sentiment'] = [sentiment(text)[0] for text in text_clean]

tweets['type2'] = [i if i == 'stakeholder' else 'company' for i in tweets['type'] ]

t = tweets.groupby(['company', 'type2'])['sentiment'].mean()
t = pd.DataFrame(t).reset_index()

t = t[t['type2'] == 'stakeholder'][['company','sentiment']]
print(t)

df = pd.read_csv('company_data2.csv')
df['women_total_'] = np.where(np.isnan(df['women_total']), df['women_8_9'], df['women_total'])
df['women_management_'] = np.where(np.isnan(df['women_management']), df['women_subtop'], df['women_management'])

df = df.merge(t, left_on = 'name', right_on = 'company')

tweets_all = pd.read_pickle(r'tweets_all.pkl')
text_clean_all = tweets_all['text_clean']

from pattern.nl import sentiment
tweets_all['sentiment'] = [sentiment(text)[0] for text in text_clean_all]

tweets_all['type2'] = [i if i == 'stakeholder' else 'company' for i in tweets_all['type'] ]

t = tweets_all.groupby(['company', 'type2'])['sentiment'].mean()
t = pd.DataFrame(t).reset_index()

t = t[t['type2'] == 'stakeholder'][['company','sentiment']]
print(t)

'''



'''

mport spacy
from spacy.lang.nl import Dutch

#parser_nl = Dutch()
nlp_nl = spacy.load('nl_core_news_sm')
nlp_en = spacy.load('en_core_web_sm')
stopwords_en = nlp_en.Defaults.stop_words
#account names of companies and representatives


def tokenize(text):
    lda_tokens = []
    tokens = nlp_nl(text)
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

tweets = pd.read_pickle(r'tweets.pkl')

comps_and_reps = tweets['account_username'].unique()
comps_and_reps = ['@' + string for string in comps_and_reps]

print(tokenize(tweets['text'][0]))

import nltk
from nltk.corpus import wordnet as wn

print(wn.langs())

'''