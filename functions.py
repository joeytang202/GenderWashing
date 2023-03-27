from twarc import Twarc2, expansions
from keys import consumer_key, consumer_secret, bearer_token

from logging import error
from re import L
from typing import Text
from twarc import Twarc2, expansions
import json
import datetime
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

from pattern.nl import sentiment as sentiment_nl
from pattern.en import sentiment as sentiment_en



bearertoken = bearer_token
#initiate client with bearer_token
client = Twarc2(bearer_token=bearertoken)
 
engine = sqlalchemy.create_engine('mysql://root:Batavus202*@localhost/twitter')

def loader(query, start_time, end_time, table, company):
    print(company)
    search_results = client.search_all(query=query, start_time=start_time, end_time=end_time, max_results=100)
    # #Twarc returns all Tweets for the criteria set above, so we page through the results
    ## Other endpoints are possible as well. See the API documentation for this
    tweetnr=1
    pagenr=1
    for page in search_results:
        df = pd.DataFrame()
        # The Twitter API v2 returns the Tweet information and the user, media etc.  separately
        # so we use expansions.flatten to get all the information in a single JSON
        page = expansions.flatten(page)
        #print("ON PAGE NUMBER " + str(pagenr))
        #resultslist.append(page)
        for tweet in page:
            #print("Tweet number " + str(tweetnr) + " on page number " + str(pagenr))
            tweet_id = tweet["id"]
            text = tweet["text"]
            author_id = tweet["author_id"]
            conversation_id = tweet["conversation_id"]
            created_at = tweet["created_at"]
            try:
                in_reply_to_user = tweet["in_reply_to_user_id"]
            except:
                in_reply_to_user = "NA"
            lang = tweet["lang"]
            possibly_sensitive = tweet["possibly_sensitive"]
            retweets = tweet["public_metrics"]["retweet_count"]
            replies = tweet["public_metrics"]["reply_count"]
            likes = tweet["public_metrics"]["like_count"]
            quotes = tweet["public_metrics"]["quote_count"]
            source = tweet["source"]
        
            try:
                geo_full_name= tweet["geo"]["full_name"]
            except:
                geo_full_name= "NA"
        
            try:
                geo_country=tweet["geo"]["country"]
            except:
                geo_country= "NA"

            try:
                geo_country_code=tweet["geo"]["country_code"]
            except:
                geo_country_code= "NA"
        
            try:
                geo_coordinates= tweet["geo"]["geo"]["bbox"]
            except:
                geo_coordinates= "NA"
                    

            #referenced tweet level
            try:
                type_retweet = tweet["referenced_tweets"][0]["type"]
            except KeyError:
                type_retweet = "original_tweet"
            try:
                org_tweet_text = tweet["referenced_tweets"][0]["text"]
            except KeyError:
                org_tweet_text = tweet["text"]

            try:
                org_tweet_id = tweet["referenced_tweets"][0]["id"]
            except KeyError:
                org_tweet_id = "NA"

            try:
                org_author_id = tweet["referenced_tweets"][0]["author_id"]
            except KeyError:
                org_author_id = "NA"

            try:
                org_conversation_id = tweet["referenced_tweets"][0]["conversation_id"]
            except KeyError:
                org_conversation_id = "NA"

            try:
                org_created_at = tweet["referenced_tweets"][0]["created_at"]
            except KeyError:
                org_created_at = "NA"

            try:
                org_retweets = tweet["referenced_tweets"][0]["public_metrics"]["retweet_count"]
            except KeyError:
                org_retweets = "NA"
            try:
                org_replies = tweet["referenced_tweets"][0]["public_metrics"]["reply_count"]
            except KeyError:
                org_replies = "NA"

            try:
                org_likes = tweet["referenced_tweets"][0]["public_metrics"]["like_count"]
            except KeyError:
                org_likes = "NA"

            try:
                org_quotes = tweet["referenced_tweets"][0]["public_metrics"]["quote_count"]
            except KeyError:
                org_quotes = "NA"

            org_mentions = []
            try:
                mentionslist = tweet["referenced_tweets"][0]["entities"]["mentions"]
                for mention in mentionslist:
                    org_mentions.append(mention["username"])     
            except KeyError:
                org_mentions = "NA"

            org_hashtags =[]
            try:
                for tag in tweet["referenced_tweets"][0]["entities"]["hashtags"]:
                    org_hashtags.append(tag["tag"])
            except KeyError:
                org_hashtags = "NA"
            
            org_urls = []
            try:
                for url in tweet["referenced_tweets"][0]["entities"]["urls"]:
                    #print(url["expanded_url"])
                    org_urls.append(url["expanded_url"])
            except KeyError:
                org_urls = "NA"

            #original tweet account data
            try:
                org_account_username = tweet["referenced_tweets"][0]["author"]["username"]
            except KeyError:
                org_account_username = "NA"

            try:
                org_account_followers = tweet["referenced_tweets"][0]["author"]["public_metrics"]["followers_count"]
            except KeyError:
                org_account_followers = "NA"

            try:
                org_account_following = tweet["referenced_tweets"][0]["author"]["public_metrics"]["following_count"]
            except KeyError:
                org_account_following = "NA"

            try:
                org_account_volume = tweet["referenced_tweets"][0]["author"]["public_metrics"]["tweet_count"]
            except KeyError:
                org_account_volume = "NA"

            try:
                org_account_bio = tweet["referenced_tweets"][0]["author"]["description"]
            except KeyError:
                org_account_bio = "NA"

            try:
                org_account_created_at = tweet["referenced_tweets"][0]["author"]["created_at"]
            except KeyError:
                org_account_created_at = "NA"
        
            try:
                org_account_pic = tweet["referenced_tweets"][0]["author"]["profile_image_url"]
                org_account_pic = str(org_account_pic[0:-10] + "400x400.jpg")
            except KeyError:
                org_account_pic = "NA"    
        
            mentions=[]
            try:
                for mention in tweet["entities"]["mentions"]:
                    mentions.append(mention["username"])
            except:
                mentions = "NA"

            hashtags =[]
            try:
                for tag in tweet["entities"]["hashtags"]:
                    hashtags.append(tag["tag"])
            except:
                hashtags = "NA"

            urls = []
            titles =[]
            descs =[]
            try:
                for url in tweet["entities"]["urls"]:
                    urls.append(url["expanded_url"])
                    titles.append(url["title"])
                    descs.append(url["description"])
            except:
                urls= "NA"
                titles="NA"
                descs= "NA"

            #account level vars
            account_username = tweet["author"]["username"]
            account_followers = tweet["author"]["public_metrics"]["followers_count"]
            account_following = tweet["author"]["public_metrics"]["following_count"]
            account_volume = tweet["author"]["public_metrics"]["tweet_count"]
            account_bio = tweet["author"]["description"]
            account_created_at = tweet["author"]["created_at"]
        
            try:
                account_location = tweet["author"]["location"]
            except:
                account_location = "NA"
        
            try:
                account_pic = str(tweet["author"]["profile_image_url"][0:-10] + "400x400.jpg")
            except KeyError:
                account_pic = "NA"
        

            #bind together as row for df
            tweetdata = {
                "tweet_id": tweet_id,
                "text": text,
                "author_id" : author_id,
                "conversation_id": conversation_id,
                "created_at": created_at,
                "in_reply_to_user" : in_reply_to_user,
                "lang" : lang,
                "possibly_sensitive" : possibly_sensitive,
                "retweets" : retweets,
                "replies" : replies, 
                "likes" : likes,
                "quotes" : quotes,
                "source" : source,
                "geo_full_name":geo_full_name,
                "geo_country":geo_country,
                "geo_country_code":geo_country_code,
                "geo_coordinates":str(geo_coordinates),
                "mentions" : str(mentions),
                "hashtags" : str(hashtags),
                "urls" : str(urls),
                "urls_titles" : str(titles),
                "urls_descs" : str(descs),
                "type_retweet" : type_retweet,
                "org_tweet_text" : org_tweet_text,
                "org_tweet_id" : org_tweet_id,
                "org_author_id" : org_author_id,
                "org_conversation_id": org_conversation_id,
                "org_created_at" : org_created_at,
                "org_retweets" : org_retweets,
                "org_replies" : org_replies,
                "org_likes" : org_likes,
                "org_quotes" : org_quotes,
                "org_mentions" : str(org_mentions),
                "org_hashtags" : str(org_hashtags),
                "org_urls" : str(org_urls),
                "org_account_username" : org_account_username,
                "org_account_followers" : org_account_followers,
                "org_account_following" : org_account_following,
                "org_account_volume" : org_account_volume,
                "org_account_bio" : org_account_bio,
                "org_account_created_at" : org_account_created_at,
                "org_account_pic": org_account_pic,
                "account_username" : account_username,
                "account_followers" : account_followers,
                "account_following" : account_following,
                "account_volume" : account_volume,
                "account_bio" : account_bio,
                "account_created_at" : account_created_at,
                "account_location" : account_location,
                "account_pic" : account_pic
            }

            df = df.append(tweetdata, ignore_index= True)
            df['company'] = company
            tweetnr = tweetnr+1
            #if tweetnr > 500:
            #    break

        df.to_sql(name=table, con=engine, index=False, if_exists='append')
        #lastdate= print("last date is: " + str(created_at))

        pagenr=pagenr+1
        if pagenr % 25 == 0:
            print("ON PAGE NUMBER " + str(pagenr))

    #You are done!
    print("Scraping done!")
    


def clean_tweets(tweets, type_):
    tweets['account_bio'] = tweets['account_bio'].str.lower()
    tweets['account_username'] = tweets['account_username'].str.lower()
    #give tweets type 'company'
    tweets['type'] = type_
    return(tweets)

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

def dater(string):
    if '2021' in string:
        return(2021)
    elif '2020' in string: 
        return(2020)
    elif '2019' in string:
        return(2019)
    elif '2018' in string:
        return(2018)
    elif '2017' in string:
        return(2017)

def sentimenter(tweets):
    sentiment = [sentiment_nl(text)[0] if lang == 'nl' else sentiment_en(text)[0] for text, lang in zip(tweets['text'], tweets['lang'])]
    return sentiment
