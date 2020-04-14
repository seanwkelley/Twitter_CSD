#Sentiment Analysis of all participants in Participants.csv file 
import os
import re
import pandas as pd
import string 
import emoji
import json
import csv 
import numpy as np
import contractions 
import nltk
import seaborn as sns
import matplotlib.pyplot as plt
from bs4 import BeautifulSoup
from re import finditer
from nltk.corpus import stopwords
from nltk.corpus import wordnet
from textblob import TextBlob
from nltk.tokenize import word_tokenize 
from nltk.stem import WordNetLemmatizer 
import wordninja #great packagee to split any compound words that are primarily from hashtags 
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from dateutil import parser
from dateutil.relativedelta import relativedelta
import matplotlib.pyplot as plt
import spacy
from spacy.tokenizer import Tokenizer
nlp = spacy.load('en_core_web_sm')
from nltk.tokenize import word_tokenize 
tokenizer = Tokenizer(nlp.vocab)
from scipy import interpolate
from functools import reduce 
import shutil
from shutil import rmtree
from sys import argv
import stat
import datetime, pytz
from pytz import timezone
#from datetime import datetime
from dateutil import tz
from langdetect import detect
from langdetect import detect_langs
from langdetect import DetectorFactory 
DetectorFactory.seed = 0
import random, string
from numpy.random import seed
from numpy.random import randint
import cryptography
from cryptography.fernet import Fernet
from io import StringIO
from timezonefinder import TimezoneFinder 
tf = TimezoneFinder()
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
##########################################################################################
###########################################################################################
#Set path directory and type of tweet in set_wd_path.txt 
with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0]
tweet_type = set_wd_path[1]

os.chdir(path)
###########################################################################################
def get_dates(handle,episode_number):
        try: 
            start,end = "1", "2"
            handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
            episode_1 = str(handle_row['Depression_Dates.1_'  + episode_number + '_' + start])
            episode_1 = episode_1.split(' ')[4]
            episode_1 = episode_1.split('\n')[0]
            episode_1 = datetime.datetime.strptime(episode_1, "%d/%m/%Y")
            episode_2 = str(handle_row['Depression_Dates.1_' + episode_number + '_' + end])
            episode_2 = episode_2.split(' ')[4]
            episode_2 = episode_2.split('\n')[0]
            episode_2 = datetime.datetime.strptime(episode_2, "%d/%m/%Y")
            episodes = [episode_1,episode_2]
            
        except (KeyError,IndexError,ValueError):
            episodes = []
        return (episodes)

def submission_date(handle):
    handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
    episode_1 = str(handle_row['EndDate'])
    episode_1 = (handle_row['EndDate']).to_string()

    episode_1 = episode_1.split(' ')
    episodes = []
    for i in episode_1:
        episodes.append(i)
    episodes = episodes[4]
    return (episodes)
###########################################################################################
#read in the sentiments from VADER, ANEW, and LIWC 
#rename columns that were altered by LIWC and drop unnecessary or redundant columns 
os.chdir(path)

file = open('Participant_Data/key.key', 'rb')
key = file.read()
file.close()

#participants data file 
with open('Participant_Data/FYP_Twitter_Participants_23.03.csv.encrypted', 'rb') as f:
    data = f.read()

#de-encrypt the Participants.csv data file 
fernet = Fernet(key)
encrypted = fernet.decrypt(data)


participants_encrypted = str(encrypted,'utf-8')
data = StringIO(participants_encrypted) 

Participant_info = Dates = pd.read_csv(data)


file_names = list(Participant_info['Twitter_Handle'])
id_dict = dict(zip(Participant_info['Twitter_Handle'],Participant_info['Id']))

#results of sentiment analysis 
VADER_ANEW_LIWC = pd.read_csv(path + 'Sentiments/' + tweet_type + '/' + 'VADER_ANEW_LIWC_complete_dep.csv',encoding="utf-8")


for i in range(1,2):
    
    handle = file_names[i]

    random_id = id_dict[handle]
    
    print(random_id)
    sentiments_handle = VADER_ANEW_LIWC[VADER_ANEW_LIWC['Twitter_Handle'] == random_id]

    print(sentiments_handle['negemo'])
    sentiments_handle['negemo'] = sentiments_handle['negemo'].interpolate(method='linear')
    print(sentiments_handle['negemo'])