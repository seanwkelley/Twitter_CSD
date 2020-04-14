import os
from os import listdir
import pandas as pd 
from langdetect import detect
from langdetect import detect_langs
from langdetect import DetectorFactory 
DetectorFactory.seed = 0
from random import seed
from random import randint
from functools import reduce
seed(123)
###########################################################################################
#Set path directory and type of tweet in set_wd_path.txt 
with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0]
tweet_type = set_wd_path[1]

os.chdir(path)
###########################################################################################

#read in the sentiments from VADER, ANEW, and LIWC 
VADER_ANEW_LIWC = pd.read_csv(path + 'Sentiments/' + tweet_type + '/' + 'VADER_ANEW_LIWC.csv',encoding="utf-8")

#rename columns that are changed in LIWC 
VADER_ANEW_LIWC = VADER_ANEW_LIWC.drop(['C'],axis=1)
VADER_ANEW_LIWC = VADER_ANEW_LIWC.rename({'A': 'Day', 'B': 'Twitter_Handle','D':'recruitment_type','E':'negative','F':'neutral','G':'positive','H':'compound','I':'valence','J':'arousal','K':'dominance'}, axis=1)


#average of sentiments across the past year per participant 
VADER_ANEW_LIWC_mean = VADER_ANEW_LIWC.groupby('Twitter_Handle').mean()

VADER_ANEW_LIWC_mean = VADER_ANEW_LIWC_mean.drop('Day',axis=1)
VADER_ANEW_LIWC_mean = VADER_ANEW_LIWC_mean.reset_index(level='Twitter_Handle')
VADER_ANEW_LIWC_mean.rename(columns = {'Twitter_Handle': 'Id'}, inplace=True)

VADER_ANEW_LIWC_mean = VADER_ANEW_LIWC_mean.drop(['negative','neutral','positive','compound','valence','Comma',
						'Colon','SemiC','Dash','Quote','Parenth','OtherP'],axis=1)

VADER_ANEW_LIWC_mean.to_csv("Sentiments/" + tweet_type + "/VADER_ANEW_LIWC_mean.csv",index = False)

