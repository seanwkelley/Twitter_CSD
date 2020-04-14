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
from timezonefinder import TimezoneFinder 
import random

tf = TimezoneFinder()
def alpha_numeric_string():
    x = ''.join(random.choice(string.ascii_uppercase + string.ascii_lowercase + string.digits) for _ in range(16))
    return(x)

print(alpha_numeric_string())

