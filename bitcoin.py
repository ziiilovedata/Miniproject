#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 21 11:07:22 2021

@author: Nikkikong
"""

import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime as dt
from nltk.corpus import stopwords
from wordcloud import WordCloud
import string

bc = pd.read_csv('/Users/nikkkikong/Desktop/Project/BitcoinNews_2021_1.csv')

df2 = bc.groupby(['Date']).count()

df2.to_csv('nee.csv', index=False)

df2.reset_index(inplace=True)
df2['Date'] =pd.to_datetime(df2.Date)
df2=df2.sort_values(by='Date')
df2['Date'] = df2['Date'].dt.strftime('%m-%d')


fig = plt.figure(figsize=(15,10))
plt.plot(df2['Date'], df2['Source'],marker='D',mfc='red',color='#607c8e')
plt.title("Frequency of Bitcoin News by Date",fontsize=20)
plt.xlabel("Year: 2021")
plt.ylabel("Frequency")
#plt.xticks(rotation=90)
fig.autofmt_xdate()
plt.show()

fig = plt.figure(figsize=(15,10))
plt.hist(bc['Source'], rwidth=0.9,color='#607c8e')
plt.xticks(rotation=45)
plt.title("Frequency of Bitcoin News by Source",fontsize=20)
plt.ylabel("Frequency")
plt.show()



# Create three strings, remove punctuation and stopwords.
stoplist = stopwords.words('english')
table = str.maketrans(dict.fromkeys(string.punctuation))  

title_string = []
for t in bc.Title:
    title_string.append(t)
title_text = pd.Series(title_string).str.cat(sep=' ')
title_text=title_text.lower()
title_text = title_text.translate(table)     
clean_title_list = str([word for word in title_text.split() if word not in stoplist])

headline_string = []
for t in bc.Headline:
    headline_string.append(t)
headline_text = pd.Series(headline_string).str.cat(sep=' ')
headline_text=headline_text.lower()
headline_text = headline_text.translate(table)     
clean_headline_list = str([word for word in headline_text.split() if word not in stoplist])




# plot the WordCloud image

wordcloud_title = WordCloud(width = 800, height = 800, 
                background_color ='white', max_words=50,
                min_font_size = 10, colormap="Paired").generate(clean_title_list)
plt.figure(figsize = (12, 8), facecolor = None) 
plt.imshow(wordcloud_title) 
plt.axis("off") 
plt.tight_layout(pad = 0) 
plt.show() 



wordcloud_headline = WordCloud(width = 800, height = 800, 
                background_color ='white', max_words=50,
                min_font_size = 10).generate(clean_headline_list)
plt.figure(figsize = (12, 8), facecolor = None) 
plt.imshow(wordcloud_headline) 
plt.axis("off") 
plt.tight_layout(pad = 0) 
plt.show() 




bc_new = pd.read_csv('newshealinenew.csv')

t= bc_new["Headline"].values.tolist()
t = [x + "." for x in t]
def concatenate_list_data(list):
    result= ''
    for element in list:
        result += str(element)
    return result
temp=concatenate_list_data(t)