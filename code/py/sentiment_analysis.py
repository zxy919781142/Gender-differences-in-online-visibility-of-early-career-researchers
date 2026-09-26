## https://www.pluralsight.com/guides/building-a-twitter-sentiment-analysis-in-python
import pandas as pd
import numpy as np
import re
import string
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.model_selection import train_test_split
from nltk.stem import PorterStemmer
from nltk.stem import WordNetLemmatizer
# ML Libraries
from sklearn.metrics import accuracy_score
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC

# Global Parameters
stop_words = set(stopwords.words('english'))

def load_dataset(filename, cols):
    dataset = pd.read_csv(filename, encoding='latin-1')
    dataset.columns = cols
    return dataset

def remove_unwanted_cols(dataset, cols):
    for col in cols:
        del dataset[col]
    return dataset

### process steps:
### 1.Letter casing: Converting all letters to either upper case or lower case.
### 2.Tokenizing: Turning the tweets into tokens. Tokens are words separated by spaces in a text.
### 3.Noise removal: Eliminating unwanted characters, such as HTML tags, punctuation marks, special characters, white spaces etc.
### 4.Stopword removal: Some words do not contribute much to the machine learning model, so it's good to remove them. A list of stopwords can be defined by the nltk library, or it can be business-specific.
### 5.Normalization: Normalization generally refers to a series of related tasks meant to put all text on the same level. Converting text to lower case, removing special characters, and removing stopwords will remove basic inconsistencies. Normalization improves text matching.
### 6.Stemming: Eliminating affixes (circumfixes, suffixes, prefixes, infixes) from a word in order to obtain a word stem. Porter Stemmer is the most widely used technique because it is very fast. Generally, stemming chops off end of the word, and mostly it works fine. Example: Working -> Work
### 7.Lemmatization: The goal is same as with stemming, but stemming a word sometimes loses the actual meaning of the word. Lemmatization usually refers to doing things properly using vocabulary and morphological analysis of words. It returns the base or dictionary form of a word, also known as the lemma. Example: Better -> Good.

def preprocess_tweet_text(tweet):
    tweet.lower()
    # Remove urls
    tweet = re.sub(r"http\S+|www\S+|https\S+", '', tweet, flags=re.MULTILINE)
    # Remove user @ references and '#' from tweet
    tweet = re.sub(r'\@\w+|\#','', tweet)
    # Remove punctuations
    tweet = tweet.translate(str.maketrans('', '', string.punctuation))
    # Remove stopwords
    tweet_tokens = word_tokenize(tweet)
    filtered_words = [w for w in tweet_tokens if not w in stop_words]
    

  
    return " ".join(filtered_words)


def get_feature_vector(train_fit):
    vector = TfidfVectorizer(sublinear_tf=True)
    vector.fit(train_fit)
    return vector


def int_to_string(sentiment):
    if sentiment == 0:
        return "Negative"
    elif sentiment == 2:
        return "Neutral"
    else:
        return "Positive"
    
def model_training():
    
    dataset = load_dataset("training.csv", ['target', 't_id', 'created_at', 'query', 'user', 'text'])
    3# Remove unwanted columns from dataset
    n_dataset = remove_unwanted_cols(dataset, ['t_id', 'created_at', 'query', 'user'])
    5#Preprocess data
    dataset.text = dataset['text'].apply(preprocess_tweet_text)
    tf_vector = get_feature_vector(np.array(dataset.iloc[:, 1]).ravel())
    
    X = tf_vector.transform(np.array(dataset.iloc[:, 1]).ravel())
    y = np.array(dataset.iloc[:, 0]).ravel()
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=30)

    15# Training Naive Bayes model
    NB_model = MultinomialNB()
    NB_model.fit(X_train, y_train)

    # Training Logistics Regression model
    LR_model = LogisticRegression(solver='lbfgs')
    LR_model.fit(X_train, y_train)
    
    return tf_vector,NB_model,LR_model
    
    
def text_analysis(tweet,model,tf_vector):    

    tweet = preprocess_tweet_text(tweet)
    test_feature = tf_vector.transform(np.array([tweet]))

    test_prediction_lr= model.predict(test_feature)
   
    return int_to_string(test_prediction_lr)
