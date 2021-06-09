from textblob import TextBlob
from textblob import exceptions
import pandas as pd

class textblob_translator():

    def read_csv(self, data='data.csv', sep=";", enc = "windows-1252"):
        self.text = pd.read_csv(data, sep=sep, encoding=enc)

    def translate(self, column='openanswer'):
        translated = []
        for t in self.text[column]:
            try:
                tmp = TextBlob(t).translate(to='en').string
            except exceptions.NotTranslated:
                tmp = "NotTranslated"
                print("NotTranslated")
            translated.append(tmp)
        self.text['translation'] = translated

    def save(self, filename='translated_data_google.csv', sep=';'):
        self.text.to_csv(filename, sep = sep)




