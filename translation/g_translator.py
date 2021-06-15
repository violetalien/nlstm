from google_trans_new import google_translator
import pandas as pd

class g_translator():

    def __init__(self, lang='en'):
        self.language = lang

    def read_csv(self, data='data.csv', sep=";", enc = "windows-1252"):
        self.text = pd.read_csv(data, sep=sep, encoding=enc)

    def translate_df(self, column='openanswer'):
        off =  google_translator()  
        translated = [off.translate(i, lang_tgt=self.language) for i in self.text[column]]
        self.text['translation']=translated

    def save(self, filename='translated_data_google.csv', sep=';'):
        self.text.to_csv(filename, sep = sep)





