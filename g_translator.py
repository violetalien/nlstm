from google_trans_new import google_translator
import pandas as pd

class g_translator():

    def read_csv(self, data='data.csv', sep=";", enc = "windows-1252"):
        self.text = pd.read_csv(data, sep=sep, encoding=enc)

    def translate_df(self, column='openanswer'):
        off =  google_translator()  
 #       translated = []
 #       for i in self.text['openanswer']:
 #           tmp = off.translate(i, lang_tgt='en')
 #           translated.append(tmp)

        translated = [off.translate(i, lang_tgt='en') for i in self.text[column]]
        self.text['translation']=translated

    def save(self, filename='translated_data_google.csv', sep=';'):
        self.text.to_csv(filename, sep = sep)





