import pandas as pd
import translatepy


class translator():

    def __init__(self):
        self.translator = translatepy.Translator()  

    def read_csv(self, data='data.csv', sep=";", enc = "windows-1252"):
        self.text = pd.read_csv(data, sep=sep, encoding=enc)


    def translate(self, column='openanswer'):
        translated = [self.translator.translate(t, "English").result for t in self.text[column]]
        self.text['translation'] = translated

    def save(self, filename='translated_data_google.csv', sep=';'):
        self.text.to_csv(filename, sep = sep)





