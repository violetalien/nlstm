import pandas as pd
import translatepy


class translator():

    def __init__(self, lang='en'):
        self.translator = translatepy.Translator()  
        self.language = lang

    def read_csv(self, data='data.csv', sep=";", enc = "windows-1252"):
        self.text = pd.read_csv(data, sep=sep, encoding=enc)


    def translate(self, column='openanswer'):
        translated = [self.translator.translate(t, self.language).result for t in self.text[column]]
        self.text['translation'] = translated

    def save(self, filename='translated_data_translatepy.csv', sep=';'):
        self.text.to_csv(filename, sep = sep)





