import argparse

import g_translator as gt
import textblob_translator as tt
import translator as t

if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description="Simple translation into english script, " +
        "takes a csv file, translates a specific column, " +
        "adds the translated strings and writes to a new csv file")
    parser.add_argument(
        "--lang",
        dest="lang",
        type=str,
        help="Language to translate to, code according to ISO 639-1, " +
        "interesting would be is, de, da, sv ",
        default="en",
    )
    parser.add_argument(
        "--library",
        dest="lib",
        type=int,
        help="Name of the library to use, default is google_trans_new[1], " +
        "options are: translatepy = 2, textblob = 3",
        default=1,
    )
    parser.add_argument(
        "--csv",
        dest="csv",
        type=str,
        help="csv file to translate",
        default="data/data.csv",
    )
    parser.add_argument(
        "--sep",
        dest="sep",
        type=str,
        help="Name of the column to translate",
        default=";",
    )
    parser.add_argument(
        "--column",
        dest="col",
        type=str,
        help="Name of the column to translate",
        default="openanswer",
    )
    parser.add_argument(
        "--output",
        dest="output",
        type=str,
        help="Name of the new csv with translation to save.",
        default="data/translated1.csv",
    )

    args = parser.parse_args()

    if args.lib == 1:
        trans = gt.g_translator(args.lang)
        trans.read_csv(args.csv, args.sep)
        trans.translate_df(args.col)
        trans.save(args.output, args.sep)
    elif args.lib == 3:
        trans = tt.textblob_translator(args.lang)
        trans.read_csv(args.csv, args.sep)
        trans.translate(args.col)
        trans.save(args.output, args.sep)
    elif args.lib == 2:
        trans = t.translator(args.lang)
        trans.read_csv(args.csv, args.sep)
        trans.translate(args.col)
        trans.save(args.output, args.sep)
    else:
        print("wrong")
