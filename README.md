# nlstm

Student project to replicate the Study “Explaining topic prevalence in
answers to open-ended survey questions about climate change”, Tvinnereim, Fløttum 2015

## Translation of the data

Requirements: python3


To translate the norwegian data that can be found in `data/original/ncp-stm-data.csv` (default) run:

```
virtualenv venv
source venv/bin/activate
pip install -r requirements.txt 
python3 translate.py --csv file_to_translate --output venvtest2.csv --library 2 --sep , --column openanswer
deactivate
```

## Replicates 
### Original unaltered data

To be found in `data/original/`

### STM original replication

To be found in `src/original/`

### English translation of the data

To be found in `data/english-translation/`

### Icelandic translation of the data

To be found in ` data/icelandic-translation/`


### Original slightly altered script to replicate the results

To be found in `src/R/original-replicated/`

Run with:

```
Rscript src/R/lda-ctm/ncp-ctm.R -f data/translated_data.csv -a translated_data_replica -p global_path_to_repo

```

### Exploratory data plots

To be found in `src/R/preprocess/ncp-exploratory.R`

Run with:

```
Rscript src/R/lda-ctm/ncp-exploratory.R -f data/translated_data.csv -a translated_data_replica -p global_path_to_repo

```

### Script to run STM with translated data

To be found in `src/R/stm-translated/ncp-stm-translated-data.R`
Run with:

```
Rscript src/R/lda-ctm/ncp-stm-translated-data.R -f data/translated_data.csv -a translated_data_replica -p global_path_to_repo

```



## CTM

### CTM with original and translated data

The script is located in `src/R/lda-ctm/ncp-ctm.Rls
`

Run with:

```
Rscript src/R/lda-ctm/ncp-ctm.R -f data/translated_data.csv -a translated_data_replica -p global_path_to_repo

```


## LDA

### LDA original and english translated data

The script is located in `src/R/lda-ctm/ncp-lda.R`


Run with:

```
Rscript src/R/lda-ctm/ncp-lda.R -f data/translated_data.csv -a translated_data_replica -p global_path_to_repo

```


# Python Code

## OCTIS: CTM and LDA topic modeling and evaluation

To be found in `src/notebooks/OCTIS/OCTIS.ipynb`

## BERTopic

To be found in: `src/notebooks/Bertopic`


## LDA

To be found in: `src/notebooks/LDA` 