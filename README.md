# nlstm


## translate the norwegian data

Requirements: python3


To translate the norwegian data that can be found in `data/data.csv` (default) run:

```
virtualenv venv
source venv/bin/activate
pip install -r requirements.txt 
python3 translate.py --output venvtest2.csv --library 2
deactivate
```

## Replicates 

Requirements: RScript version>= 4.1.0

### Of the original results
Run from the root of this repo

You need to have the R script and the data in the same directory. Than run:

```
Rscript original-replicated/ncp-stm.R 

```

### With translated data:

```

Rscript ncp-translated/ncp-stm-translated-data.R -f data/translated_data.csv -a translated_data_replica
```

If needed install libraries (uncomment code in `ncp-stm-replica.R` script 



# Plan


1. BERTtopic with norwegian
2. LDA 
3. how to perform comparison - smenatic coherence? these are different embeddings
4. How to perform meta analysis -> age/gender/education/topics
5. https://github.com/MIND-Lab/OCTIS
