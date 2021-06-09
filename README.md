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



## Replicate the results with different translation data:

Requirements: RScript version>= 4.1.0

Run

```
Rscript ncp-stm-replica.R -f data/translated_data.csv -o translated_data
```

If needed install libraries (uncomment code in `ncp-stm-replica.R` script 
