% LDA Analysis of some European parliamentary protocols

- Belgium (time interval?)
- Czechia (time interval?)
- Austria (time interval?)
- Spain (time interval?)
- Hungary (2014-2018 cycle)
- Ireland (time interval?)
- Italy (time interval?)
- Netherlands (time interval?)
- France (time interval?)
- Poland (time interval?)

# What is included:

- scrape-??.R scripts to get the protocols for the different countries
- lda-??.R LDA analysis stub to extract a topic model from the protocols
- scrape.R utility code to help scraping the protocols
- lda-generic.R generic LDA topic modeling workflow for all countries

# What is required:

Please be sure to have the necessary spell checker modules for the
different languages installed. On Debian-/Ubuntu-based operating systems,
the following packages will be required to run the lda-??.R scripts:

- hunspell-cs
- hunspell-de-at
- hunspell-en-gb
- hunspell-es
- hunspell-fr
- hunspell-hu
- hunspell-it
- hunspell-nl
- hunspell-pl

# Already implemented

- downloading the necessary data
- converting the downloaded pdf files into txt format
- cleaning the txt files
- tokenizing the texts, exporting them to csv format (columns: line,word,origin)\
  TIP: this step requires substantial computing power, thus might take long to finish
  
# TODO

- Analysis
- exporting results 
