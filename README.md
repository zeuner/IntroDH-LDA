% LDA Analysis of some European parliamentary protocols

- Belgium (1999-2019)
- Czechia (2010-2021)
- Austria (1999-2019)
- Spain (2011-2021)
- Hungary (2014-2021)
- Ireland (2016-2020)
- Italy (2013-2018)
- Netherlands (2014-2021)
- France (2012-2021)
- Poland (1991-2021)

# What is included:

- scrape-??.R scripts to get the protocols for the different countries
- lda-??.R LDA analysis stub to extract a topic model from the protocols
- analyse-??.R extracting summaries from the LDA topic models
- scrape.R utility code to help scraping the protocols
- lda-generic.R generic LDA topic modeling workflow for all countries

# What is required:

Please be sure to have the necessary spell checker modules for the
different languages installed. On Debian-/Ubuntu-based operating systems,
the following packages will be required to run the analyse-??.R and
country-references.R scripts:

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
