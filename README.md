% LDA Analysis of Hungarian parliamentary protocols (2014-2018 cycle)

# What is included:

- lda.R: the main script, which downloads the necessary data, processes it and generates the results

The following locales have to be installed:

- cs_CZ.UTF-8
- fr_BE.UTF-8
- fr_FR.UTF-8

# Already implemented

- downloading the necessary data
- converting the downloaded pdf files into txt format
- cleaning the txt files
- tokenizing the texts, exporting them to csv format (columns: line,word,origin)\
  TIP: this step requires substantial computing power, thus might take long to finish
  
# TODO

- Analysis
- exporting results 
