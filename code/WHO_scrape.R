# Some example code to scrape the WHO pdf files of situation reports

# Load in packages
library(tidyverse)
#library(pdftools)
library(tabulizer)

# Take an example file and scrape out the big table of information
file = "https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200320-sitrep-60-covid-19.pdf?sfvrsn=d2bb4f1f_2"
# extract_tables(file) # Currently crashes Rstudio
