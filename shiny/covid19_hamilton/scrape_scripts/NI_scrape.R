# Scrape data from NI
# This only works since April 1st

# Clear workspace and load in packages
library(tidyverse)
library(pdftools)
library(rvest)
library(xml2)
library(stringr)

# Here's where all the URLs are:
main_url = "https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports"

# Read in the page
main_page = read_html(main_url)

# Find the good URLs
data_urls = html_attr(html_nodes(main_page, "a"), "href") %>% unlist
# Yes they actually called it survellance on one day
pdf_strings = which(!is.na(str_match(data_urls, "Surveillance|Survellance"))) 
pdf_links = data_urls[pdf_strings]

# Create a container for storing everything
max_len = 300
store = tibble(
  Date = rep(as.Date(Sys.time()), max_len),
  Daily_cases = numeric(max_len),
  Daily_deaths = numeric(max_len),
  Cases = numeric(max_len),
  Deaths = numeric(max_len),
  Tests = numeric(max_len),
)
store_age = tibble(
  Age_grp = character(0),
  Value = numeric(0)
)
store_sex = tibble(
  Sex = character(0),
  Value = numeric(0)
)
store_county = tibble(
  County = character(0),
  Value = numeric(0)
)

find_num_fun = function(x, p, n) as.numeric(str_split(x,
                                                      pattern = p)[[1]][n])

# Load them in
for(i in 1:length(pdf_links)) {
  #print(pdf_links[i])
  
  text = pdf_text(pdf_links[i])
  
  # Extract the date
  date_raw = str_extract(text[1], "Data correct to (.*) at")
  store$Date[i] = strptime(date_raw, format = "Data correct to %d/%m/%Y")
  
  if(store$Date[i] <= "2020-03-31") break
  
  # Store split
  totals_split_by_new_line = str_split(text[1], pattern = "\\n")[[1]]
  store$Daily_cases[i] = find_num_fun(totals_split_by_new_line[9], "\\:", 2)
  store$Daily_deaths[i] = find_num_fun(totals_split_by_new_line[11], "\\:", 2)
  store$Cases[i] = find_num_fun(totals_split_by_new_line[16], "\\:", 2)
  store$Deaths[i] = find_num_fun(totals_split_by_new_line[18], "\\:", 2)
  store$Tests[i] = as.numeric(str_extract_all(totals_split_by_new_line[20], "(\\d)+")[[1]][2])
  

  #print(store[i,])
  #Sys.sleep(3)
  
  # Age group
  which_age = which(!is.na(str_match(text, "Age and gender")))
  if(length(which_age) > 0) {
    age_split_by_new_line = str_split(text[which_age], pattern = "\\n")[[1]]
    curr_age = tibble(
      "Date" = store$Date[i],
      "0_to_44" = as.numeric(str_split(age_split_by_new_line[3],
                                       pattern = "\\s{2,}")[[1]][4]),
      "45_to_69" = as.numeric(str_split(age_split_by_new_line[4],
                                        pattern = "\\s{2,}")[[1]][3]),
      "70+" = as.numeric(str_split(age_split_by_new_line[5],
                                   pattern = "\\s{2,}")[[1]][3])) %>% 
      pivot_longer(names_to = 'Age_grp', values_to = 'Value', -Date) %>% 
      mutate(Date = store$Date[i])
    store_age = bind_rows(store_age, curr_age)
    
    # Sex
    curr_sex = tibble(
      "Date" = store$Date[i],
      "Male" = as.numeric(str_split(age_split_by_new_line[7],
                                    pattern = "\\s{2,}")[[1]][3]),
      "Female" = as.numeric(str_split(age_split_by_new_line[6],
                                      pattern = "\\s{2,}")[[1]][4]),
      "Unknown" = as.numeric(str_split(age_split_by_new_line[8],
                                       pattern = "\\s{2,}")[[1]][3])) %>% 
      pivot_longer(names_to = 'Sex', values_to = 'Value', -Date) %>% 
      mutate(Date = store$Date[i])
    store_sex = bind_rows(store_sex, curr_sex)
  }
  
  
  # By county
  which_county = which(!is.na(str_match(text, "Antrim")))
  if(length(which_county) > 0) {
    county_raw = str_split(text[which_county], pattern = "\\n")[[1]]
    county = map(county_raw, ~str_split(str_trim(.x), "\\s{2,}"))
    counties = map(county, function(x) x[[1]][1]) %>% unlist
    values = map(county, function(x) x[[1]][2]) %>% unlist
    curr_county = tibble(
      "Date" = store$Date[i],
      "County" = counties[3:13],
      "Value" = as.numeric(values[3:13])
    )
    store_county = bind_rows(store_county, curr_county)
  }

}

latest_NI_data = list(totals = store %>% filter(Cases > 0),
                      totals_age = store_age,
                      totals_sex = store_sex,
                      totals_county = store_county)
