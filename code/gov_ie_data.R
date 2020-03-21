library(tidyverse)
library(xml2)
library(rvest)


main_url <- "https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/" %>% 
  xml2::read_html()
urls <- html_attr(html_nodes(main_url, "a"), "href")  

corona_urls <- urls[
  which(str_detect(urls, pattern = "\\/en\\/publication\\/"))] 


all_tables <- corona_urls[
  str_detect(corona_urls, "analysis")] %>% 
  map(~{
  all_tables <- paste0(
    "https://www.gov.ie", .x) %>% 
    xml2::read_html() %>% 
    html_nodes("table") %>% 
    map(html_table, fill = TRUE)

  all_tables_rows <- all_tables %>% map_dbl(nrow)
  
  # Counties'table
  counties <- all_tables[[
    which(all_tables_rows %in% c(26, 24)) 
  ]] %>% 
    setNames(c("County", "Number of Cases", "% Total")) %>% 
    slice(-1)
  
  # Cluster'table
  # cluster_type <- all_tables[[
  #   which(all_tables_rows %in% 8) 
  #   ]] %>% 
  #   setNames(c("Type", "Number of Cases", "% Total")) 
  
  
  # Travel'table
  travel <- all_tables[[
    max(which(all_tables_rows == 4))
    ]] %>% 
    setNames(c("Travel", "Number of Cases", "% Total")) 
  
  # Age table
  age <- all_tables[[
    min(which(all_tables_rows %in% c(10, 11))) 
    ]] %>% 
    setNames(c("Age", "Number of Cases", "% Total"))  %>% 
    slice(-1)
  
  text <- paste0(
    "https://www.gov.ie", .x) %>% 
    xml2::read_html() %>% 
    html_nodes("h1") %>% 
    html_text() 
  published <- str_extract(
    text,
    pattern = "[0-9]{2}[ ][:alpha:]{5,9}[ ]2020")
  
  list(counties = counties, 
       travel = travel, 
       age = age, 
       published = published
  )
  })

saveRDS(all_tables, "data/scraped/all_tables.rds")

