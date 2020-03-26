
library(tidyverse)
library(xml2)
library(rvest)


main_url <- "https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/" %>% 
  xml2::read_html()
urls <- html_attr(html_nodes(main_url, "a"), "href")  

corona_urls <- urls[
  which(str_detect(urls, pattern = "\\/en\\/publication\\/"))] 

corona_urls <- corona_urls[
  str_detect(corona_urls, "analysis")]

all_tables <- corona_urls %>% 
  map(~{
    print(.x)
    html_reading <- paste0(
      "https://www.gov.ie", .x) %>% 
      xml2::read_html() %>% 
      html_nodes("table")  
    
    all_tables <- html_reading %>% 
      map(html_table, fill = TRUE)
    
    all_tables_names <- html_reading %>% 
      map(html_table, fill = TRUE, header = TRUE) %>% 
      map_chr(~names(.x)[1])
    
    all_tables_rows <- all_tables %>% map_dbl(nrow)
    
    
    # Counties'table
    # This is more or less working fine for now (24/03/2020)
    counties <- all_tables[[
      which(all_tables_names == "Carlow" | 
              all_tables_rows %in% c(26, 25, 24) )
      ]] %>% 
      setNames(c("County", "Number of Cases", "% Total")) 
    
    
    if(counties$County[1] == "County") {
      
      counties <- counties %>% slice(-1)
      
    }
    # Cluster'table
    # cluster_type <- all_tables[[
    #   which(all_tables_rows %in% 8)
    #   ]] %>%
    #   setNames(c("Type", "Number of Cases", "% Total"))
    
    
    # Travel'table
    travel <- 
      tryCatch(
        all_tables[[
          max(which(str_detect(all_tables_names, "Travel|travel") | 
                      all_tables_rows == 4))
          ]] %>% 
          setNames(c("Travel", "Number of Cases", "% Total")), 
        error = function(e) e)
    
    if("travel" %in% class(travel)){
      totals <-  NULL
    }
    
    if(travel$Travel[1] == "Travel") {
      
      travel <- travel %>% slice(-1)
      
    }
    
    
    age_hospitalised <- 
      tryCatch(
        all_tables[[
          max(
            which(str_detect(all_tables_names, "<5") |
                    str_detect(all_tables_names, "Age|age"))) 
          ]] %>% 
          select(1:3) %>% 
          setNames(c("Hospitalised Age",
                     "Number of Cases", "% Total")),  
        error = function(e) e )
    
    if(str_detect(age_hospitalised$`Hospitalised Age`[1], "Age")){
      age_hospitalised <- age_hospitalised %>% slice(-1)
    }
    
    if("error" %in% class(age_hospitalised)){
      age_hospitalised <-  NULL
    }
    
    # Age table
    age <- all_tables[[
      min(which(str_detect(all_tables_names, "Age|age") | 
                  all_tables_rows %in% c(10, 11, 12))) 
      ]] %>% 
      select(1:3) %>% 
      setNames(c("Age", "Number of Cases", "% Total"))  %>% 
      slice(-1)
    
    eqs <- all.equal(age, age_hospitalised)
    if(length(eqs) == 1){
      age_hospitalised <- NULL
    }
    
    totals <- 
      tryCatch(
        all_tables[[
          which(str_detect(all_tables_names, "Total|total|Source"))
          ]] %>% 
          select(1:3) %>% 
          setNames(c("Totals", "Number of Cases", "% Total")), 
        error = function(e) e )
    
    
    if(str_detect(totals$Totals[1], "Source")){
      totals <- totals %>% slice(-1)
    }
    
    if("error" %in% class(totals)){
      totals <-  NULL
    }
    
    
    transmission <- 
      tryCatch(
        all_tables[[
          which(str_detect(all_tables_names, "Community|community"))
          ]] %>% 
          setNames(c("Transmission", "Cases")), 
        error = function(e) e )
    
    if("error" %in% class(transmission)){
      transmission <-  NULL
    }
    
    
    gender <- 
      tryCatch(
        all_tables[[
          which(str_detect(all_tables_names, "Gender|gender"))
          ]] %>% 
          select(1:3) %>% 
          setNames(c("Gender", "Number of Cases", "% Total"))  %>% 
          slice(-1), 
        error = function(e) e )
    
    if("error" %in% class(gender)){
      gender <-  NULL
    }
    
    
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
         age_hospitalised = age_hospitalised, 
         gender = gender, 
         transmission = transmission, 
         published = published, 
         totals = totals
    )
  })

saveRDS(all_tables, "all_tables_current.rds")
