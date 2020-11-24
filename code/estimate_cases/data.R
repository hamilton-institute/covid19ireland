library(tidyverse)
setwd("estimate_cases")
#--- simple function which gets all 2 and 3 letter country codes
#--- from a .csv file saved on my laptop, taken from online
getCountryCodes <- function()
{
  
  httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
  allDatRaw <- readr::read_csv(tf) %>%
    dplyr::rename(date = dateRep, 
                  country = countriesAndTerritories,
                  countryCode = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::dmy(date))
  
  countryCodesLookUp <- allDatRaw %>%
    dplyr::select(country, 
                  countryCode) %>% 
    unique()
  
  return(countryCodesLookUp)
  
}

# up to date version of the function
getUnderReportingCaseDeathPopulationAndTestingData <- function()
{
  
  owid_data <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
    dplyr::select(date, iso_code, new_cases)
  
  owid_data2 <- readr::read_csv("https://covid.ourworldindata.org/data/testing/covid-testing-all-observations.csv") %>% 
    dplyr::select(Date, `ISO code`, `Daily change in cumulative total`) %>% 
    setNames(c("date", "iso_code", "new_tests"))
  
  owid_data3 <- owid_data %>% 
    inner_join(owid_data2, by = c("date", "iso_code"))
  
  files <- "result_IRL.rds"
  
  underReportingRawData <- readRDS(files) %>% 
    dplyr::mutate(countryCode = files) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  underReportingAndTestingData <- owid_data3 %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::select(date, iso_code, new_cases, estimate, lower, upper, new_tests)
  
  return(underReportingAndTestingData)
  
}


#--- putting together testing data from various sources
getFigure1TestData <- function()
{
  
  ecdc_case_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::mutate(date = lubridate::dmy(dateRep)) %>%
    dplyr::select(date, new_cases = cases, iso_code = countryterritoryCode)
  
  owid_data <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
    dplyr::select(date, iso_code, new_cases)
  
  owid_data2 <- readr::read_csv("https://covid.ourworldindata.org/data/testing/covid-testing-all-observations.csv") %>% 
    dplyr::select(Date, `ISO code`, `Daily change in cumulative total`) %>% 
    setNames(c("date", "iso_code", "new_tests"))
  
  owid_data3 <- owid_data %>% 
    inner_join(owid_data2, by = c("date", "iso_code"))
  
  files <- "result_IRL.rds"
  
  underReportingRawData <- readRDS(files) %>% 
    dplyr::mutate(countryCode = files) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  all_case_test_under_reporting_data_no_uk <- owid_data3 %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::select(date, iso_code, new_cases, estimate, lower, upper, new_tests) %>%  
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en'))  %>% 
    dplyr::mutate(testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
    dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) %>%
    dplyr::select(date, iso_code, country, estimate, lower, upper, new_cases, new_tests, testing_effort)
  

  #--- putting it all together
  
  figure_1_data <- all_case_test_under_reporting_data_no_uk %>%
    dplyr::filter(iso_code != "GBR") %>%
  #  dplyr::full_join(uk_test_under_reporting_data) %>%
    dplyr::mutate(new_cases = dplyr::case_when(new_cases < 0 ~ 0,
                                               new_cases >= 0 ~ new_cases)) %>%
    dplyr::mutate(testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
    dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) 
  
  figure_1_testing_data <- figure_1_data %>%
    dplyr::select(date, iso_code, country, testing_effort) 
  #%>%tidyr::drop_na()
  
  
  return(figure_1_testing_data)
  
}

#--- the testing data for the UK, US and Brazil are less easily formatted and accessed
#--- we put together this function which scrapes the testing data from OWID and combines two
#--- different time-series into a single smoothed time-series
getAdHocTestingData <- function()
{
  
  testing_data_all <- readr::read_csv("https://covid.ourworldindata.org/data/testing/covid-testing-all-observations.csv")
  
  testing_data_UK_people_tested <- testing_data_all %>%
    dplyr::filter(Entity == "United Kingdom - people tested")
  
  testing_data_UK_tests_performed <- testing_data_all %>%
    dplyr::filter(Entity == "United Kingdom - tests performed")
  
  testing_data_UK_clean <- testing_data_UK_people_tested %>% dplyr::filter(Date <= "2020-05-03") %>%
    dplyr::bind_rows(testing_data_UK_tests_performed %>% dplyr::filter(Date > "2020-05-04")) %>%
    dplyr::mutate(location = "United Kingdom") %>%
    dplyr::rename(date = Date, 
                  iso_code = "ISO code", 
                  new_tests = "Daily change in cumulative total") %>%
    dplyr::select(date, location, iso_code, new_tests) %>%
    tidyr::drop_na()
  
  testing_data_US_units_unclear <- testing_data_all %>%
    dplyr::filter(Entity == "United States - units unclear")
  
  
  # testing_data_US_tests_performed <- testing_data_all %>%
  #   dplyr::filter(Entity == "United States - tests performed")
  
  
  testing_data_US_clean <- testing_data_US_units_unclear %>% 
    # dplyr::filter(Date <= "2020-05-19") %>%
    # dplyr::bind_rows(testing_data_US_tests_performed %>% dplyr::filter(Date > min(Date) + 7)) %>%
    dplyr::mutate(location = "United States") %>%
    dplyr::rename(date = Date, 
                  iso_code = "ISO code", 
                  new_tests = "Daily change in cumulative total") %>%
    dplyr::select(date, location, iso_code, new_tests) %>%
    tidyr::drop_na()
  
  
  both_together <- testing_data_UK_clean %>%
    dplyr::bind_rows(testing_data_US_clean)
  
  return(both_together)
  
}

getFigure1UnderReportingData <- function()
{
  
  ecdc_case_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::mutate(date = lubridate::dmy(dateRep)) %>%
    dplyr::select(date, new_cases = cases, iso_code =countryterritoryCode)
  
  all_case_test_under_reporting_data_no_uk <- getUnderReportingCaseDeathPopulationAndTestingData() %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Korea (the Republic of)" ~ "South Korea",
                                             country == "United States of America (the)" ~ "USA",
                                             country != "Korea (the Republic of)" | country != "United States of America (the)" ~ country)) %>% 
    dplyr::mutate(testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
    dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) %>%
    dplyr::select(date, iso_code, country, estimate, lower, upper, new_cases, new_tests, testing_effort)
  
  
  #--- sorting out UK test data
  
  #--- putting it all together
  
  figure_1_data <- all_case_test_under_reporting_data_no_uk %>%
    dplyr::filter(iso_code != "GBR") %>%
    dplyr::mutate(new_cases = dplyr::case_when(new_cases < 0 ~ 0,
                                               new_cases >= 0 ~ new_cases)) %>%
    dplyr::mutate(testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
    dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) 
  
  
  figure_1_under_reporting_data <- figure_1_data %>%
    dplyr::select(date, iso_code, country, estimate, lower, upper) %>%
    tidyr::drop_na()
  
}

# new version of the function, much more streamlined
getAdjustedCaseDataNational <- function()
{
  
  asymptomatic_mid <- 0.5
  asymptomatic_low <- 0.2
  asymptomatic_high <- 0.7
  
  ecdc_case_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::mutate(date = lubridate::dmy(dateRep)) %>%
    dplyr::rename(new_cases = cases,
                  new_deaths = deaths, 
                  iso_code = countryterritoryCode,
                  country = countriesAndTerritories) %>%
    dplyr::filter(new_cases >= 0)
  
   
   # underReportingPath <- "covid_underreporting/data/current_estimates_extracted_not_age_adjusted/"
   # files <- dir(path = underReportingPath,
   #              pattern = "*.rds")
   # 
   # underReportingRawData <- dplyr::tibble(countryCode = files) %>% 
   #   dplyr::mutate(file_contents = purrr::map(countryCode, 
   #                                            ~ readRDS(file.path(underReportingPath, .)))) %>% 
   #   tidyr::unnest(cols = c(file_contents)) %>%
   #   dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
   #   dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
   #   dplyr::group_by(countryCode) %>%
   #   dplyr::select(date, everything()) %>%
   #   dplyr::select(date, countryCode, everything()) %>%
   #   dplyr::group_by(countryCode) %>%
   #   dplyr::ungroup() %>%
   #   dplyr::rename(iso_code = countryCode)
   
  files <- "result_IRL.rds"

  underReportingRawData <- readRDS(files) %>%
    dplyr::mutate(countryCode = files) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)

  
  
  underReportingAndCaseData <- ecdc_case_data %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(country, date) %>%
    tidyr::drop_na() %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths, population_2019 = popData2019, estimate, lower, upper)
  
  
  dataOut <- underReportingAndCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_cases_smoothed             = zoo::rollmean(new_cases, k = 7, fill = NA),
                  new_cases_adjusted_mid         = new_cases/estimate,
                  new_cases_adjusted_low         = new_cases/upper,
                  new_cases_adjusted_high        = new_cases/lower,
                  new_cases_adjusted_smooth_mid  = zoo::rollmean(new_cases_adjusted_mid, k = 7, fill = NA),
                  new_cases_adjusted_smooth_low  = zoo::rollmean(new_cases_adjusted_low, k = 7, fill = NA),
                  new_cases_adjusted_smooth_high = zoo::rollmean(new_cases_adjusted_high, k = 7, fill = NA)) %>%
    dplyr::mutate(cumulative_incidence_mid  = cumsum(new_cases_adjusted_mid)/(population_2019*(1 - asymptomatic_mid)),
                  cumulative_incidence_low  = cumsum(new_cases_adjusted_low)/(population_2019*(1 - asymptomatic_low)),
                  cumulative_incidence_high = cumsum(new_cases_adjusted_high)/(population_2019*(1 - asymptomatic_high))) %>%
    dplyr::mutate(date_infection  = date - 10) %>%
    dplyr::mutate(cumulative_incidence_mid = dplyr::case_when(cumulative_incidence_mid >= 1 ~ 1,
                                                              cumulative_incidence_mid <= 0 ~ 0,
                                                              cumulative_incidence_mid > 0 & cumulative_incidence_mid < 1 ~ cumulative_incidence_mid)) %>%
    dplyr::mutate(cumulative_incidence_low = dplyr::case_when(cumulative_incidence_low > 1 ~ 1,
                                                              cumulative_incidence_low < 0 ~ 0,
                                                              cumulative_incidence_low > 0 & cumulative_incidence_low < 1 ~ cumulative_incidence_low)) %>%
    dplyr::mutate(cumulative_incidence_high = dplyr::case_when(cumulative_incidence_high > 1 ~ 1,
                                                               cumulative_incidence_high < 0 ~ 0,
                                                               cumulative_incidence_high > 0 & cumulative_incidence_high < 1 ~ cumulative_incidence_high)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "USA",
                                             country != "United States of America" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                             country != "United Kingdom" ~ country))
  
  
  return(dataOut)
  
}
