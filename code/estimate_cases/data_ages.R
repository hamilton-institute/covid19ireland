cfr_age <- c(0.002299908, 0.009705793, 0.15, 0.314009662)*100

df_age <- data.frame(
  age_props  = c(2.38 + 5.73 + 17.34 + 16.94 + 15.69, 
                 15.27 + 10.98, 5.83 + 5.17, 4.62),
  cats  = c("0-44", "45-64", "65-84", "85+"))


# up to date version of the function
getUnderReportingCaseDeathPopulationAndTestingData <- function(
  files = "result_IRL.rds", prop_cases = prop_cases)
{
  
  owid_data <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
    dplyr::select(date, iso_code, new_cases) %>% 
    mutate(new_cases = round(prop_cases * new_cases))
  
  owid_data2 <- readr::read_csv("https://covid.ourworldindata.org/data/testing/covid-testing-all-observations.csv") %>% 
    dplyr::select(Date, `ISO code`, `Daily change in cumulative total`) %>% 
    setNames(c("date", "iso_code", "new_tests")) %>% 
    mutate(new_tests = round(prop_cases * new_tests))
  
  owid_data3 <- owid_data %>% 
    inner_join(owid_data2, by = c("date", "iso_code"))
  
  underReportingRawData <- readRDS(files) %>% 
    dplyr::mutate(countryCode = "IRL") %>% 
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  underReportingAndTestingData <- owid_data3 %>% 
    filter(iso_code == "IRL") %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::select(date, iso_code, new_cases, estimate, lower, upper, new_tests, country)
  return(underReportingAndTestingData)
  
}



getFigure1UnderReportingData <- function(files, prop_cases = 1)
{
  
  ecdc_case_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::mutate(date = lubridate::dmy(dateRep)) %>%
    mutate(new_cases = round(cases * prop_cases)) %>% 
    dplyr::select(date, new_cases, 
                  iso_code =countryterritoryCode)
  
  all_case_test_under_reporting_data_no_uk <- getUnderReportingCaseDeathPopulationAndTestingData(
    files, prop_cases
  ) %>%
    #filter(iso_code == "IRL") %>% 
    dplyr::mutate(
      new_tests = round(prop_cases * new_tests), 
      testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
    dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) %>%
    mutate(country = "Ireland") %>% 
    dplyr::select(date, iso_code, country, estimate, lower, upper, new_cases, new_tests, testing_effort)
  
  us_uk_test_data <-  getAdHocTestingData()
  
  #--- sorting out UK test data
  
  uk_case_test_data <- us_uk_test_data %>% 
    dplyr::rename(country = location) %>%
    dplyr::left_join(ecdc_case_data, by = c("date", "iso_code"))
  
  uk_test_under_reporting_data <- all_case_test_under_reporting_data_no_uk %>% 
    dplyr::filter(iso_code == "GBR") %>%
    dplyr::select(-new_tests) %>%
    dplyr::left_join(uk_case_test_data) %>%
    dplyr::mutate(new_tests = dplyr::case_when(date == "2020-05-04" ~ 59031,
                                               date != "2020-05-04" ~ new_tests)) %>%
    dplyr::select(date, iso_code, country, estimate, lower, upper, new_tests, new_cases)
  
  #--- putting it all together
  
  figure_1_data <- all_case_test_under_reporting_data_no_uk %>%
    dplyr::filter(iso_code != "GBR") %>%
    dplyr::full_join(uk_test_under_reporting_data) %>%
    dplyr::mutate(new_cases = dplyr::case_when(new_cases < 0 ~ 0,
                                               new_cases >= 0 ~ new_cases)) %>%
    dplyr::mutate(testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
    dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) 
  
  
  figure_1_under_reporting_data <- figure_1_data %>%
    dplyr::select(date, iso_code, country, estimate, lower, upper) %>%
    filter(country == "Ireland") %>% 
    mutate(file = files)
  
  return(figure_1_under_reporting_data)
  
}

# new version of the function, much more streamlined
getAdjustedCaseDataNational <- function(files, prop_cases)
{
  
  asymptomatic_mid <- 0.5
  asymptomatic_low <- 0.2
  asymptomatic_high <- 0.7
  
  ecdc_case_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
    dplyr::mutate(date = lubridate::dmy(dateRep)) %>%
    mutate(cases = round(cases * prop_cases)) %>% 
    dplyr::rename(new_cases = cases,
                  new_deaths = deaths, 
                  iso_code = countryterritoryCode,
                  country = countriesAndTerritories) %>%
    dplyr::filter(new_cases >= 0)

  
  underReportingRawData <- readRDS(files) %>%
    dplyr::mutate(iso_code = "IRL")
  
  
  underReportingAndCaseData <- ecdc_case_data %>% 
    filter(country == "Ireland") %>% 
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
    mutate(file = files)
  
  
  return(dataOut)
  
}
