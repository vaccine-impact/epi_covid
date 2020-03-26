# epi_covid.R
# run it from the source directory

# To assess the health benefits of vaccination versus the risks of sustaining or 
# suspending immunisation programmes in Africa during the COVID-19 pandemic. 

# load libraries
library (data.table)
library (ggplot2)
library (tictoc)

# remove all objects from workspace
rm (list = ls ())


# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Extract vaccine coverage estimates for 2018 from WHO for 54 African countries 
# ------------------------------------------------------------------------------
get_vaccine_coverage <- function () {
  
  # Source: WHO vaccine-preventable diseases: monitoring system 2019 global summary
  # Last update: 10 Dec 2019
  # http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_series.xls
  
  load ("data/data.vaccine_coverage.rda")
  vaccine_coverage <- data.vaccine_coverage
  
  # year of vaccination
  setnames (vaccine_coverage, old = "Year", new = "vac_year")  
  
  # extract data for 54 countries in Africa
  vaccine_coverage <- vaccine_coverage [Continent == "Africa" & vac_year == 2018 ]
  
  # extract data for 9 vaccines 
  # HepB3, Hib3, HPVfem, MCV1 & MCV2, MenA, PCV3, RotaC, RCV1, YFV
  vaccine_coverage <- vaccine_coverage [Vaccine %in% c("HepB3", "Hib3", "HPVfem", 
                                                       "MCV1", "MCV2", "MenA", "PCV3", 
                                                       "RotaC", "RCV1", "YFV")]
  # return vaccine coverage estimates 
  return (vaccine_coverage)
  
} # end of function -- get_vaccine_coverage
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# estimate potential deaths due to covid-19 by continuing vaccination programmes
estimate_covid_deaths <- function (vaccine_impact) {
  
  # TO DO for Kevin: Please implement this function
  vaccine_covid_impact <- vaccine_impact 
  
  # add a column "covid_deaths" to "vaccine_covid_impact" table 
  # for potential deaths due to covid-19 by continuing vaccination programmes
  
  
  
  
  
  
  return (vaccine_covid_impact)
  
} # end of function -- deaths_covid
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# main program 
# ------------------------------------------------------------------------------

# start time
print (Sys.time ())
tic ()

# move to base directory (run code from source directory)
source_wd <- getwd ()
setwd ("../")

# extract vaccine coverage estimates for 2018 from WHO for 54 African countries
vaccine_coverage <- get_vaccine_coverage ()

# add population estimates from UNWPP 2019
vaccine_coverage_population <- add_population (vaccine_coverage)

# add deaths averted by vaccination
vaccine_impact <- deaths_averted_vaccination (vaccine_coverage_population)

# ------------------------------------------------------------------------------
# TO DO for Kevin: Please implement this function
#
# estimate potential deaths due to covid-19 by continuing vaccination programmes
vaccine_covid_impact <- estimate_covid_deaths (vaccine_impact)
# ------------------------------------------------------------------------------

# estimate benefit risk ratio
benefit_risk_ratio <- benefit_risk (vaccine_covid_impact)

# generate map of benefit risk ratio
benefit_risk_ratio_map (benefit_risk_ratio)

# return to source directory
setwd (source_wd)

# end time
print (Sys.time ())
toc ()
# ------------------------------------------------------------------------------
