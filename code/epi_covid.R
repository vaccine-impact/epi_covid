# epi_covid.R
# run it from the source directory

# To assess the health benefits of vaccination versus the risks of sustaining or 
# suspending immunisation programmes in Africa during the COVID-19 pandemic. 

# load libraries
library (data.table)
library (ggplot2)
library (rnaturalearth)
library (rnaturalearthdata)
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
  setnames (vaccine_coverage, 
            old = c ("Cname",   "Year"), 
            new = c ("Country", "vac_year") ) 
  
  # extract data for 54 countries in Africa
  vaccine_coverage <- vaccine_coverage [Continent == "Africa" & vac_year == 2018 ]
  
  # extract data for 9 vaccines 
  # HepB3, Hib3, HPVfem, MCV1, MenA, PCV3, RotaC, RCV1, YFV
  vaccine_coverage <- vaccine_coverage [Vaccine %in% c("HepB3", "Hib3", "HPVfem", 
                                                       "MCV1", "MCV2", "MenA", "PCV3", 
                                                       "RotaC", "RCV1", "YFV")]
  # return vaccine coverage estimates 
  return (vaccine_coverage)
  
} # end of function -- get_vaccine_coverage
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Add population estimates from UNWPP 2019 
# ------------------------------------------------------------------------------
add_population <- function (vaccine_coverage) {
  
  vaccine_coverage_pop <- vaccine_coverage
  
  # add population year
  vaccine_coverage_pop [, pop_year := 2020]
  
  # add age of vaccination
  vaccine_coverage_pop [, age := 0]
  vaccine_coverage_pop [Vaccine == "MCV2",   age := 1]
  vaccine_coverage_pop [Vaccine == "HPVfem", age := 12]
  vaccine_coverage_pop [Vaccine == "HPVfem" & Country == "South Africa", age := 9]
  vaccine_coverage_pop [Vaccine == "HPVfem" & Country == "Ethiopia",     age := 14]
  
  # add gender
  vaccine_coverage_pop [, gender := "both"]
  vaccine_coverage_pop [Vaccine == "HPVfem", gender := "female"]
  
  # load unwpp 2019 population estimates
  load ("data/data.population_both.rda")
  load ("data/data.population_female.rda")
  
  # add population
  vaccine_coverage_pop_both <- merge (
    vaccine_coverage_pop [gender == "both"], 
    data.population_both, 
    by.x = c ("ISO_code",     "pop_year", "age",      "gender"), 
    by.y = c ("country_code", "year",     "age_from", "gender"), 
    all.x = TRUE
    )
  
  vaccine_coverage_pop_female <- merge (
    vaccine_coverage_pop [gender == "female"], 
    data.population_female, 
    by.x = c ("ISO_code",     "pop_year", "age",      "gender"), 
    by.y = c ("country_code", "year",     "age_from", "gender"), 
    all.x = TRUE
  )
  
  vaccine_coverage_pop <- 
    rbindlist (list (vaccine_coverage_pop_both, vaccine_coverage_pop_female), 
               use.names = TRUE, 
               fill      = TRUE)
  
  setnames (vaccine_coverage_pop, old = "value", new = c ("population") ) 
  vaccine_coverage_pop [, c("country_code_numeric", "country", "age_to") := NULL] 
  
  # add vaccinated population
  vaccine_coverage_pop [, vac_population := (population * Percent_covrage/100)]
  
  # return vaccine coverage estimates 
  return (vaccine_coverage_pop)
  
} # end of function -- add_population
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# add deaths averted by vaccination
deaths_averted_vaccination <- function (vaccine_coverage_pop) {
  
  # Estimating the health impact of vaccination against 10 pathogens in 98 low 
  # and middle income countries from 2000 to 2030
  # https://www.medrxiv.org/content/10.1101/19004358v1
  # load vaccine impact estimates
  load ("data/data.vaccine_impact.rda")
  
  # add column for vaccine impact per 1000 fully vaccinated people
  vaccine_impact <- merge (
    vaccine_coverage_pop, 
    data.vaccine_impact, 
    by.x = c ("ISO_code", "Vaccine"), 
    by.y = c ("country",  "vaccine"), 
    all.x = TRUE
  )
  
  # drop redundant columns
  vaccine_impact [, c("country_name", "gavi73", "who_region") := NULL]
  
  # 9 countries with no vaccine impact data from 98 vimc countries
  # Botswana, Algeria, Gabon, Equatorial Guinea, Libya
  # Mauritius, Namibia, Seychelles, South Africa
  #
  # Congo (COG) -- no vaccine impact data for menA
  # Sao Tome and Principe -- no vaccine impact data for YFV
  #
  # for couuntries wth no vaccine impact values, set them to the mean 
  # vaccine impact in other African countries for corresponding vaccines
  
  # TO BE UPDATED
  
  
  
  
  # estimate deaths averted by each vaccine in each country
  vaccine_impact [, vac_deaths_averted := (vac_population * mid / 1000)]
  
  # set column order
  setcolorder (vaccine_impact, 
               c ("Continent", "ISO_code", "Country", "WHO_Region",   
                  "pop_year", "age", "gender", "population", 
                  "Vaccine", "vac_year", "Percent_covrage", "vac_population", 
                  "deaths_averted_1000FVP", "mid", "low", "high", 
                  "vac_deaths_averted") )

  return (vaccine_impact)
  
} # end of function -- deaths_averted_vaccination
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate potential deaths due to covid-19 by continuing vaccination programmes
estimate_covid_deaths <- function (vaccine_impact) {
  
  # TO DO for Kevin/Simon: Please implement this function
  vaccine_covid_impact <- vaccine_impact 
  
  # add a column "covid_deaths" to "vaccine_covid_impact" table 
  # for potential deaths due to covid-19 by continuing vaccination programmes
  
  vaccine_covid_impact [, covid_deaths := 0]
  
  # TO BE UPDATED
  
  
  
  
  
  return (vaccine_covid_impact)
  
} # end of function -- estimate_covid_deaths
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate benefit risk ratio
benefit_risk_ratio <- function (vaccine_covid_impact) {
  
  benefit_risk <- vaccine_covid_impact
  
  # estimate benefit ratio
  benefit_risk [, benefit_risk_ratio := vac_deaths_averted / covid_deaths]
  
  # TO BE UPDATED
  # estimate benefit risk ratios at the country level across all vaccines
  
  
  
  
  
  return (benefit_risk)
  
} # end of function -- benefit_risk_ratio
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# generate map of benefit risk ratio
benefit_risk_ratio_map <- function (benefit_risk) {
  
  # map tutorial
  # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  africa <- ne_countries (continent   = 'africa', 
                          scale       = "medium", 
                          returnclass = "sf")
  setDT (africa)
  setkey (africa, sov_a3)
  
  pdf ("figures/benefit_risk_ratio_maps.pdf")
  theme_set (theme_bw())
  
  vaccines <- unique (benefit_risk$Vaccine)
  
  # drop MCV2 for maps
  vaccines <- vaccines [vaccines != "MCV2"]
  
  # generate benefit-risk ratio maps for different vaccines
  for (vaccine in vaccines) {
    
    # combine tables to add geometry
    dt <- merge (x    = benefit_risk [Vaccine == vaccine], 
                 y    = africa, 
                 by.x = "ISO_code", 
                 by.y = "iso_a3", 
                 all  = T )
    
    # map of cases averted per 1000 vaccinated girls
    p <- ggplot (data = dt) +
      geom_sf (aes (fill = vac_deaths_averted, geometry = geometry)) + 
      scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey90") +
      labs (title    = "EPI benefits versus COVID-19 risks", 
            subtitle = vaccine, 
            fill     = "benefit-risk ratio") + 
      #theme (legend.title     = "benefit-risk ratio") + 
      theme (axis.text.x      = element_blank(), axis.ticks = element_blank()) + 
      theme (axis.text.y      = element_blank(), axis.ticks = element_blank()) + 
      theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      theme (plot.title       = element_text(size = 12)) +
      theme (plot.subtitle    = element_text(size = 11)) +
      theme (legend.title     = element_text(size = 10)) 
    
    print (p)
    
  }
  dev.off ()
  
} # end of function -- benefit_risk_ratio_map
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
vaccine_coverage_pop <- add_population (vaccine_coverage)

# add deaths averted by vaccination
vaccine_impact <- deaths_averted_vaccination (vaccine_coverage_pop)

# ------------------------------------------------------------------------------
# TO DO for Kevin/Simon: Please implement this function
#
# estimate potential deaths due to covid-19 by continuing vaccination programmes
vaccine_covid_impact <- estimate_covid_deaths (vaccine_impact)
# ------------------------------------------------------------------------------

# estimate benefit risk ratio
benefit_risk <- benefit_risk_ratio (vaccine_covid_impact)

# generate map of benefit risk ratio
benefit_risk_ratio_map (benefit_risk)

# return to source directory
setwd (source_wd)

# end time
print (Sys.time ())
toc ()
# ------------------------------------------------------------------------------
