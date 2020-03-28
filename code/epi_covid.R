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
# add deaths averted by vaccination among "all" or "under5" age groups
deaths_averted_vaccination <- function (vaccine_coverage_pop, 
                                        age_group) {
  
  # Estimating the health impact of vaccination against 10 pathogens in 98 low 
  # and middle income countries from 2000 to 2030
  # https://www.medrxiv.org/content/10.1101/19004358v1
  
  # load vaccine impact estimates among "all" or "under5" age groups
  if (age_group == "all") { 
    
    load ("data/data.vaccine_impact.rda")
    vaccine_impact <- data.vaccine_impact
    
  } else if (age_group == "under5") { 
    
    load ("data/data.vaccine_impact_u5.rda")
    vaccine_impact <- data.vaccine_impact_u5
  }
  
  # add column for vaccine impact per 1000 fully vaccinated people
  vaccine_impact <- merge (
    vaccine_coverage_pop, 
    vaccine_impact, 
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
estimate_covid_deaths <- function (vaccine_impact, 
                                   suspension_period) {
  
  vaccine_covid_impact <- vaccine_impact 
  
  # timeline during which the covid deaths occur, that is 
  # if EPI is suspended for this period of time (unit in year), 
  # then what is the estimated number of covid deaths that are prevented 
  # by suspension of EPI
  vaccine_covid_impact [, suspension_period := suspension_period]
  
  # number of contacts with vaccinators 
  # 3x contacts covering PCV3, Hib3, HepB3, MenA, RotaC
  # 1x contacts covering MCV1, RCV1, YFV
  # 1x contacts covering MCV2
  # 2x contact based on coverage of HPV 
  
  # include here the independent risk based on coverage of each antigen alone, these will be a
  # aggregated outside this function to account for  antingens being grouped into contacts
  
  # infection risk from a single contact with vaccinator:
  
  # inf_risk1 <- 0.001   # upper bound using Mark's assumption
  # inf_risk1 <- 0.00001 # lower bound using Mark's assumption
  inf_risk1 <- 0.02     # assumes all vaccinators get infection over 6-month suspension period
  
  inf_risk2 <- 1 - (1 - inf_risk1)^2 # cum. risk for two visits
  inf_risk3 <- 1 - (1 - inf_risk1)^3 # cum. risk for three visits
  
  # Infection Fatality Risk from imperial work - Verity et al
  # ifr(age0-9)=0.0016%, ifr(age10-19)=0.007%, ifr(age20-29)=0.031%, ifr(age30-39)=0.084%
  # ifr(age40-49)=0.16%, ifr(age50-59)=0.60%, ifr(age60-69)=1.9%, ifr(age70-79)=4.3%, ifr(80+)=7.8%
  
  ifr_child <- 0.000016 # assume ifr for age 0-9
  ifr_mother <- 0.00084 # assume ifr for age 30-39 
  
  # fatality risk assume if either mother or child become infected then both become infected
  # ***need to think about other household contacts***
  
  fr1 <- 2 * inf_risk1 * (ifr_child + ifr_mother)
  fr2 <- 2 * inf_risk2 * (ifr_child + ifr_mother)
  fr3 <- 2 * inf_risk3 * (ifr_child + ifr_mother)
  
  # add a column "covid_deaths" to "vaccine_covid_impact" table 
  # for potential deaths due to covid-19 by continuing vaccination programmes
  
  # antigens with 3 contacts
  vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"), 
                        covid_deaths := vac_population * suspension_period * fr3]
  # antigens with 2 contacts
  vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"), 
                        covid_deaths := vac_population * suspension_period * fr2]
  # antigens with 1 contacts
  vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"), 
                        covid_deaths := vac_population * suspension_period * fr1] 
  
  
  return (vaccine_covid_impact)
  
} # end of function -- estimate_covid_deaths
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate benefit risk ratio
benefit_risk_ratio <- function (vaccine_covid_impact, 
                                suspension_period) {
  
  benefit_risk <- vaccine_covid_impact
  
  # estimate benefit ratio
  benefit_risk [, benefit_risk_ratio := 
                  (vac_deaths_averted * suspension_period) / covid_deaths]
  
  # TO BE UPDATED
  # estimate benefit risk ratios at the country level across all vaccines
  
  
  
  
  
  return (benefit_risk)
  
} # end of function -- benefit_risk_ratio
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# generate map of benefit risk ratio
benefit_risk_ratio_map <- function (benefit_risk, 
                                    suspension_period_string, 
                                    age_group) {
  
  # save benefit-risk results in tables folder
  fwrite (benefit_risk, file = paste0 ("tables/benefit_risk_results_", 
                                       suspension_period_string, 
                                       "_suspension_", 
                                       age_group, 
                                       ".csv") )
  
  # vaccination impact timeline -- lifetime or under 5-year-old children
  if (age_group == "all") {
    
    vaccine_impact_timeline <- "lifetime"
    
  } else if (age_group == "under5") {
    
    vaccine_impact_timeline <- "under 5-year-old children"
  } 
  
  
  # map tutorial
  # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  africa <- ne_countries (continent   = 'africa', 
                          scale       = "medium", 
                          returnclass = "sf")
  setDT (africa)
  setkey (africa, sov_a3)
  
  pdf (paste0 ("figures/benefit_risk_ratio_maps_", 
               suspension_period_string, "_suspension_",
               age_group, ".pdf"))
  
  vaccines <- unique (benefit_risk$Vaccine)
  
  # drop MCV2 for maps
  vaccines <- vaccines [vaccines != "MCV2"]
  
  theme_set (theme_bw())
  
  # generate benefit-risk ratio maps for different vaccines
  for (vaccine in vaccines) {
    
    # combine tables to add geometry
    dt <- merge (x    = benefit_risk [Vaccine == vaccine], 
                 y    = africa, 
                 by.x = "ISO_code", 
                 by.y = "iso_a3", 
                 all  = T )
    
    # map of benefit-risk ratio for different vaccines
    p <- ggplot (data = dt) +
      geom_sf (aes (fill = benefit_risk_ratio, geometry = geometry)) + 
      scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey90") +
      labs (title    = "EPI benefits versus COVID-19 risks", 
            subtitle = paste0 (vaccine, 
                               " / EPI suspension period: ", suspension_period_string, 
                               " / vaccine impact: ", vaccine_impact_timeline),  
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

# potential delay or suspension period of EPI due to COVID-19
# suspension_periods        <- c ( 3/12,       6/12,       12/12)  # unit in year
# suspension_period_strings <- c ("3 months", "6 months", "12 months") 

suspension_periods        <- c ( 6/12)  # unit in year
suspension_period_strings <- c ("6 months") 

for (period in 1:length (suspension_periods)) {
  
  # set suspension period
  suspension_period        <- suspension_periods        [period]
  suspension_period_string <- suspension_period_strings [period]
  
  # age group for vaccine impact -- "all" or "under5" age groups
  age_groups <- c("under5", "all")
  
  for (age_group in age_groups) {
    
    # extract vaccine coverage estimates for 2018 from WHO for 54 African countries
    vaccine_coverage <- get_vaccine_coverage ()
    
    # add population estimates from UNWPP 2019
    vaccine_coverage_pop <- add_population (vaccine_coverage)
    
    # add deaths averted by vaccination among "all" or "under5" age groups
    vaccine_impact <- deaths_averted_vaccination (vaccine_coverage_pop, 
                                                  age_group = age_group)
    
    # ------------------------------------------------------------------------------
    # TO DO for Kevin/Simon: Please implement this function
    #
    # estimate potential deaths due to covid-19 by continuing vaccination programmes
    vaccine_covid_impact <- estimate_covid_deaths (vaccine_impact, 
                                                   suspension_period)
    # ------------------------------------------------------------------------------
    
    # estimate benefit risk ratio
    benefit_risk <- benefit_risk_ratio (vaccine_covid_impact, 
                                        suspension_period)
    
    # generate map of benefit risk ratio
    benefit_risk_ratio_map (benefit_risk, 
                            suspension_period_string, 
                            age_group = age_group)
    
  }
  
}

# return to source directory
setwd (source_wd)

# end time
print (Sys.time ())
toc ()
# ------------------------------------------------------------------------------
