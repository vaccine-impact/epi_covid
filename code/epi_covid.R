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
library (tidyverse)

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
  
  # adjusted impact of MCV1 and MCV2 based on measles impact
  for (country_code in unique (vaccine_impact [, ISO_code]) ) {
    
    vaccine_impact [ISO_code == country_code & Vaccine == "MCV2", 
                    mid := (4/97) * vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", mid] ]
    
    vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", 
                    mid := (93/97) * vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", mid] ]
  }
  
  # ----------------------------------------------------------------------------
  # 9 countries with no vaccine impact data from 98 vimc countries
  # Botswana, Algeria, Gabon, Equatorial Guinea, Libya
  # Mauritius, Namibia, Seychelles, South Africa
  #
  # Congo (COG) -- no vaccine impact data for menA
  # Sao Tome and Principe -- no vaccine impact data for YFV
  #
  # for countries wth no vaccine impact values, set them to the mean 
  # vaccine impact in other African countries for corresponding vaccines
  
  for (vaccine  in unique (vaccine_impact [, Vaccine]) ) {
    vaccine_impact [is.na(mid) & Vaccine == vaccine, 
                    mid := mean (vaccine_impact [!is.na (mid) & Vaccine == vaccine, mid] ) ]
  }
  # ----------------------------------------------------------------------------
 
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
# Add data on household size
# ------------------------------------------------------------------------------
add_hh_size_data <- function (vaccine_impact) {

  # load UN household data
  load ("data/data.household_size.rda")
  
  #select household data for most recent year from DHS or IPUMS
  data.household_size <- data.household_size %>%
    dplyr::filter_at(vars(`Data source category`), all_vars(. %in% c("DHS","IPUMS"))) %>%
    dplyr::group_by(iso3_code) %>%
    dplyr::top_n(1,`Reference date (dd/mm/yyyy)`) %>%
    dplyr::mutate_at(vars(6,18,25,30),as.numeric) %>%
    dplyr::select(iso3_code = 1,
                  hh_size = 6,
                  under_20_in_hh_at_least_one_under_20 = 30,
                  percent_hh_at_least_one_under_20 = 18,
                  percent_hh_under_20_and_over_60 = 25)
  
  # merge data into vaccine_impact
  vaccine_impact <- merge (
    vaccine_impact,
    data.household_size, 
    by.x = c ("ISO_code"), 
    by.y = c ("iso3_code"), 
    all.x = TRUE
  )
  
  # [1] "iso3_code"                            "hh_size" 4.95                            
  # [3] "under_20_in_hh_at_least_one_under_20" 3.34 "percent_hh_at_least_one_under_20" 79.73    
  # [5] "percent_hh_under_20_and_over_60 17.98
  # 
  # ----------------------------------------------------------------------------
  # for countries with no household size data:
  # "CPV" "DJI" "DZA" "ERI" "GNB" "GNQ" "LBY" "MRT" "MUS" "SOM" "SYC" "TUN"
  # set them to the mean value of other African countries

  household <- vaccine_impact [!is.na (hh_size), 
                               lapply (.SD, mean),
                               .SDcols = c("hh_size", 
                                           "under_20_in_hh_at_least_one_under_20",
                                           "percent_hh_at_least_one_under_20",
                                           "percent_hh_under_20_and_over_60"),
                               by = "ISO_code"]
  
  vaccine_impact [is.na (hh_size), 
                  ':=' (hh_size                              = mean (household [, hh_size]), 
                        under_20_in_hh_at_least_one_under_20 = mean (household [, under_20_in_hh_at_least_one_under_20]), 
                        percent_hh_at_least_one_under_20     = mean (household [, percent_hh_at_least_one_under_20]), 
                        percent_hh_under_20_and_over_60      = mean (household [, percent_hh_under_20_and_over_60])
                  )
                  ]
  # ----------------------------------------------------------------------------
  
  return (vaccine_impact)

} # end of function -- add_hh_size_data
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
  
  # infection risk from contacts due to vaccination visits
  
  baseline_pop_infected = 0.5  # prop. of the pop. who become infected without vaccination visits
  
  prev_community  <- 0.02  # prev. community infectives based on 50% pop. infected over 6 months
  prev_vaccinator <- 0.04  # prev. vaccinator infectives assuming all infected over 6 months
  
  p_transmit_community  <- 0.1   # prob. that a community contact transmits
  p_transmit_vaccinator <- 0.05  # assume halved due to better infection control
  
  vac_contacts_per_visit       <- 1  # number vaccinators contacts due to vaccine clinic visit
  community_contacts_per_visit <- 2  # number extra community contacts due to vaccine clinic visit
  
  # risk of household infection for one clinic visit - note additional factor of 2 because both
  # child and caregiver could become infected
  
  # for 1 vaccine clinic visit
  hh_inf_risk1 <-  1 - (
    (1 - prev_community * p_transmit_community)^(community_contacts_per_visit * 2) *
      (1 - prev_vaccinator * p_transmit_vaccinator)^(vac_contacts_per_visit * 2)
  )
  
  # for 2 vaccine clinic visits
  hh_inf_risk2 <-  1 - (
    (1 - prev_community * p_transmit_community)^(community_contacts_per_visit * 2 * 2) *
      (1 - prev_vaccinator * p_transmit_vaccinator)^(vac_contacts_per_visit * 2 * 2)
  )
  
  # for 3 vaccine clinic visits
  hh_inf_risk3 <-  1 - (
    (1 - prev_community * p_transmit_community)^(community_contacts_per_visit * 2 * 3) *
      (1 - prev_vaccinator * p_transmit_vaccinator)^(vac_contacts_per_visit * 2 * 3)
  )
  
  # risk above baseline due to vaccine visits
  hh_inf_risk1 <- (1 - baseline_pop_infected) * hh_inf_risk1
  hh_inf_risk2 <- (1 - baseline_pop_infected) * hh_inf_risk2
  hh_inf_risk3 <- (1 - baseline_pop_infected) * hh_inf_risk3
  
  # age-specific infection fatality risk from Verity et al.
  # ifr(age 0-9) = 0.0016% 
  # ifr(age 10-19) = 0.007% 
  # ifr(age 20-29) = 0.031%
  # ifr(age 30-39) = 0.084%
  # ifr(age 40-49) = 0.16% 
  # ifr(age 50-59) = 0.60% 
  # ifr(age 60-69) = 1.9% 
  # ifr(age 70-79) = 4.3%
  # ifr(age 80+) = 7.8%
  
  # assume ifr for household members as follows
  
  ifr_child        <- 0.000016  # assume ifr for age 0-9
  ifr_parents      <- 0.00031   # assume ifr for age 20-29 
  ifr_grandparents <- 0.019     # assume ifr for age 60-69
  
  # add a column for estimated covid-19 deaths due to continuing vaccination programmes
  # if infection is imported into household then assume the following become infected:
  # (1) the child who attends the clinic
  # (2) other children in household based on the average household members <20 years in a 
  # household with at least one child - divded by 2 to account for birth order
  # (3) 2 adults in every household (caregiver who attends clinic plus one other)
  # (4) 2 adults over 60 adjusted for the proportion of households with members <20 years that
  # also have a household member >60 years

  vaccine_covid_impact [,child_covid_deaths := 0]
  vaccine_covid_impact [,sibling_covid_deaths := 0]
  vaccine_covid_impact [,parent_covid_deaths := 0]
  vaccine_covid_impact [,grandparent_covid_deaths := 0]
  
  
  # antigens with 3 contacts
  vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
                        child_covid_deaths := vac_population * suspension_period * hh_inf_risk3 * ifr_child]
  vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
                        sibling_covid_deaths := vac_population * suspension_period * hh_inf_risk3 *
                          (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
  vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
                        parent_covid_deaths := vac_population * suspension_period * hh_inf_risk3 * 2 * ifr_parents]
  vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
                        grandparent_covid_deaths := vac_population * suspension_period * hh_inf_risk3 * 
                          (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * ifr_grandparents)]
  vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
                        covid_deaths := child_covid_deaths + sibling_covid_deaths + parent_covid_deaths +
                          grandparent_covid_deaths]
  
  # antigens with 2 contacts
  vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"), 
                        child_covid_deaths := vac_population * suspension_period * hh_inf_risk2 * ifr_child]
  vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
                        sibling_covid_deaths := vac_population * suspension_period * hh_inf_risk2 *
                          (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
  vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
                        parent_covid_deaths := vac_population * suspension_period * hh_inf_risk2 * 2 * ifr_parents]
  vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
                        grandparent_covid_deaths := vac_population * suspension_period * hh_inf_risk2 * 
                          (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * ifr_grandparents)]
  vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
                        covid_deaths := child_covid_deaths + sibling_covid_deaths + parent_covid_deaths +
                          grandparent_covid_deaths]   
  
  # antigens with 1 contacts
  vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"), 
                        child_covid_deaths := vac_population * suspension_period * hh_inf_risk1 * ifr_child]
  vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
                        sibling_covid_deaths := vac_population * suspension_period * hh_inf_risk1 *
                          (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
  vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
                        parent_covid_deaths := vac_population * suspension_period * hh_inf_risk1 * 1 * ifr_parents]
  vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
                        grandparent_covid_deaths := vac_population * suspension_period * hh_inf_risk1 * 
                          (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * ifr_grandparents)]
  vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
                        covid_deaths := child_covid_deaths + sibling_covid_deaths + parent_covid_deaths +
                          grandparent_covid_deaths] 
  
  return (vaccine_covid_impact)
  
} # end of function -- estimate_covid_deaths
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate benefit risk ratio
benefit_risk_ratio <- function (vaccine_covid_impact, 
                                suspension_period) {
  
  benefit_risk <- vaccine_covid_impact
  
  # compute benefit-risk ratio across combined vaccines in same visits
  
  # clinical visits for vaccination among children aged 0-11 months and their adult carers = 4 
  #   (3 visits for DTP3-HepB-Hib + PCV3 + RotaC) + 
  #   (1 visit for MCV1 + RCV1 + MenA + YF)
  # clinical visits for vaccination among children aged 12-23 months and their adult carers = 1 
  #   (1 visit for MCV2) -- single vaccine estimates already computed
  
  # compute benefit-risk ratio for vaccines in the same visit
  benefit_risk_3visits_age0 <- benefit_risk [Vaccine %in% c("HepB3", "Hib3", "PCV3", "RotaC")]
  benefit_risk_1visits_age0 <- benefit_risk [Vaccine %in% c("MCV1", "RCV1", "MenA", "YFV")]
  
  # set vaccine name list
  benefit_risk_3visits_age0 [, Vaccine := "HepB3, Hib3, PCV3, RotaC"]
  benefit_risk_1visits_age0 [, Vaccine := "MCV1, RCV1, MenA, YFV"]
  
  # add deaths averted by vaccination in the vaccine list
  benefit_risk_3visits_age0 [, vac_deaths_averted := sum (vac_deaths_averted), by = "ISO_code"]
  benefit_risk_1visits_age0 [, vac_deaths_averted := sum (vac_deaths_averted), by = "ISO_code"]
  
  # set covid deaths to the maximum estimate in the vaccine list
  benefit_risk_3visits_age0 [, covid_deaths := max (covid_deaths), by = "ISO_code"]
  benefit_risk_1visits_age0 [, covid_deaths := max (covid_deaths), by = "ISO_code"]
  
  # extract 1 row per country for combined vaccine impact estimates
  benefit_risk_3visits_age0 [, min_val := mid < max (mid), by = "ISO_code"]
  benefit_risk_3visits_age0 [min_val == FALSE]
  benefit_risk_3visits_age0 [, min_val := NULL]
  
  benefit_risk_1visits_age0 [, min_val := mid < max (mid), by = "ISO_code"]
  benefit_risk_1visits_age0 [min_val == FALSE]
  benefit_risk_1visits_age0 [, min_val := NULL]
  
  # add combined vaccine impact estimates to benefit risk table
  benefit_risk <- 
    rbindlist (list (benefit_risk, 
                     benefit_risk_3visits_age0, 
                     benefit_risk_1visits_age0), 
               use.names = TRUE, 
               fill      = TRUE)
  
  # compute benefit-risk ratio across EPI vaccines
  benefit_risk_EPI <- benefit_risk [Vaccine == "HepB3, Hib3, PCV3" |
                                      Vaccine == "MCV1, RCV1, MenA, YFV" |
                                      Vaccine == "MCV2"]
  
  # estimate benefit ratio
  benefit_risk [, benefit_risk_ratio := 
                  (vac_deaths_averted * suspension_period) / covid_deaths]
  
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
  # vaccines <- vaccines [vaccines != "MCV2"]
  
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
    
    # add UN household size data from DHS / IPUMS
    vaccine_impact <- add_hh_size_data (vaccine_impact)
    
    # estimate potential deaths due to covid-19 by continuing vaccination programmes
    vaccine_covid_impact <- estimate_covid_deaths (vaccine_impact, 
                                                   suspension_period)
    
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
