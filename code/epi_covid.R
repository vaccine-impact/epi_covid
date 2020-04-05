# epi_covid.R
# run it from the source directory

# To assess the health benefits of vaccination versus the risks of sustaining or 
# suspending immunisation programmes in Africa during the COVID-19 pandemic. 

# load libraries
library (countrycode)
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
get_vaccine_coverage <- function (age_group) {
  
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
  
  # ----------------------------------------------------------------------------
  # extract data for DTP3 vaccine
  vaccine_coverage_dtp <- vaccine_coverage [Vaccine %in% c("DTP3")]
  
  vaccine_coverage_diptheria <- copy (vaccine_coverage_dtp)
  vaccine_coverage_tetanus   <- copy (vaccine_coverage_dtp)
  vaccine_coverage_pertussis <- copy (vaccine_coverage_dtp)
  
  vaccine_coverage_diptheria [, Vaccine := "Diptheria (DTP3)"]
  vaccine_coverage_tetanus   [, Vaccine := "Tetanus (DTP3)"  ]
  vaccine_coverage_pertussis [, Vaccine := "Pertussis (DTP3)"]
  
  # ----------------------------------------------------------------------------
  
  # extract data for 9 vaccines 
  # HepB3, Hib3, HPVfem, MCV1, MenA, PCV3, RotaC, RCV1, YFV
  vaccine_coverage <- vaccine_coverage [Vaccine %in% c("HepB3", "Hib3", "HPVfem", 
                                                       "MCV1", "MCV2", "MenA", "PCV3", 
                                                       "RotaC", "RCV1", "YFV")]
  
  # add vaccine coverage data for DTP3 vaccine
  vaccine_coverage <- rbindlist (list (vaccine_coverage, 
                                       vaccine_coverage_diptheria, 
                                       vaccine_coverage_tetanus, 
                                       vaccine_coverage_pertussis), 
                                 use.names = T)
  
  # drop HPVfem for under-5 children
  if (age_group == "under5") {
    
    vaccine_coverage <- vaccine_coverage [Vaccine != "HPVfem"]
  }
  
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
                                        age_group, 
                                        psa) {
  
  # Estimating the health impact of vaccination against 10 pathogens in 98 low 
  # and middle income countries from 2000 to 2030
  # https://www.medrxiv.org/content/10.1101/19004358v1
  
  # load DTP3 vaccine impact estimates
  load ("data/data.vaccine_impact_dtp.rda")
  
  # load vaccine impact estimates among "all" or "under5" age groups
  if (age_group == "all") { 
    
    load ("data/data.vaccine_impact.rda")
    vaccine_impact <- rbindlist (list (data.vaccine_impact_dtp, data.vaccine_impact), 
                                 use.names = TRUE)
    
  } else if (age_group == "under5") { 
    
    load ("data/data.vaccine_impact_u5.rda")
    vaccine_impact <- rbindlist (list (data.vaccine_impact_dtp, data.vaccine_impact_u5), 
                                 use.names = TRUE)
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
  
  for (vaccine in unique (vaccine_impact [, Vaccine]) ) {
    
    # estimate mid value
    vaccine_impact [is.na(mid) & Vaccine == vaccine, 
                    mid := mean (vaccine_impact [!is.na (mid) & Vaccine == vaccine, mid] ) ]
    
    # proxy estimate of lower 95% credible interval of vaccine impact
    vaccine_impact [is.na(low) & Vaccine == vaccine, 
                    low := mean (vaccine_impact [!is.na (low) & Vaccine == vaccine, low/mid] ) * mid ]
    
    vaccine_impact [is.na(high) & Vaccine == vaccine, 
                    high := mean (vaccine_impact [!is.na (high) & Vaccine == vaccine, high/mid] ) * mid ]
    
    # proxy estimate of upper 95% credible interval of vaccine impact
    
  }
  # ----------------------------------------------------------------------------
 
  # adjusted impact of MCV1 and MCV2 based on measles impact
  for (value in c("mid", "low", "high")) {
    
    for (country_code in unique (vaccine_impact [, ISO_code]) ) {
      
      # vaccine_impact [ISO_code == country_code & Vaccine == "MCV2",
      #                 mid := (4/97) * vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", mid] ]

      vaccine_impact [ISO_code == country_code & Vaccine == "MCV2",
                      as.character (as.name (value)) := (4/97) * vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", eval (as.name (value))] ]

      # vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", 
      #                 mid := (93/97) * vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", mid] ]
      
      vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", 
                      as.character (as.name (value)) := (93/97) * vaccine_impact [ISO_code == country_code & Vaccine == "MCV1", eval (as.name (value))] ]
    }
  }

  # estimate deaths averted by each vaccine in each country
  vaccine_impact [, vac_deaths_averted := (vac_population * mid / 1000)]
  
  # add run id
  vaccine_impact [, run_id := 0]
  
  # set column order
  setcolorder (vaccine_impact, 
               c ("run_id", 
                  "Continent", "ISO_code", "Country", "WHO_Region",   
                  "pop_year", "age", "gender", "population", 
                  "Vaccine", "vac_year", "Percent_covrage", "vac_population", 
                  "deaths_averted_1000FVP", "mid", "low", "high", 
                  "vac_deaths_averted") )
  
  # ----------------------------------------------------------------------------
  # probabilistic sensitivity analysis
  
  # add a small value to zero values to avoid log(0) = -Inf
  vaccine_impact [low == 0, low := 1e-6]

  # estimate standard error in log scale for deaths averted per 1000 vaccinated individuals
  vaccine_impact [, log_ci_se := (log (high) - log (low)) / 3.92]
  
  # initialise psa data table
  vaccine_impact_psa <- vaccine_impact [0, ]
  
  # copy vaccine impact for PSA
  for (i in 1:psa) {
    
    # copy data table
    dt <- copy (vaccine_impact)
    
    # set run id
    dt [, run_id := i]
    
    # add combined vaccine impact estimates to benefit risk table
    vaccine_impact_psa <- rbindlist (list (vaccine_impact_psa, dt), 
                                     use.names = TRUE)
  }
  
  # update estimates for deaths averted by vaccination
  vaccine_impact_psa [, vac_deaths_averted := 
                        vac_population * (rlnorm (n       = psa, 
                                                  meanlog = log (mid), 
                                                  sdlog   = log_ci_se)) / 1000, 
                      by = .(ISO_code, Vaccine) ]

  # ----------------------------------------------------------------------------
  
  return (vaccine_impact_psa)
  
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
#
# estimate_covid_deaths_old -- function NOT USED -- old version
# estimate_covid_deaths -- new updated version (see below after the old version)
#
# ------------------------------------------------------------------------------
# # estimate potential deaths due to covid-19 by continuing vaccination programmes
# estimate_covid_deaths_old <- function (vaccine_impact, 
#                                    suspension_period, 
#                                    psa) {
#   
#   vaccine_covid_impact <- vaccine_impact 
#   
#   # timeline during which the covid deaths occur, that is 
#   # if EPI is suspended for this period of time (unit in year), 
#   # then what is the estimated number of covid deaths that are prevented 
#   # by suspension of EPI
#   vaccine_covid_impact [, suspension_period := suspension_period]
#   
#   # infection risk from contacts due to vaccination visits
#   
#   # using point estiamtes for deterministic values
#   
#   big_t <- suspension_period * 12 * 30   # duration of period at risk of SARS-CoV-2 ** in days **
#   r0    <- 2.5                           # basic reproduction number
#   theta <- 1-(1/r0)                      # proportion of pop. infected at big_t assuming no "overshooting"
#   psi   <- 7                             # duration of infectiousness
#   n     <- 2                             # number of potentially infectious contacts on visit to clinic
#   iota1 <- 2                             # RR vacinator being infected + infectious vs community member
#   iota2 <- 0.5                           # RR of infectious vaccinator transmitting vs community member
#   p0    <- (theta * psi)/big_t           # prevalence of infectiousness amongst community members on given day
#   pv    <- p0 * iota1                    # prevalence of infectiousness amongst vaccinators on given day
#   big_n <- 5                             # av. no. of community member transmission relevant contacts per day
#   t0    <- r0/(big_n * psi)              # prob. transmission given potentially infectious community contact
#   tv    <- t0/iota2                      # prob. transmission given potentially infectious vaccinator contact
#   
#   pe_v1_mid <- (1-((1-pv)^(2*1*tv) * (1-p0)^(2*1*n*t0))) * (1-theta) # excess household risk from 1 visit
#   pe_v2_mid <- (1-((1-pv)^(2*2*tv) * (1-p0)^(2*2*n*t0))) * (1-theta) # excess household risk from 2 visits
#   pe_v3_mid <- (1-((1-pv)^(2*3*tv) * (1-p0)^(2*3*n*t0))) * (1-theta) # excess household risk from 3 visits
#   
#   # using PSA
#   
#   big_t <- runif  (psa, big_t-30, big_t+30) # uniform on -/+ 30 days from period that is passed in 
#   r0    <- rgamma (psa, shape = 25, scale = (2.5/25))                   
#   theta <- 1-(1/r0)            
#   psi   <- rgamma (psa, shape = 14, scale = (7/14))                    
#   n     <- runif  (psa, 1, 10)                       
#   iota1 <- runif  (psa, 1, 4)                   
#   iota2 <- runif  (psa, 0.25, 1)               
#   p0    <- (theta * psi)/big_t    
#   pv    <- p0 * iota1             
#   big_n <- runif  (psa, 2, 10)                  
#   t0    <- r0/(big_n * psi)       
#   tv    <- t0/iota2               
#   
#   pe_v1 <- (1-((1-pv)^(2*1*tv) * (1-p0)^(2*1*n*t0))) * (1-theta) # excess household risk from 1 visit
#   pe_v2 <- (1-((1-pv)^(2*2*tv) * (1-p0)^(2*2*n*t0))) * (1-theta) # excess household risk from 2 visits
#   pe_v3 <- (1-((1-pv)^(2*3*tv) * (1-p0)^(2*3*n*t0))) * (1-theta) # excess household risk from 3 visits
#   
#   # upper bound
#   pe_v1_high <- quantile (pe_v1, 0.975)
#   pe_v2_high <- quantile (pe_v2, 0.975) 
#   pe_v3_high <- quantile (pe_v3, 0.975) 
#   
#   # lower bound
#   pe_v1_low <- quantile (pe_v1, 0.025) 
#   pe_v2_low <- quantile (pe_v2, 0.025) 
#   pe_v3_low <- quantile (pe_v3, 0.025)
#   
#   # age-specific infection fatality risk from Verity et al.
#   # ifr(age 0-9) = 0.0016% 
#   # ifr(age 10-19) = 0.007% 
#   # ifr(age 20-29) = 0.031%
#   # ifr(age 30-39) = 0.084%
#   # ifr(age 40-49) = 0.16% 
#   # ifr(age 50-59) = 0.60% 
#   # ifr(age 60-69) = 1.9% 
#   # ifr(age 70-79) = 4.3%
#   # ifr(age 80+) = 7.8%
#   
#   # assume ifr for household members as follows
#   
#   ifr_child        <- 0.000016  # assume ifr for age 0-9
#   ifr_parents      <- 0.00031   # assume ifr for age 20-29 
#   ifr_grandparents <- 0.019     # assume ifr for age 60-69
#   
#   # add a column for estimated covid-19 deaths due to continuing vaccination programmes
#   # if infection is imported into household then assume the following become infected:
#   # (1) the child who attends the clinic
#   # (2) other children in household based on the average household members <20 years in a 
#   # household with at least one child - divded by 2 to account for birth order
#   # (3) 2 adults in every household (caregiver who attends clinic plus one other)
#   # (4) 2 adults over 60 adjusted for the proportion of households with members <20 years that
#   # also have a household member >60 years
#   
#   vaccine_covid_impact [,child_covid_deaths      := 0]
#   vaccine_covid_impact [,child_covid_deaths_low  := 0]
#   vaccine_covid_impact [,child_covid_deaths_high := 0]
#   
#   vaccine_covid_impact [,sibling_covid_deaths      := 0]
#   vaccine_covid_impact [,sibling_covid_deaths_low  := 0]
#   vaccine_covid_impact [,sibling_covid_deaths_high := 0]
#   
#   vaccine_covid_impact [,parent_covid_deaths      := 0]
#   vaccine_covid_impact [,parent_covid_deaths_low  := 0]
#   vaccine_covid_impact [,parent_covid_deaths_high := 0]
#   
#   vaccine_covid_impact [,grandparent_covid_deaths      := 0]
#   vaccine_covid_impact [,grandparent_covid_deaths_low  := 0]
#   vaccine_covid_impact [,grandparent_covid_deaths_high := 0]
#   
#   vaccine_covid_impact [,covid_deaths      := 0]
#   vaccine_covid_impact [,covid_deaths_low  := 0]
#   vaccine_covid_impact [,covid_deaths_high := 0]
#   
#   
#   # antigens with 3 contacts 
#   
#   # mid
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         child_covid_deaths := 
#                           vac_population * suspension_period * pe_v3_mid * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         sibling_covid_deaths := 
#                           vac_population * suspension_period * pe_v3_mid *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         parent_covid_deaths := 
#                           vac_population * suspension_period * pe_v3_mid * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         grandparent_covid_deaths := 
#                           vac_population * suspension_period * pe_v3_mid * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         covid_deaths := child_covid_deaths + sibling_covid_deaths + 
#                           parent_covid_deaths + grandparent_covid_deaths]
#   
#   # low
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         child_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v3_low * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         sibling_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v3_low *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         parent_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v3_low * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         grandparent_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v3_low * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         covid_deaths_low := child_covid_deaths_low + sibling_covid_deaths_low + 
#                           parent_covid_deaths_low + grandparent_covid_deaths_low]
#   
#   # high
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         child_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v3_high * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         sibling_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v3_high *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         parent_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v3_high * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         grandparent_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v3_high * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
#                         covid_deaths_high := child_covid_deaths_high + sibling_covid_deaths_high + 
#                           parent_covid_deaths_high + grandparent_covid_deaths_high]
#   
#   # antigens with 2 contacts
#   # mid
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         child_covid_deaths := 
#                           vac_population * suspension_period * pe_v2_mid * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         sibling_covid_deaths := 
#                           vac_population * suspension_period * pe_v2_mid *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         parent_covid_deaths := 
#                           vac_population * suspension_period * pe_v2_mid * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         grandparent_covid_deaths := 
#                           vac_population * suspension_period * pe_v2_mid * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         covid_deaths := child_covid_deaths + sibling_covid_deaths + 
#                           parent_covid_deaths + grandparent_covid_deaths]
#   
#   # low
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         child_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v2_low * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         sibling_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v2_low *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         parent_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v2_low * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         grandparent_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v2_low * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         covid_deaths_low := child_covid_deaths_low + sibling_covid_deaths_low + 
#                           parent_covid_deaths_low + grandparent_covid_deaths_low]
#   
#   # high
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         child_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v2_high * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         sibling_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v2_high *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         parent_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v2_high * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         grandparent_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v2_high * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
#                         covid_deaths_high := child_covid_deaths_high + sibling_covid_deaths_high + 
#                           parent_covid_deaths_high + grandparent_covid_deaths_high]
#   
#   # antigens with 1 contacts
#   # mid
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         child_covid_deaths := 
#                           vac_population * suspension_period * pe_v1_mid * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         sibling_covid_deaths := 
#                           vac_population * suspension_period * pe_v1_mid *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         parent_covid_deaths := 
#                           vac_population * suspension_period * pe_v1_mid * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         grandparent_covid_deaths := 
#                           vac_population * suspension_period * pe_v1_mid * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         covid_deaths := child_covid_deaths + sibling_covid_deaths + 
#                           parent_covid_deaths + grandparent_covid_deaths]
#   
#   # low
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         child_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v1_low * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         sibling_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v1_low *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         parent_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v1_low * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         grandparent_covid_deaths_low := 
#                           vac_population * suspension_period * pe_v1_low * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         covid_deaths_low := child_covid_deaths_low + sibling_covid_deaths_low + 
#                           parent_covid_deaths_low + grandparent_covid_deaths_low]
#   
#   # high
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         child_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v1_high * ifr_child]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         sibling_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v1_high *
#                           (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         parent_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v1_high * 2 * ifr_parents]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         grandparent_covid_deaths_high := 
#                           vac_population * suspension_period * pe_v1_high * 
#                           (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
#                              ifr_grandparents)]
#   vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
#                         covid_deaths_high := child_covid_deaths_high + sibling_covid_deaths_high + 
#                           parent_covid_deaths_high + grandparent_covid_deaths_high]
#   
#   
#   return (vaccine_covid_impact)
#   
# } # end of function -- estimate_covid_deaths_old
# # ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
#
# estimate_covid_deaths -- new updated version 
#                          (incorporates psa & uncertainty propagation)
#
# ------------------------------------------------------------------------------
# estimate potential deaths due to covid-19 by continuing vaccination programmes
estimate_covid_deaths <- function (vaccine_impact_psa, 
                                   suspension_period, 
                                   psa) {
  
  vaccine_covid_impact <- vaccine_impact_psa 
  
  # timeline during which the covid deaths occur, that is 
  # if EPI is suspended for this period of time (unit in year), 
  # then what is the estimated number of covid deaths that are prevented 
  # by suspension of EPI
  vaccine_covid_impact [, suspension_period := suspension_period]
  
  # infection risk from contacts due to vaccination visits
  
  # using point estiamtes for deterministic values
  
  big_t <- suspension_period * 12 * 30   # duration of period at risk of SARS-CoV-2 ** in days **
  r0    <- 2.5                           # basic reproduction number
  theta <- 1-(1/r0)                      # proportion of pop. infected at big_t assuming no "overshooting"
  psi   <- 7                             # duration of infectiousness
  n     <- 2                             # number of potentially infectious contacts on visit to clinic
  iota1 <- 2                             # RR vacinator being infected + infectious vs community member
  iota2 <- 0.5                           # RR of infectious vaccinator transmitting vs community member
  p0    <- (theta * psi)/big_t           # prevalence of infectiousness amongst community members on given day
  pv    <- p0 * iota1                    # prevalence of infectiousness amongst vaccinators on given day
  big_n <- 5                             # av. no. of community member transmission relevant contacts per day
  t0    <- r0/(big_n * psi)              # prob. transmission given potentially infectious community contact
  tv    <- t0/iota2                      # prob. transmission given potentially infectious vaccinator contact
  
  pe_v1_mid <- (1-((1-pv)^(2*1*tv)*(1-p0)^(2*1*n*t0)))*(1-theta) # excess household risk from 1 visit
  pe_v2_mid <- (1-((1-pv)^(2*2*tv)*(1-p0)^(2*2*n*t0)))*(1-theta) # excess household risk from 2 visits
  pe_v3_mid <- (1-((1-pv)^(2*3*tv)*(1-p0)^(2*3*n*t0)))*(1-theta) # excess household risk from 3 visits
  
  # using PSA
  
  big_t <- runif  (psa, big_t-30, big_t+30) # uniform on -/+ 30 days from period that is passed in 
  r0    <- rgamma (psa, shape = 25, scale = (2.5/25))                   
  theta <- 1-(1/r0)            
  psi   <- rgamma (psa, shape = 14, scale = (7/14))                    
  n     <- runif  (psa, 1, 10)                       
  iota1 <- runif  (psa, 1, 4)                   
  iota2 <- runif  (psa, 0.25, 1)               
  p0    <- (theta * psi)/big_t    
  pv    <- p0 * iota1             
  big_n <- runif  (psa, 2, 10)                  
  t0    <- r0/(big_n * psi)       
  tv    <- t0/iota2               
  
  pe_v1 <- (1-((1-pv)^(2*1*tv)*(1-p0)^(2*1*n*t0)))*(1-theta) # excess household risk from 1 visit
  pe_v2 <- (1-((1-pv)^(2*2*tv)*(1-p0)^(2*2*n*t0)))*(1-theta) # excess household risk from 2 visits
  pe_v3 <- (1-((1-pv)^(2*3*tv)*(1-p0)^(2*3*n*t0)))*(1-theta) # excess household risk from 3 visits
  
  # # upper bound
  # pe_v1_high <- quantile (pe_v1, 0.975)
  # pe_v2_high <- quantile (pe_v2, 0.975) 
  # pe_v3_high <- quantile (pe_v3, 0.975) 
  # 
  # # lower bound
  # pe_v1_low <- quantile (pe_v1, 0.025) 
  # pe_v2_low <- quantile (pe_v2, 0.025) 
  # pe_v3_low <- quantile (pe_v3, 0.025)
  
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
  
  # ifr_child        <- 0.000016  # assume ifr for age 0-9
  # ifr_parents      <- 0.00031   # assume ifr for age 20-29 
  # ifr_grandparents <- 0.019     # assume ifr for age 60-69
  
  ifr_child        <- rgamma (psa, shape = 0.5163, rate = 10000) # assume ifr for age 0-9
  ifr_parents      <- rgamma (psa, shape = 3.5485, rate = 10000) # assume ifr for age 20-29 
  ifr_grandparents <- rgamma (psa, shape = 14.563, rate = 737.06) # assume ifr for age 60-69
  
  
  # add a column for estimated covid-19 deaths due to continuing vaccination programmes
  # if infection is imported into household then assume the following become infected:
  # (1) the child who attends the clinic
  # (2) other children in household based on the average household members <20 years in a 
  # household with at least one child - divded by 2 to account for birth order
  # (3) 2 adults in every household (caregiver who attends clinic plus one other)
  # (4) 2 adults over 60 adjusted for the proportion of households with members <20 years that
  # also have a household member >60 years
  
  vaccine_covid_impact [,child_covid_deaths      := 0]
  # vaccine_covid_impact [,child_covid_deaths_low  := 0]
  # vaccine_covid_impact [,child_covid_deaths_high := 0]
  
  vaccine_covid_impact [,sibling_covid_deaths      := 0]
  # vaccine_covid_impact [,sibling_covid_deaths_low  := 0]
  # vaccine_covid_impact [,sibling_covid_deaths_high := 0]
  
  vaccine_covid_impact [,parent_covid_deaths      := 0]
  # vaccine_covid_impact [,parent_covid_deaths_low  := 0]
  # vaccine_covid_impact [,parent_covid_deaths_high := 0]
  
  vaccine_covid_impact [,grandparent_covid_deaths      := 0]
  # vaccine_covid_impact [,grandparent_covid_deaths_low  := 0]
  # vaccine_covid_impact [,grandparent_covid_deaths_high := 0]
  
  vaccine_covid_impact [,covid_deaths      := 0]
  # vaccine_covid_impact [,covid_deaths_low  := 0]
  # vaccine_covid_impact [,covid_deaths_high := 0]
  
  
  # psa runs
  for (i in 1:psa) {
  
    # antigens with 3 contacts 
    
    # mid
    vaccine_covid_impact [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3") & run_id == i,
                          child_covid_deaths := 
                            vac_population * suspension_period * pe_v3 [i] * ifr_child [i]]
    
    vaccine_covid_impact [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3") & run_id == i,
                          sibling_covid_deaths := 
                            vac_population * suspension_period * pe_v3 [i] *
                            (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child [i])]
    
    vaccine_covid_impact [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3") & run_id == i,
                          parent_covid_deaths := 
                            vac_population * suspension_period * pe_v3 [i] * 2 * ifr_parents [i]]
    
    vaccine_covid_impact [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3") & run_id == i,
                          grandparent_covid_deaths := 
                            vac_population * suspension_period * pe_v3 [i] * 
                            (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
                               ifr_grandparents [i])]
    
    vaccine_covid_impact [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3") & run_id == i,
                          covid_deaths := child_covid_deaths + sibling_covid_deaths + 
                            parent_covid_deaths + grandparent_covid_deaths]
    
    # # low
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       child_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v3_low * ifr_child]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       sibling_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v3_low *
    #                         (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       parent_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v3_low * 2 * ifr_parents]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       grandparent_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v3_low * 
    #                         (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
    #                            ifr_grandparents)]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       covid_deaths_low := child_covid_deaths_low + sibling_covid_deaths_low + 
    #                         parent_covid_deaths_low + grandparent_covid_deaths_low]
    # 
    # # high
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       child_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v3_high * ifr_child]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       sibling_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v3_high *
    #                         (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       parent_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v3_high * 2 * ifr_parents]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       grandparent_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v3_high * 
    #                         (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
    #                            ifr_grandparents)]
    # vaccine_covid_impact [Vaccine %in% c("HepB3", "Hib3", "PCV3"),
    #                       covid_deaths_high := child_covid_deaths_high + sibling_covid_deaths_high + 
    #                         parent_covid_deaths_high + grandparent_covid_deaths_high]
    
    # antigens with 2 contacts
    # mid
    vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem") & run_id == i,
                          child_covid_deaths := 
                            vac_population * suspension_period * pe_v2 [i] * ifr_child [i]]
    
    vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem") & run_id == i,
                          sibling_covid_deaths := 
                            vac_population * suspension_period * pe_v2 [i] *
                            (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child [i])]
    
    vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem") & run_id == i,
                          parent_covid_deaths := 
                            vac_population * suspension_period * pe_v2 [i] * 2 * ifr_parents [i]]
    
    vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem") & run_id == i,
                          grandparent_covid_deaths := 
                            vac_population * suspension_period * pe_v2 [i] * 
                            (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
                               ifr_grandparents [i])]
    
    vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem") & run_id == i,
                          covid_deaths := child_covid_deaths + sibling_covid_deaths + 
                            parent_covid_deaths + grandparent_covid_deaths]
    
    # # low
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       child_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v2_low * ifr_child]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       sibling_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v2_low *
    #                         (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       parent_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v2_low * 2 * ifr_parents]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       grandparent_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v2_low * 
    #                         (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
    #                            ifr_grandparents)]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       covid_deaths_low := child_covid_deaths_low + sibling_covid_deaths_low + 
    #                         parent_covid_deaths_low + grandparent_covid_deaths_low]
    # 
    # # high
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       child_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v2_high * ifr_child]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       sibling_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v2_high *
    #                         (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       parent_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v2_high * 2 * ifr_parents]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       grandparent_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v2_high * 
    #                         (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
    #                            ifr_grandparents)]
    # vaccine_covid_impact [Vaccine %in% c("RotaC","HPVfem"),
    #                       covid_deaths_high := child_covid_deaths_high + sibling_covid_deaths_high + 
    #                         parent_covid_deaths_high + grandparent_covid_deaths_high]
    
    # antigens with 1 contacts
    # mid
    vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA") & run_id == i,
                          child_covid_deaths := 
                            vac_population * suspension_period * pe_v1 [i] * ifr_child [i]]
    
    vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA") & run_id == i,
                          sibling_covid_deaths := 
                            vac_population * suspension_period * pe_v1 [i] *
                            (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child [i])]
    
    vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA") & run_id == i,
                          parent_covid_deaths := 
                            vac_population * suspension_period * pe_v1 [i] * 2 * ifr_parents [i]]
    
    vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA") & run_id == i,
                          grandparent_covid_deaths := 
                            vac_population * suspension_period * pe_v1 [i] * 
                            (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
                               ifr_grandparents [i])]
    
    vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA") & run_id == i,
                          covid_deaths := child_covid_deaths + sibling_covid_deaths + 
                            parent_covid_deaths + grandparent_covid_deaths]
    
    # # low
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       child_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v1_low * ifr_child]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       sibling_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v1_low *
    #                         (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       parent_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v1_low * 2 * ifr_parents]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       grandparent_covid_deaths_low := 
    #                         vac_population * suspension_period * pe_v1_low * 
    #                         (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
    #                            ifr_grandparents)]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       covid_deaths_low := child_covid_deaths_low + sibling_covid_deaths_low + 
    #                         parent_covid_deaths_low + grandparent_covid_deaths_low]
    # 
    # # high
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       child_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v1_high * ifr_child]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       sibling_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v1_high *
    #                         (((under_20_in_hh_at_least_one_under_20 - 1)/2) * ifr_child)]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       parent_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v1_high * 2 * ifr_parents]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       grandparent_covid_deaths_high := 
    #                         vac_population * suspension_period * pe_v1_high * 
    #                         (2 * (percent_hh_under_20_and_over_60/percent_hh_at_least_one_under_20) * 
    #                            ifr_grandparents)]
    # vaccine_covid_impact [Vaccine %in% c("MCV1", "RCV1", "MCV2", "YFV", "MenA"),
    #                       covid_deaths_high := child_covid_deaths_high + sibling_covid_deaths_high + 
    #                         parent_covid_deaths_high + grandparent_covid_deaths_high]
    
  }
  
  return (vaccine_covid_impact)
  
} # end of function -- estimate_covid_deaths
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate benefit risk ratio
benefit_risk_ratio <- function (vaccine_covid_impact, 
                                suspension_period) {
  
  benefit_risk <- vaccine_covid_impact
  
  # deaths among different groups
  #   whole household (covid_deaths); 
  #   child (child_covid_deaths); 
  #   sibling (sibling_covid_deaths);
  #   parent (parent_covid_deaths); 
  #   grandparent (grandparent_covid_deaths)
  deaths <- c ("covid_deaths", 
               "child_covid_deaths", 
               "sibling_covid_deaths",
               "parent_covid_deaths",
               "grandparent_covid_deaths")
  
  # benefit_risk_ratios
  br_ratios <- c ("benefit_risk_ratio",
                  "child_benefit_risk_ratio", 
                  "sibling_benefit_risk_ratio", 
                  "parent_benefit_risk_ratio", 
                  "grandparent_benefit_risk_ratio")
  

  # compute benefit-risk ratio across combined vaccines in same visits
  
  # clinical visits for vaccination among children aged 0-11 months and their adult carers = 4 
  #   (3 visits for DTP3-HepB-Hib + PCV3 + RotaC) + 
  #   (1 visit for MCV1 + RCV1 + MenA + YF)
  # clinical visits for vaccination among children aged 12-23 months and their adult carers = 1 
  #   (1 visit for MCV2) -- single vaccine estimates already computed
  
  # ----------------------------------------------------------------------------
  # compute benefit-risk ratio for vaccines in the same visit
  benefit_risk_3visits_age0 <- benefit_risk [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3", "RotaC")]
  benefit_risk_1visits_age0 <- benefit_risk [Vaccine %in% c("MCV1", "RCV1", "MenA", "YFV")]
  
  # set vaccine name list
  benefit_risk_3visits_age0 [, Vaccine := "DTP3, HepB3, Hib3, PCV3, RotaC"]
  benefit_risk_1visits_age0 [, Vaccine := "MCV1, RCV1, MenA, YFV"]
  
  # add deaths averted by vaccination in the vaccine list
  benefit_risk_3visits_age0 [, vac_deaths_averted := sum (vac_deaths_averted, na.rm = T), by = .(ISO_code, run_id)]
  benefit_risk_1visits_age0 [, vac_deaths_averted := sum (vac_deaths_averted, na.rm = T), by = .(ISO_code, run_id)]
  
  # set covid deaths to the maximum estimate in the vaccine list
  # benefit_risk_3visits_age0 [, covid_deaths := max (covid_deaths), by = "ISO_code"]
  # benefit_risk_1visits_age0 [, covid_deaths := max (covid_deaths), by = "ISO_code"]
  
  for (death in deaths) {
    benefit_risk_3visits_age0 [, as.character (as.name (death)) := max (eval (as.name (death))), by = .(ISO_code, run_id)]
    benefit_risk_1visits_age0 [, as.character (as.name (death)) := max (eval (as.name (death))), by = .(ISO_code, run_id)]
  }
  
  # sample 1 row per country for combined vaccine impact estimates
  benefit_risk_3visits_age0 <- benefit_risk_3visits_age0 %>% group_by (ISO_code, run_id)
  benefit_risk_3visits_age0 <- sample_n (benefit_risk_3visits_age0, 1)
  
  benefit_risk_1visits_age0 <- benefit_risk_1visits_age0 %>% group_by (ISO_code, run_id)
  benefit_risk_1visits_age0 <- sample_n (benefit_risk_1visits_age0, 1)
  
  # add combined vaccine impact estimates to benefit risk table
  benefit_risk <- 
    rbindlist (list (benefit_risk, 
                     benefit_risk_3visits_age0, 
                     benefit_risk_1visits_age0), 
               use.names = TRUE, 
               fill      = TRUE)
  
  # ----------------------------------------------------------------------------
  # compute benefit-risk ratio across EPI vaccines
  benefit_risk_EPI <- benefit_risk [Vaccine %in% c("DTP3, HepB3, Hib3, PCV3, RotaC", 
                                                   "MCV1, RCV1, MenA, YFV",
                                                   "MCV2")]
  
  # set vaccine name list
  benefit_risk_EPI [, Vaccine := "DTP3, HepB3, Hib3, PCV3, RotaC, MCV1, RCV1, MenA, YFV, MCV2"]
  
  # add deaths averted by vaccination in the vaccine list
  benefit_risk_EPI [, vac_deaths_averted := sum (vac_deaths_averted, na.rm = T), by = .(ISO_code, run_id)]

  # add covid deaths in the vaccine list
  for (death in deaths) {
    benefit_risk_EPI [, as.character (as.name (death)) := sum (eval (as.name (death)), na.rm = T), by = .(ISO_code, run_id)]
  }
  
  # sample 1 row per country for combined vaccine impact estimates
  benefit_risk_EPI <- benefit_risk_EPI %>% group_by (ISO_code, run_id)
  benefit_risk_EPI <- sample_n (benefit_risk_EPI, 1)
  
  # add combined vaccine impact estimates to benefit risk table
  benefit_risk <- 
    rbindlist (list (benefit_risk, 
                     benefit_risk_EPI), 
               use.names = TRUE, 
               fill      = TRUE)
  # ----------------------------------------------------------------------------
  
  # estimate benefit-risk ratios among different groups
  for (i in 1:5) {
    
    benefit_risk [, as.character (as.name (br_ratios [i])) := 
                    (vac_deaths_averted * suspension_period) / eval (as.name (deaths [i]) ) ]
  }

  return (benefit_risk)
  
} # end of function -- benefit_risk_ratio
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate benefit risk ratio for the whole continent of Africa
benefit_risk_ratio_Africa <- function (vaccine_covid_impact, 
                                       suspension_period) {
  
  benefit_risk_Africa <- vaccine_covid_impact
  
  benefit_risk_Africa <- 
    benefit_risk_Africa [, list (vac_deaths_averted       = sum (vac_deaths_averted,       na.rm = T),
                                 child_covid_deaths       = sum (child_covid_deaths,       na.rm = T),
                                 sibling_covid_deaths     = sum (sibling_covid_deaths,     na.rm = T),         
                                 parent_covid_deaths      = sum (parent_covid_deaths,      na.rm = T),
                                 grandparent_covid_deaths = sum (grandparent_covid_deaths, na.rm = T),
                                 covid_deaths             = sum (covid_deaths,             na.rm = T)
    ), 
    by = .(run_id, Continent, Vaccine) ]
  
  # deaths among different groups
  #   whole household (covid_deaths); 
  #   child (child_covid_deaths); 
  #   sibling (sibling_covid_deaths);
  #   parent (parent_covid_deaths); 
  #   grandparent (grandparent_covid_deaths)
  deaths <- c ("covid_deaths", 
               "child_covid_deaths", 
               "sibling_covid_deaths",
               "parent_covid_deaths",
               "grandparent_covid_deaths")
  
  # benefit_risk_ratios
  br_ratios <- c ("benefit_risk_ratio",
                  "child_benefit_risk_ratio", 
                  "sibling_benefit_risk_ratio", 
                  "parent_benefit_risk_ratio", 
                  "grandparent_benefit_risk_ratio")
  
  
  # compute benefit-risk ratio across combined vaccines in same visits
  
  # clinical visits for vaccination among children aged 0-11 months and their adult carers = 4 
  #   (3 visits for DTP3-HepB-Hib + PCV3 + RotaC) + 
  #   (1 visit for MCV1 + RCV1 + MenA + YF)
  # clinical visits for vaccination among children aged 12-23 months and their adult carers = 1 
  #   (1 visit for MCV2) -- single vaccine estimates already computed
  
  # ----------------------------------------------------------------------------
  # compute benefit-risk ratio for vaccines in the same visit
  benefit_risk_3visits_age0 <- benefit_risk_Africa [Vaccine %in% c("Diptheria (DTP3)", "Tetanus (DTP3)", "Pertussis (DTP3)", "HepB3", "Hib3", "PCV3", "RotaC")]
  benefit_risk_1visits_age0 <- benefit_risk_Africa [Vaccine %in% c("MCV1", "RCV1", "MenA", "YFV")]
  
  # set vaccine name list
  benefit_risk_3visits_age0 [, Vaccine := "DTP3, HepB3, Hib3, PCV3, RotaC"]
  benefit_risk_1visits_age0 [, Vaccine := "MCV1, RCV1, MenA, YFV"]
  
  # add deaths averted by vaccination in the vaccine list
  benefit_risk_3visits_age0 [, vac_deaths_averted := sum (vac_deaths_averted, na.rm = T), by = .(run_id)]
  benefit_risk_1visits_age0 [, vac_deaths_averted := sum (vac_deaths_averted, na.rm = T), by = .(run_id)]
  
  # set covid deaths to the maximum estimate in the vaccine list
  # benefit_risk_3visits_age0 [, covid_deaths := max (covid_deaths), by = "ISO_code"]
  # benefit_risk_1visits_age0 [, covid_deaths := max (covid_deaths), by = "ISO_code"]
  
  for (death in deaths) {
    benefit_risk_3visits_age0 [, as.character (as.name (death)) := max (eval (as.name (death))), by = .(run_id)]
    benefit_risk_1visits_age0 [, as.character (as.name (death)) := max (eval (as.name (death))), by = .(run_id)]
  }
  
  # sample 1 row per country for combined vaccine impact estimates
  benefit_risk_3visits_age0 <- benefit_risk_3visits_age0 %>% group_by (run_id)
  benefit_risk_3visits_age0 <- sample_n (benefit_risk_3visits_age0, 1)
  
  benefit_risk_1visits_age0 <- benefit_risk_1visits_age0 %>% group_by (run_id)
  benefit_risk_1visits_age0 <- sample_n (benefit_risk_1visits_age0, 1)
  
  # add combined vaccine impact estimates to benefit risk table
  benefit_risk_Africa <- 
    rbindlist (list (benefit_risk_Africa, 
                     benefit_risk_3visits_age0, 
                     benefit_risk_1visits_age0), 
               use.names = TRUE, 
               fill      = TRUE)
  
  # ----------------------------------------------------------------------------
  # compute benefit-risk ratio across EPI vaccines
  benefit_risk_EPI <- benefit_risk_Africa [Vaccine %in% c("DTP3, HepB3, Hib3, PCV3, RotaC", 
                                                          "MCV1, RCV1, MenA, YFV",
                                                          "MCV2")]
  
  # set vaccine name list
  benefit_risk_EPI [, Vaccine := "DTP3, HepB3, Hib3, PCV3, RotaC, MCV1, RCV1, MenA, YFV, MCV2"]
  
  # add deaths averted by vaccination in the vaccine list
  benefit_risk_EPI [, vac_deaths_averted := sum (vac_deaths_averted, na.rm = T), by = .(run_id)]
  
  # add covid deaths in the vaccine list
  for (death in deaths) {
    benefit_risk_EPI [, as.character (as.name (death)) := sum (eval (as.name (death)), na.rm = T), by = .(run_id)]
  }
  
  # sample 1 row per country for combined vaccine impact estimates
  benefit_risk_EPI <- benefit_risk_EPI %>% group_by (run_id)
  benefit_risk_EPI <- sample_n (benefit_risk_EPI, 1)
  
  # add combined vaccine impact estimates to benefit risk table
  benefit_risk_Africa <- 
    rbindlist (list (benefit_risk_Africa, 
                     benefit_risk_EPI), 
               use.names = TRUE, 
               fill      = TRUE)
  # ----------------------------------------------------------------------------
  
  # estimate benefit-risk ratios among different groups
  for (i in 1:5) {
    
    benefit_risk_Africa [, as.character (as.name (br_ratios [i])) := 
                           (vac_deaths_averted * suspension_period) / eval (as.name (deaths [i]) ) ]
  }
  
  return (benefit_risk_Africa)
  
} # end of function -- benefit_risk_ratio_Africa
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate benefit risk ratio -- summary estimates (median, credible intervals)
benefit_risk_ratio_summary <- function (benefit_risk) {
  
  benefit_risk_summary <- benefit_risk
  
  # estimate benefit-risk ratios among different groups
  # median and credible intervals
  benefit_risk_summary <- 
    benefit_risk_summary [, list (benefit_risk_ratio                  = quantile (benefit_risk_ratio,             0.5,   na.rm = T),
                                  benefit_risk_ratio_low              = quantile (benefit_risk_ratio,             0.025, na.rm = T), 
                                  benefit_risk_ratio_high             = quantile (benefit_risk_ratio,             0.975, na.rm = T), 
                                  
                                  child_benefit_risk_ratio            = quantile (child_benefit_risk_ratio,       0.5,   na.rm = T),
                                  child_benefit_risk_ratio_low        = quantile (child_benefit_risk_ratio,       0.025, na.rm = T), 
                                  child_benefit_risk_ratio_high       = quantile (child_benefit_risk_ratio,       0.975, na.rm = T),
                                  
                                  sibling_benefit_risk_ratio          = quantile (sibling_benefit_risk_ratio,     0.5,   na.rm = T),
                                  sibling_benefit_risk_ratio_low      = quantile (sibling_benefit_risk_ratio,     0.025, na.rm = T), 
                                  sibling_benefit_risk_ratio_high     = quantile (sibling_benefit_risk_ratio,     0.975, na.rm = T),
                                  
                                  parent_benefit_risk_ratio           = quantile (parent_benefit_risk_ratio,      0.5,   na.rm = T),
                                  parent_benefit_risk_ratio_low       = quantile (parent_benefit_risk_ratio,      0.025, na.rm = T), 
                                  parent_benefit_risk_ratio_high      = quantile (parent_benefit_risk_ratio,      0.975, na.rm = T),
                                  
                                  grandparent_benefit_risk_ratio      = quantile (grandparent_benefit_risk_ratio, 0.5,   na.rm = T),
                                  grandparent_benefit_risk_ratio_low  = quantile (grandparent_benefit_risk_ratio, 0.025, na.rm = T), 
                                  grandparent_benefit_risk_ratio_high = quantile (grandparent_benefit_risk_ratio, 0.975, na.rm = T), 
                                  
                                  vac_deaths_averted                  = quantile (vac_deaths_averted,             0.5,   na.rm = T),
                                  vac_deaths_averted_low              = quantile (vac_deaths_averted,             0.025, na.rm = T), 
                                  vac_deaths_averted_high             = quantile (vac_deaths_averted,             0.975, na.rm = T), 
                                  
                                  covid_deaths                         = quantile (covid_deaths,                  0.5,   na.rm = T),
                                  covid_deaths_low                     = quantile (covid_deaths,                  0.025, na.rm = T), 
                                  covid_deaths_high                    = quantile (covid_deaths,                  0.975, na.rm = T), 
                                  
                                  child_covid_deaths                  = quantile (child_covid_deaths,             0.5,   na.rm = T),
                                  child_covid_deaths_low              = quantile (child_covid_deaths,             0.025, na.rm = T), 
                                  child_covid_deaths_high             = quantile (child_covid_deaths,             0.975, na.rm = T), 
                                  
                                  sibling_covid_deaths                = quantile (sibling_covid_deaths,           0.5,   na.rm = T),
                                  sibling_covid_deaths_low            = quantile (sibling_covid_deaths,           0.025, na.rm = T), 
                                  sibling_covid_deaths_high           = quantile (sibling_covid_deaths,           0.975, na.rm = T), 
                                  
                                  parent_covid_deaths                  = quantile (parent_covid_deaths,           0.5,   na.rm = T),
                                  parent_covid_deaths_low              = quantile (parent_covid_deaths,           0.025, na.rm = T), 
                                  parent_covid_deaths_high             = quantile (parent_covid_deaths,           0.975, na.rm = T), 
                                  
                                  grandparent_covid_deaths             = quantile (grandparent_covid_deaths,      0.5,   na.rm = T),
                                  grandparent_covid_deaths_low         = quantile (grandparent_covid_deaths,      0.025, na.rm = T), 
                                  grandparent_covid_deaths_high        = quantile (grandparent_covid_deaths,      0.975, na.rm = T)
                                  ), 
                          by = .(ISO_code, Vaccine) ]
  
  return (benefit_risk_summary)
  
} # end of function -- benefit_risk_ratio_summary
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# generate map of benefit risk ratio
benefit_risk_ratio_map <- function (benefit_risk_summary, 
                                    suspension_period_string, 
                                    age_group) {
  # file to save plots
  pdf (paste0 ("figures/benefit_risk_ratio_maps_", 
               suspension_period_string, "_suspension_",
               age_group, ".pdf"))
  
  # ggplot theme
  theme_set (theme_bw())
  
  # benefit_risk_ratios
  br_ratios <- c ("benefit_risk_ratio",
                  "child_benefit_risk_ratio", 
                  "sibling_benefit_risk_ratio", 
                  "parent_benefit_risk_ratio", 
                  "grandparent_benefit_risk_ratio")
  
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
  setDT  (africa)
  setkey (africa, sov_a3)
  
  # list of vaccines
  vaccines <- unique (benefit_risk$Vaccine)
  
  # benefit-risk ratios for different groups
  for (br_ratio in br_ratios) {
    
    # generate benefit-risk ratio maps for different vaccines
    for (vaccine in vaccines) {
      
      # combine tables to add geometry
      dt <- merge (x    = benefit_risk_summary [Vaccine == vaccine], 
                   y    = africa, 
                   by.x = "ISO_code", 
                   by.y = "iso_a3", 
                   all  = T )
      
      # copy data for Somalia to Somaliland
      dt [sov_a3 == "SOL", 
          c(as.character (as.name (br_ratio))) := dt [ISO_code == "SOM", eval (as.name (br_ratio))]
          ]
      
      # map of benefit-risk ratio for different vaccines
      p <- ggplot (data = dt) +
        geom_sf (aes (fill = eval (as.name (br_ratio)), geometry = geometry)) + 
        scale_fill_viridis_c (option = "plasma", direction = -1, limits = c(0, NA), na.value = "grey80") + 
        # scale_fill_gradient2 (midpoint = 0,
        #                       low = "red",
        #                       mid = "white",
        #                       high = "blue",
        #                       na.value = "grey70",
        #                       trans = "log",
        #                       limits = c (0.5, NA),
        #                       breaks = c(0, 0.5, 1, 5, 10, 50, 100)) +
        labs (title = "Deaths averted by vaccination per excess COVID-19 death",  # uncomment this line to generate figures for paper
        # labs (title    = paste0 ("Deaths averted by vaccination per excess COVID-19 death / ", br_ratio),  # comment
              subtitle = paste0 (vaccine), 
          #                       "\n EPI suspension period: ", suspension_period_string,           # comment
           #                      " / vaccine impact: ", vaccine_impact_timeline),                  # comment
              fill     = "benefit-risk ratio") +
        theme (axis.text.x      = element_blank(), axis.ticks = element_blank()) + 
        theme (axis.text.y      = element_blank(), axis.ticks = element_blank()) + 
        theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
        theme (plot.title       = element_text(size = 12)) +
        theme (plot.subtitle    = element_text(size = 10)) +
        theme (legend.title     = element_text(size = 10))

      
      print (p)
      
    } # end -- for (vaccine in vaccines)
    
  } # end -- for (br_ratio in br_ratios) 
  
  dev.off ()  # close plots file
  
  return ()
  
} # end of function -- benefit_risk_ratio_map
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# save benefit-risk results 
save_benefit_risk_results <- function (benefit_risk, 
                                       benefit_risk_summary, 
                                       benefit_risk_summary_Africa, 
                                       suspension_period_string, 
                                       age_group) {

  # save benefit-risk results of all psa runs
  fwrite (benefit_risk, file = paste0 ("tables/benefit_risk_results_", 
                                       suspension_period_string, 
                                       "_suspension_", 
                                       age_group, 
                                       ".csv") )
  
  # save benefit-risk results summary -- country level
  fwrite (benefit_risk_summary , file = paste0 ("tables/benefit_risk_summary_results_", 
                                                suspension_period_string, 
                                                "_suspension_", 
                                                age_group, 
                                                ".csv") )
  
  # save benefit-risk results summary -- continent level
  fwrite (benefit_risk_summary_Africa , file = paste0 ("tables/benefit_risk_summary_Africa_results_", 
                                                       suspension_period_string, 
                                                       "_suspension_", 
                                                       age_group, 
                                                       ".csv") )
  
  # ----------------------------------------------------------------------------
  # save benefit-risk results summary -- continent level -- for paper
  
  # ----------------------------------------------------------------------------

return ()

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

set.seed (1)  # seed for random number generator
psa <- 10   # number of runs for probabilistic sensitivity analysis

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
  # age_groups <- c("under5", "all")
  age_groups <- c("under5")
  
  for (age_group in age_groups) {
    
    # extract vaccine coverage estimates for 2018 from WHO for 54 African countries
    vaccine_coverage <- get_vaccine_coverage (age_group)
    
    # add population estimates from UNWPP 2019
    vaccine_coverage_pop <- add_population (vaccine_coverage)
    
    # add UN household size data from DHS / IPUMS
    vaccine_coverage_pop_hh <- add_hh_size_data (vaccine_coverage_pop)
    
    # add deaths averted by vaccination among "all" or "under5" age groups
    vaccine_impact_psa <- deaths_averted_vaccination (vaccine_coverage_pop_hh, 
                                                      age_group = age_group, 
                                                      psa)
    
    # estimate potential deaths due to covid-19 by continuing vaccination programmes
    vaccine_covid_impact <- estimate_covid_deaths (vaccine_impact_psa, 
                                                   suspension_period, 
                                                   psa)
  
    # --------------------------------------------------------------------------
    # estimate benefit risk ratio
    # country level
    benefit_risk <- benefit_risk_ratio (vaccine_covid_impact, 
                                        suspension_period)
    
    # continental level
    benefit_risk_Africa <- benefit_risk_ratio_Africa (vaccine_covid_impact, 
                                                      suspension_period)
    # --------------------------------------------------------------------------

    # --------------------------------------------------------------------------
    # estimate benefit risk ratio -- summary estimates (median, credible intervals)
    # country level
    benefit_risk_summary <- benefit_risk_ratio_summary (benefit_risk)
    
    # add country names
    benefit_risk_summary [, Country := countrycode (sourcevar   = ISO_code,
                                                    origin      = "iso3c",
                                                    destination = "country.name")]
    # set column order
    setcolorder (benefit_risk_summary, "Country")
    
    # continental level
    # rename Continent column to ISO_code
    setnames (benefit_risk_Africa, "Continent", "ISO_code")
    
    # estimate benefit risk ratio -- summary estimates (median, credible intervals)
    benefit_risk_summary_Africa <- benefit_risk_ratio_summary (benefit_risk_Africa) 
    
    # rename ISO_code column back to Continent
    setnames (benefit_risk_summary_Africa, "ISO_code", "Continent")
    # --------------------------------------------------------------------------
    
    # generate map of benefit risk ratio
    benefit_risk_ratio_map (benefit_risk_summary,
                            suspension_period_string,
                            age_group = age_group)
    
    # save benefit-risk results
    save_benefit_risk_results (benefit_risk, 
                               benefit_risk_summary, 
                               benefit_risk_summary_Africa, 
                               suspension_period_string, 
                               age_group = age_group)
    
  } # end -- for (age_group in age_groups)

} # end -- for (period in 1:length (suspension_periods))

# return to source directory
setwd (source_wd)

# end time
print (Sys.time ())
toc ()
# ------------------------------------------------------------------------------
