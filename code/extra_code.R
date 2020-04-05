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