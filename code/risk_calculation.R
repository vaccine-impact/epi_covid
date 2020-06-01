# Results -- risk calculation
# We estimate that the three immunisation visits for EPI-1 add 6% altogether 
# and each immunisation visit of EPI-2 and EPI-3 add 2% to the probability of 
# a SARS-CoV-2 infection in the household. 

rm (list = ls ())

# ------------------------------------------------------------------------------
# function to calculate risk of immunisation visits
# -- get risk values for paper results -- 
# We estimate that the three immunisation visits for EPI-1 add 2.3% (0.6 - 7.8) 
# altogether and each immunisation visit of EPI-2 and EPI-3 add 0.8% (0.2 - 2.8) 
# to the probability of excess SARS-CoV-2 infection in the household.
# ------------------------------------------------------------------------------
risk_calculation <- function (visits, 
                              runs = 100000) {
  
  cat (paste0 ("\n\n", "visits = ", visits, "\n\n"))
  
  # Basic reproduction number for SARS-CoV-2
  Ro <- rgamma (n = runs, shape = 25, scale = 2.5/25)
  
  # Proportion of SARS-CoV-2 infected population at the end of the study period 
  # assuming neither 
  # (i) "overshooting" of the epidemic due to high rates of transmission  or 
  # (ii) elimination of transmission prior to herd immunity being reached.
  theta <- 1 - 1/Ro
  
  # Duration of period at risk for SARS-CoV-2
  time_period_T <- runif (n = runs, min = 5/12 * 366, max = 6/12 * 366)
  
  # Duration of infectiousness
  infectious_period_psi <- rgamma (n = runs, shape = 14, scale = 7/14)
  
  # Average number of transmission relevant contacts of a community member per day
  N <- runif (n = runs, min = 2, max = 10)
  
  # Risk ratio of a vaccinator being infected and infectious versus another community member
  iota_1 <- runif (n = runs, min = 1, max = 4)
  
  # Risk ratio per potentially infectious contact of a vaccinator transmitting versus another community member
  iota_2 <- runif (n = runs, min = 0.25, max = 1)
  
  # Number of non-vaccinator contacts of child and carer during their travel to the vaccine clinic and in the waiting room
  n <- runif (n = runs, min = 1, max = 10)
  
  # Prevalence of infectious community members on any given day
  po <- theta * infectious_period_psi / time_period_T
  
  # Prevalence of infectious vaccinators on any given day
  pv <- iota_1 * po
  
  # Probability of transmission given potentially infectious contact with community members
  to <- Ro / (N * infectious_period_psi)
  
  # Probability of transmission given potentially infectious contact with vaccinators
  tv <- iota_2 * to 
  
  # Probability for SARS-CoV-2 infection for the whole household of a child who gets vaccinated
  P <- (1 - (1-tv)^(2*visits*pv) * (1 - to)^(2*visits*po*n))
  
  # Probability for excess SARS-CoV-2 infection for the whole household of a child who gets vaccinated
  Pe <- P * (1 - theta)
  
  # print prevalence (to address reviewer comment)
  print ("prevalence (po)")
  print (quantile (po, c(0.5, 0.025, 0.975)))
  
  print ("prevalence (pv)")
  print (quantile (pv, c(0.5, 0.025, 0.975)))
  
  
  # print theta -- proportion of SARS-CoV-2 infected population at the end of the study period
  print ("theta -- proportion of SARS-CoV-2 infected population at the end of the study period")
  print (quantile (theta, c(0.5, 0.025, 0.975)))
  
  # print Basic reproduction number for SARS-CoV-2
  print ("Basic reproduction number for SARS-CoV-2")
  print (quantile (Ro, c(0.5, 0.025, 0.975)))
  
  # print Duration of infectiousness
  print ("# Duration of infectiousness")
  print (quantile (infectious_period_psi, c(0.5, 0.025, 0.975)))
  
  # print Probability for SARS-CoV-2 infection for the whole household of a child who gets vaccinated
  print ("P - Probability for SARS-CoV-2 infection for the whole household of a child who gets vaccinated")
  print (quantile (P, c(0.5, 0.025, 0.975)))
  
  # print Probability for excess SARS-CoV-2 infection for the whole household of a child who gets vaccinated
  print ("Pe - Probability for excess SARS-CoV-2 infection for the whole household of a child who gets vaccinated")
  print (paste0 ("visits = ", visits, ", Pe = "))
  print (quantile (Pe, c(0.5, 0.025, 0.975)))
  
  # print Probability of transmission given potentially infectious contact with community members
  print ("to - Probability of transmission given potentially infectious contact with community members")
  print (quantile (to, c(0.5, 0.025, 0.975)))
  
  # print Probability of transmission given potentially infectious contact with vaccinators
  print ("tv - Probability of transmission given potentially infectious contact with vaccinators")
  print (quantile (tv, c(0.5, 0.025, 0.975)))
  
}
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# function - to calculate infection fatality rates from Verity paper
ifr <- function (runs = 100000) {
  
  psa <- runs
  
  # ----------------------------------------------------------------------------
  # get shape and rate parameters for child, parents (adults), and grandparents (older adults)
  #
  # Source: Verity R, Okell LC, Dorigatti I, Winskill P, Whittaker C, Imai N, et al. 
  # Estimates of the severity of coronavirus disease 2019: a model-based analysis. 
  # Lancet Infect Dis. 2020; doi:10.1016/S1473-3099(20)30243-7
  #
  # note: these values are in percentages
  #
  # child 0-9 years -- proxy for less than 20 years old
  parameters <- suppressMessages (suppressWarnings (get.gamma.par (p = c(0.025, 0.5, 0.975), 
                                                                   q = c(0.000185, 0.00161, 0.0249), 
                                                                   show.output = FALSE,
                                                                   plot        = FALSE) ))
  shape_child <- parameters ["shape"]
  rate_child  <- parameters ["rate"]
  
  # adults 30-39 years -- proxy for 20-60 years old
  parameters <- suppressMessages (suppressWarnings (get.gamma.par (p = c(0.025, 0.5, 0.975), 
                                                                   q = c(0.0408, 0.0844, 0.185), 
                                                                   show.output = FALSE,
                                                                   plot        = FALSE) ))
  shape_parents <- parameters ["shape"]
  rate_parents  <- parameters ["rate"]
  
  # adults > 60 years
  parameters <- suppressMessages (suppressWarnings (get.gamma.par (p = c(0.025, 0.5, 0.975), 
                                                                   q = c(1.82, 3.28, 6.18), 
                                                                   show.output = FALSE,
                                                                   plot        = FALSE) ))
  shape_grandparents <- parameters ["shape"]
  rate_grandparents  <- parameters ["rate"]
  
  # infection fatality rate (percentages) for child, parents (adults), and grandparents (older adults)
  ifr_child        <- rgamma (n = psa, shape = shape_child,        rate = rate_child)         
  ifr_parents      <- rgamma (n = psa, shape = shape_parents,      rate = rate_parents)       
  ifr_grandparents <- rgamma (n = psa, shape = shape_grandparents, rate = rate_grandparents)  
  
  # ----------------------------------------------------------------------------
  # output -- gamma -- shape and rate; and IFR (child, adults, older adults)
  
  print ("child parameters -- gamma -- shape and rate")
  print (shape_child)
  print (rate_child)
  
  print ("IFR - child")
  print (quantile (ifr_child, c(0.5, 0.025, 0.975)))
  
  print ("adults parameters -- gamma -- shape and rate")
  print (shape_parents)
  print (rate_parents)
  
  print ("IFR - parents")
  print (quantile (ifr_parents, c(0.5, 0.025, 0.975)))
  
  print ("older adults parameters -- gamma -- shape and rate")
  print (shape_grandparents)
  print (rate_grandparents)
  
  print ("IFR - grandparents")
  print (quantile (ifr_grandparents, c(0.5, 0.025, 0.975)))
  # ----------------------------------------------------------------------------
  
}
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# set seed
set.seed (1)

# three immunisation visits for EPI-1
risk_calculation (visits = 3)

# each immunisation visit of EPI-2 and EPI-3
risk_calculation (visits = 1)

# infection fatality rate estimates 
ifr ()
# ------------------------------------------------------------------------------

