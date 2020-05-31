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
  
  # Basic reproduction number for SARS-CoV-2
  Ro <- rgamma (n = runs, shape = 25, scale = 2.5/25)
  
  # Proportion of SARS-CoV-2 infected population at the end of the study period 
  # assuming neither 
  # (i) "overshooting" of the epidemic due to high rates of transmission  or 
  # (ii) elimination of transmission prior to herd immunity being reached.
  theta <- 1 - 1/Ro
  
  # Duration of period at risk for SARS-CoV-2
  time_period_T <- runif (n = runs, min = 4*30, max = 6*30)
  
  # Duration of infectiousness
  infectious_period_psi <- rgamma (n = runs, shape = 14, scale = 7/14)
  
  # Average number of transmission relevant contacts of a community member per day
  n <- runif (n = runs, min = 2, max = 10)
  
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
  to <- Ro / (n * infectious_period_psi)
  
  # Probability of transmission given potentially infectious contact with vaccinators
  tv <- iota_2 * to 
  
  # Probability for SARS-CoV-2 infection for the whole household of a child who gets vaccinated
  P <- (1 - (1-tv)^(2*visits*pv) * (1 - to)^(2*visits*po*n))
  
  # Probability for excess SARS-CoV-2 infection for the whole household of a child who gets vaccinated
  Pe <- P * (1 - theta)
  
  # print risk
  print ("risk calculation")
  print (paste0 ("visits = ", visits, ", Pe = "))
  print (quantile (Pe, c(0.5, 0.025, 0.975)))
  
  # print prevalence (to address reviewer comment)
  print ("prevalence")
  print (quantile (po, c(0.5, 0.025, 0.975)))
  
  # print theta -- proportion of SARS-CoV-2 infected population at the end of the study period
  print ("theta -- proportion of SARS-CoV-2 infected population at the end of the study period")
  print (quantile (theta, c(0.5, 0.025, 0.975)))
  
  # print Basic reproduction number for SARS-CoV-2
  print ("Basic reproduction number for SARS-CoV-2")
  print (quantile (Ro, c(0.5, 0.025, 0.975)))
}
# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------
# set seed
set.seed (1)

# three immunisation visits for EPI-1
risk_calculation (visits = 3)

# each immunisation visit of EPI-2 and EPI-3
risk_calculation (visits = 1)
# ------------------------------------------------------------------------------

