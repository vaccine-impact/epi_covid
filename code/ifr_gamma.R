# Approximate distributions for IFRs

library("rriskDistributions")

# ifr data from Verity et al.

ifr_age_0_9     <- c(0.00000185,0.0000161,0.000249) # lower, mid, high
ifr_age_20_30   <- c(0.000138,0.000309,0.000923)
ifr_age_60_70   <- c(0.0111,0.0193,0.0389)

p               <- c(0.025,0.5,0.975) # quantiles corresponding to vectors above

# get beta parameters

get.beta.par(p,ifr_age_0_9, tol=0.01)
get.beta.par(p,ifr_age_20_30)
get.beta.par(p,ifr_age_60_70)


# get gamma parameters

get.gamma.par(p,ifr_age_0_9, tol=0.01)
get.gamma.par(p,ifr_age_20_30)
get.gamma.par(p,ifr_age_60_70)