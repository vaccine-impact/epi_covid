README 
---------------------------------------------------------------------------------------
To assess the health benefits of vaccination versus the risks of sustaining or 
suspending immunisation programmes in Africa during the COVID-19 pandemic. 

Folders:
* code    - main program
* data    - data files (input)
* figures - maps of results (output)
* tables  - results table including benefit-risk ratios (output)

- Run the program (epi_covid.R) from the code folder (source directory). 
- The maps of results will be generated in figures folder.
- The results table will be saved in the tables folder.
---------------------------------------------------------------------------------------

Analysis:
Change the suspension period to different time periods to analyse the benefits of 
vaccination versus risks of excess COVID-19 deaths.   

# potential delay or suspension period of EPI due to COVID-19
suspension_period        <- 0.5  # unit in year
suspension_period_string <- "6 months"

The excess COVID-19 deaths occur due to social interactions between EPI staff and vaccinated individuals and their carers, in comparison to no interactions during EPI
suspension period. But this leads to increase in vaccine-preventable deaths during the lifetime of the vaccinated cohorts. 
---------------------------------------------------------------------------------------