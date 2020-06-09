READ ME for Stata code

Gutierrez, Rubli, Tavares. Delays in Death Reports and their Implications for Tracking the Evolution of COVID-19. 


This is a description of Stata do-files. 
This generates Figs. 1-5 in the paper. 
Each do-file contains more info on what is being done. 



SETTING UP DATA FOR MEXICO 

- gen_mex_data: takes the raw data for Mexico and generates a cleaned up file with individual records (each observation is a person)

- gen_mex_covid_deaths: takes the cleaned up file from the Mexico data and generates a dataset with observations at the municipality-date of death level detailing deaths reported at different dates 



SETTING UP DATA FOR ENGLAND 

- gen_eng_covid_deaths: takes the NHS England data and generates a dataset with observations at the trust-date of death level detailing deaths reported at different dates



ANALYSIS 

- desc_graphs: a few descriptive graphs of how deaths have evolved over time (occurred vs reported) 

- regs_deaths: build info on delays, histograms of delays, regressions and plotting coefficients in different ways for comparison 
