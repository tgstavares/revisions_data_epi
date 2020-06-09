/*
		this do-file generates counts of covid-confirmed deaths 
			by date of occurrence x date of report for each municipality
			using the Mexico data 
			
		generated file:		mx_covid_deaths_mun.dta
		
*/


clear 
set scheme s1color
set seed 1010

*set directory here:
global covid ""


use ${covid}stata_code/outdata/clean_mx_full.dta, clear

*keep the relevant population
keep if ever_dead==1 & covid_conf==1

*generate an identifier by state-municipality 
tostring state_res, gen(s) format(%02.0f)
tostring mun_res, gen(m) format(%03.0f)
egen geo=concat(s m)
label variable geo "State-mun identifier"

*get a counter
gen deaths=1

*get the aggregates 
collapse (sum) deaths, by(geo date_death date_death_rep)

*cleanup 
order geo date_death date_death_rep deaths
sort geo date_death date_death_rep
label variable deaths "Total deaths"

*fix instances of reports before actual death happens 
replace date_death_rep= date_death if date_death_rep-date_death<0

*generate delay
gen delay= date_death_rep-date_death
label variable delay "Days of delay for reporting deaths"


save ${covid}stata_code/outdata/mx_covid_deaths_mun.dta, replace
