/*
		this do-file generates counts of covid-confirmed deaths 
			by date of occurrence x date of report for each trust 
			for the England NHS data
			
		generated file:		eng_covid_deaths_trust.dta
		
*/


clear 
set scheme s1color
set seed 1010

*set directory here:
global covid ""

local last_file=date("June-8-2020","MDY")


 
/* ++++++++++++++++++++++++++++++++++++++++
			useful locals for later
++++++++++++++++++++++++++++++++++++++++ */

*total files available until current date
local totfiles=`last_file'-date("Apr-2-2020","MDY")+1

*last day on file for the month of MAY
local lastday=day(`last_file')


/* ++++++++++++++++++++++++++++++++++++++++
			process raw data
++++++++++++++++++++++++++++++++++++++++ */

cap prog drop nhs_process
prog define nhs_process

	syntax, m(string) d(numlist) sav(numlist)
	
	if "`m'"=="June" {
		local s="Tab4 Deaths by trust"
	}
	else if "`m'"=="May" & `d'>=21 {
		local s="Tab4 Deaths by trust"
	}
	else {
		local s="COVID19 daily deaths by trust"
	} 
	
	clear
	import excel ${covid}NHS_data/COVID-19-daily-announced-deaths-`d'-`m'-2020.xlsx, sheet(`s') cellrange(B14) allstring clear

	missings dropvars, force

	foreach j of varlist _all { 
		 local k = strtoname(`j'[1]) 
		 rename `j'  deaths`k' 
	}

	drop deathsAwaiting_verification deathsTotal

	rename deathsNHS_England_Region region
	rename deathsCode trust
	rename deathsName trust_name

	gen aux=strlen(trust)
	drop if aux!=3
	drop aux

	foreach j of varlist deaths* {
		destring `j', replace
	}

	reshape long deaths, i(region trust trust_name) j(date_str) string

	gen d=substr(date_str,2,.)
	gen d2=d+"20"
	gen date_death=date(d2, "DMY")
	format date_death %td

	keep region trust trust_name deaths date_death

	gen date_death_rep=date("`d'-`m'-2020","DMY")
	replace date_death_rep=date_death_rep-1
	format date_death_rep %td

	order region trust trust_name date_death date_death_rep deaths 

	drop if deaths==0
	drop if deaths==.

	save ${covid}stata_code/temp/eng_`sav'.dta, replace
	
end 

* ==============================================================================

/* ++++++++++++++++++++++++++++++++++++++++
			run program
++++++++++++++++++++++++++++++++++++++++ */

*run processing program by month

local i=1

	*april
forval k=2(1)30 {
	nhs_process, m(April) d(`k') sav(`i')
	local i=`i'+1
}

	*may
forval k=1(1)31 {
	nhs_process, m(May) d(`k') sav(`i')
	local i=`i'+1
}

	*june
forval k=1(1)`lastday' {
	nhs_process, m(June) d(`k') sav(`i')
	local i=`i'+1
}


/* ++++++++++++++++++++++++++++++++++++++++
			put together and format
++++++++++++++++++++++++++++++++++++++++ */
  
use ${covid}stata_code/temp/eng_1.dta, clear

forval k=2(1)`totfiles' {
		
	append using ${covid}stata_code/temp/eng_`k'.dta
}

*cleanup
label variable region "Region in England"
label variable trust "Hospital trust/system"
label variable trust_name "Hospital trust/system name"
label variable date_death "Date of death"
label variable date_death_rep "Date death was reported"
label variable deaths "Total deaths"
order region trust trust_name date_death date_death_rep deaths
sort region trust trust_name date_death date_death_rep
replace trust_name=strtrim(trust_name)
replace region=strtrim(region)

*fix instances of reports before actual death happens 
replace date_death_rep= date_death if date_death_rep-date_death<0

*generate delay
gen delay= date_death_rep-date_death
label variable delay "Days of delay for reporting deaths"

save ${covid}stata_code/outdata/eng_covid_deaths_trust.dta, replace

forval k=1(1)`totfiles' {
	rm ${covid}stata_code/temp/eng_`k'.dta
}


