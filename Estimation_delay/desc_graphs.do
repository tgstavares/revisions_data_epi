/*
		this do-file plots descriptive graphs for mexico and england 
		
*/


clear 
set scheme s1color
set seed 1010
grstyle clear

grstyle init
grstyle anglestyle vertical_tick horizontal
grstyle color p1 cranberry
grstyle color p2 black 

*set directory here:
global covid ""


/* ====================================
			put them together
==================================== */

use ${covid}stata_code/outdata/mx_covid_deaths_mun.dta, clear
gen mex=1
append using ${covid}stata_code/outdata/eng_covid_deaths_trust.dta
replace mex=0 if mex==.

*fix geo for england 
egen aux=concat(region trust)
replace geo=aux if geo==""
drop aux

*get deaths that occurred and the total
preserve 

	collapse (sum) deaths_occ=deaths, by(date_death mex)

	rename date_death date 
	label variable date ""

	label variable deaths_occ "Occurred deaths"

	sort mex date 
	bysort mex: gen total_deaths_occ=sum(deaths_occ)
	label variable total_deaths_occ "Total occurred deaths"

	save ${covid}stata_code/temp/deaths_occ.dta, replace 
	
restore 

*get deaths that were reported and the total 
preserve 

	collapse (sum) deaths_rep=deaths, by(date_death_rep mex)

	rename date_death_rep date 
	label variable date ""

	label variable deaths_rep "Reported deaths"

	sort mex date 
	bysort mex: gen total_deaths_rep=sum(deaths_rep)
	label variable total_deaths_rep "Total reported deaths"

	save ${covid}stata_code/temp/deaths_rep.dta, replace 
	
restore 

*put them together
use ${covid}stata_code/temp/deaths_occ.dta, clear
merge 1:1 date mex using ${covid}stata_code/temp/deaths_rep.dta, nogen 

save ${covid}stata_code/temp/deaths_occ_rep.dta, replace 
rm ${covid}stata_code/temp/deaths_occ.dta
rm ${covid}stata_code/temp/deaths_rep.dta

*fix cumulative counts for what we don't observe for england 
local k=2283
gen total_deaths_occ_shift=`k'+total_deaths_occ if mex==0
gen total_deaths_rep_shift=`k'+total_deaths_rep if mex==0

*nothing to fix for mexico 
replace total_deaths_occ_shift=total_deaths_occ if mex==1
replace total_deaths_rep_shift=total_deaths_rep if mex==1

*format date without year
format date %tdMonDD


/* ======================
		england
====================== */
local overlapper=date("20-Apr-2020","DMY")

qui sum date if mex==0 & date>date("31-Mar-2020","DMY"), det
local themin=`overlapper'-14
local themax=r(max)
local maxgr=`themax'+2
twoway (bar deaths_occ date, lc(cranberry) lw(medthick) fc(none)) ///
	(bar deaths_rep date, lc(cranberry%20) fc(cranberry%20)) if mex==0 & date>date("31-Mar-2020","DMY"), ///
	xti(" ") yti("Total deaths", height(5)) ///
	xscale(r(`themin' `maxgr')) xlab(`themin'(14)`themax') ///
	yscale (r(0 1200)) ylab(0(200)1200) ///
	legend(order(1 "Occurred" 2 "Reported") ring(0) position(10) bmargin(small) region(lwidth(none) fc(none)) col(1))  
graph export ${covid}stata_code/graphs/england_daily_deaths.png, replace 

qui sum date if mex==0 & date>date("31-Mar-2020","DMY"), det
local themin=`overlapper'-14
local themax=r(max)
local maxgr=`themax'+2
twoway (line total_deaths_occ_shift date, lc(cranberry) lw(medthick)) ///
	(line total_deaths_rep_shift date, lc(cranberry%20) lw(medthick)) if mex==0 & date>date("31-Mar-2020","DMY"), ///
	xti(" ") yti("Total cumulative deaths", height(5)) ///
	xscale(r(`themin' `maxgr')) xlab(`themin'(14)`themax') ///
	yscale (r(0 30000)) ylab(0(5000)30000) ///
	legend(order(1 "Occurred" 2 "Reported") ring(0) position(10) bmargin(small) region(lwidth(none) fc(none)) col(1))  
graph export ${covid}stata_code/graphs/england_cum_deaths.png, replace
	
	
/* ======================
		mexico
====================== */

qui sum date if mex==1 & date>date("19-Apr-2020","DMY"), det
local themin=r(min)
local themax=r(max)
local maxgr=`themax'+2
twoway (bar deaths_occ date, lc(black) lw(medthick) fc(none)) ///
	(bar deaths_rep date, lc(black%20) fc(black%20)) if mex==1 & date>date("19-Apr-2020","DMY"), ///
	xti(" ") yti("Total deaths", height(5)) ///
	xscale(r(`themin' `maxgr')) xlab(`themin'(7)`themax') ///
	yscale (r(0 1000)) ylab(0(200)1000) ///
	legend(order(1 "Occurred" 2 "Reported") ring(0) position(10) bmargin(small) region(lwidth(none) fc(none)) col(1))  
graph export ${covid}stata_code/graphs/mexico_daily_deaths.png, replace 

qui sum date if mex==1 & date>date("19-Apr-2020","DMY"), det
local themin=r(min)
local themax=r(max)
local maxgr=`themax'+2
twoway (line total_deaths_occ_shift date, lc(black) lw(medthick)) ///
	(line total_deaths_rep_shift date, lc(black%20) lw(medthick))  if mex==1 & date>date("19-Apr-2020","DMY"), ///
	xti(" ") yti("Total cumulative deaths", height(5)) ///
	xscale(r(`themin' `maxgr')) xlab(`themin'(7)`themax') ///
	yscale (r(0 14000)) ylab(0(2000)14000) ///
	legend(order(1 "Occurred" 2 "Reported") ring(0) position(10) bmargin(small) region(lwidth(none) fc(none)) col(1))  
graph export ${covid}stata_code/graphs/mexico_cum_deaths.png, replace 
