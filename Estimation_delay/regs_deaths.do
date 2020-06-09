/*
		this do-file runs regressions for mexico and england 
		
*/


clear 
cap log close 
set scheme s1color
set seed 1010
grstyle clear

grstyle init
grstyle anglestyle vertical_tick horizontal
grstyle color p1 cranberry
grstyle color p2 black 

*set directory here:
global covid ""



/* =======================================
			put them together
======================================= */

use ${covid}stata_code/outdata/mx_covid_deaths_mun.dta, clear
gen mex=1
append using ${covid}stata_code/outdata/eng_covid_deaths_trust.dta
replace mex=0 if mex==.

*get rid of data reported on april 19 for mexico since we don't know when exactly it was reported
drop if date_death_rep==date("19apr2020","DMY") & mex==1

*get rid of deaths from before the reports
forval x=0/1 {
	sum date_death_rep if mex==`x', det
	local k=r(min)
	drop if date_death<`k' & mex==`x'
}

*fix geo for england 
replace geo=trust if geo==""

*get a numeric geo 
encode geo, gen(geo_num)

save ${covid}stata_code/temp/mx_eng_covid.dta, replace



/* =======================================
			program for regressions
======================================= */

cap prog drop regs 
prog def regs 

	syntax, days(numlist)
	
	use ${covid}stata_code/temp/mx_eng_covid.dta, clear 
	
	*get rid of the last k days and delays above k days 
	forval x=0/1 {
		qui sum date_death_rep if mex==`x', det
		local k=r(max)-`days'
		drop if date_death>`k' & mex==`x'
		drop if delay>`days' & mex==`x'
	}

	*calculate total deaths that occurred on a given date
	bysort geo date_death: egen total_deaths=total(deaths) 

	*calculate the average delay of reporting 
	gen aux=(delay*deaths)/total_deaths
	bysort geo date_death: egen avg_delay=total(aux)
	drop aux

	*keep one observation per location-date of death 
	bysort geo date_death: gen aux=_n
	keep if aux==1
	drop aux 
	
	*get variables for first and second half of data
	forval x=0/1 {
		sum date_death if mex==`x', det
		local med=r(p50)
		gen later`x'=date_death>=`med'
	}

	*do descriptive stats and export as a log file 
	log using ${covid}stata_code/output/desc_stats_`days'days.log, replace 

		/* =======================================
				DESCRIPTIVE STATS FOR 
				AVERAGE DELAYS: `days' days
		======================================= */
		
		ttest avg_delay, by(mex)
		ttest avg_delay if mex==0, by(later0)
		ttest avg_delay if mex==1, by(later1)

	log close 
	drop later0 later1 

	*histograms for average delay 
	twoway (histogram avg_delay if mex==0, start(0) w(1) lc(cranberry) lw(medthick) fc(none)) ///
		(histogram avg_delay if mex==1, start(0) w(1) color(black%20)), ///
		xti("Average reporting delay (days)", height(5)) xlab(0(2)`days') ///
		yscale(r(0 0.5)) ylab(0(0.1)0.5) ///
		legend(order(1 "England (NHS)" 2 "Mexico") ring(0) position(12) bmargin(small) region(lwidth(none) fc(none)) col(1)) 
	graph export ${covid}stata_code/graphs/hist_delay_`days'days.png, replace
	
	sum date_death if mex==0, det
	local half=r(p50)
	twoway (histogram avg_delay if mex==0 & date_death<=`half', start(0) w(1) lc(cranberry) lw(medthick) fc(none)) ///
		(histogram avg_delay if mex==0 & date_death>`half', start(0) w(1) color(cranberry%20)), ///
		xti("Average reporting delay (days)", height(5)) xlab(0(2)`days') ///
		yscale(r(0 0.5)) ylab(0(0.1)0.5) ///
		legend(order(1 "First half of available data" 2 "Second half of available data") ring(0) position(12) bmargin(small) region(lwidth(none) fc(none)) col(1)) 
	graph export ${covid}stata_code/graphs/hist_delay_england_`days'days.png, replace
		
	sum date_death if mex==1, det
	local half=r(p50)
	twoway (histogram avg_delay if mex==1 & date_death<=`half', start(0) w(1) lc(black) lw(medthick) fc(none)) ///
		(histogram avg_delay if mex==1 & date_death>`half', start(0) w(1) color(black%20)), ///
		xti("Average reporting delay (days)", height(5)) xlab(0(2)`days') ///
		yscale(r(0 0.5)) ylab(0(0.1)0.5) ///
		legend(order(1 "First half of available data" 2 "Second half of available data") ring(0) position(12) bmargin(small) region(lwidth(none) fc(none)) col(1)) 
	graph export ${covid}stata_code/graphs/hist_delay_mexico_`days'days.png, replace
	
	*calculate negative log of average delay plus one
	gen log_delay=log(1+avg_delay)

	* get dummies for deaths by quartile of the overall distribution 
	gen d1=total_deaths==1
	gen d2=total_deaths==2
	gen d3=total_deaths==3 | total_deaths==4 | total_deaths==5
	gen d4=total_deaths>=6
	
	*get dummies for dates
	tab date_death, gen(dd_)
	rename dd_20 ddomit_20
	
	*regression for mexico 
	reg log_delay d2 d3 d4 d1 dd_* i.geo_num ddomit if mex==1, cluster(geo)
	estimates store regs1
	
	*get results 
	{
	preserve
	
		parmest, fast
		
		*get the geo identifiers
		fvregen
		gen unitfe=geo_num!=.
		
		*get the date
		split parm, p(_)
		destring parm2, replace force
		drop if parm2<20
		local k=date("1-apr-2020","DMY")
		gen date=parm2+`k'-1
		format date %tdMonDD
		sort date
		gen timefe=date!=.
		
		*get rid of non fe
		drop if unitfe==0 & timefe==0
		
		*get ordered effects for units 
		sort timefe estimate
		gen n=_n 

		*normalize median to zero 
		sum estimate if unitfe==1, det
		local shift=r(p50)
		replace estimate=estimate-`shift' if unitfe==1
		replace max95=max95-`shift' if unitfe==1
		replace min95=min95-`shift' if unitfe==1
		
		gen mex=1
		
		keep mex unitfe n timefe date estimate min95 max95 parm stderr
		order mex unitfe n timefe date estimate min95 max95 parm stderr
		
		save ${covid}stata_code/temp/mex.dta, replace
		
	restore 
	}
	
	
	*regression for england 
	reg log_delay d2 d3 d4 d1 dd_* i.geo_num ddomit if mex==0, cluster(geo)
	estimates store regs0
	
	*get results 
	{
	preserve
	
		parmest, fast
		
		*get the geo identifiers
		fvregen
		gen unitfe=geo_num!=.
		
		*get the date
		split parm, p(_)
		destring parm2, replace force
		local k=date("1-apr-2020","DMY")
		gen date=parm2+`k'-1
		format date %tdMonDD
		sort date
		gen timefe=date!=.
		
		*get rid of non fe
		drop if unitfe==0 & timefe==0
		
		*get ordered effects for units 
		sort timefe estimate
		gen n=_n 

		*normalize median to zero 
		sum estimate if unitfe==1, det
		local shift=r(p50)
		replace estimate=estimate-`shift' if unitfe==1
		replace max95=max95-`shift' if unitfe==1
		replace min95=min95-`shift' if unitfe==1
		
		gen mex=0
		
		keep mex unitfe n timefe date estimate min95 max95 parm stderr
		order mex unitfe n timefe date estimate min95 max95 parm stderr
		
		save ${covid}stata_code/temp/eng.dta, replace
		
	restore 
	}
	
	*plot results for deaths 
	coefplot (regs0, mc(cranberry)) (regs1, mc(black) offset(0.1)), ///
		vertical omitted keep(d1 d2 d3 d4) order(d1 d2 d3 d4) ///
		coeflabels(d1="1 death" d2="2 deaths" d3="3-5 deaths" d4="6+ deaths") ///
		cismooth(lw(3 3) n(20) i(0 60)) ///
		legend(order(21 "England (NHS)" 42 "Mexico") ring(0) position(12) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
		yti("Estimated shift in log average delay", height(5)) 
	graph export ${covid}stata_code/graphs/regs_deaths_dummies_`days'days.png, replace
	
	*plot fixed effects 
	use ${covid}stata_code/temp/mex.dta, clear
	append using ${covid}stata_code/temp/eng.dta 
		
	*get normalized axis 
	sum n if mex==1 & unitfe==1, det
	local mx=r(max)
	sum n if mex==0 & unitfe==1, det
	local en=r(max)
	gen n2=(n-2)/(`mx'-2) if mex==1 & unitfe==1
	replace n2=(n-2)/(`en'-2) if mex==0 & unitfe==1
	
	*location fixed effects 
	twoway (rarea max95 min95 n2 if mex==1, lc(black%20) fc(black%20)) ///
		(rarea max95 min95 n2 if mex==0, lc(cranberry%20) fc(cranberry%20)) ///
		(line estimate n2 if mex==1, lc(black) lp(dash)) ///
		(line estimate n2 if mex==0, lc(cranberry)) if unitfe==1, ///
		xlabel(none) xtick(none) xti("Reporting units", height(5)) ///
		legend(order(4 "England (NHS)" 3 "Mexico") ring(0) position(12) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
		yscale(r(-2 2)) ylab(-2(1)2) yti("Predicted shift in log average delay", height(5))  
	graph export ${covid}stata_code/graphs/regs_unitfe_`days'days.png, replace

	*date fixed effects 
	local overlapper=date("20-Apr-2020","DMY")
	sort date 
	qui sum date if timefe==1, det
	local themin=`overlapper'-14 
	local themax=r(max)
	local maxgr=`themax'+2
	twoway (rarea max95 min95 date if mex==1, lc(black%20) fc(black%20)) ///
		(rarea max95 min95 date if mex==0, lc(cranberry%20) fc(cranberry%20)) ///
		(line estimate date if mex==1, lc(black) lp(dash)) ///
		(line estimate date if mex==0, lc(cranberry))  if timefe==1, ///
		xscale(r(`themin' `maxgr')) xlab(`themin'(14)`themax') ///
		xti(" ", height(5)) ///
		legend(order(4 "England (NHS)" 3 "Mexico") ring(0) position(12) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
		yscale(r(-.5 1.5)) ylab(-.5(.5)1.5) yti("Predicted shift in log average delay", height(5))  
	graph export ${covid}stata_code/graphs/regs_timefe_`days'days.png, replace

	rm ${covid}stata_code/temp/mex.dta
	rm ${covid}stata_code/temp/eng.dta
	
end 



/* =======================================
			run program 
======================================= */

foreach j of numlist 14 21 {
	regs, days(`j') 
}

grstyle clear 	
