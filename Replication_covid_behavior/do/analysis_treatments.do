/*=================================================================


This do-file analyzes the effects of the treatment in the survey 

			
==================================================================*/

clear
set more off

/*---------------------------
these commands indicate
some of the styling formats
for the figures
-----------------------------*/
set scheme s1color
grstyle clear
grstyle init
*grstyle anglestyle vertical_tick horizontal
grstyle color p1 black
grstyle color p2 black


*this indicates the path for the folder where the file "surveydata" is
global covid "C:/Replication_covid_behavior/"

*this indicates the path for the folder where the resulting graphs and tables will be stored
global covidout "C:/Replication_covid_behavior/figures_tables/"



*calling the dataset
use ${covid}raw/surveydata.dta, clear


/* ++++++++++++++++++++++++++++++++++++++++++
			0. Data prep
++++++++++++++++++++++++++++++++++++++++++ */

*label treatment
label variable treat "Information by date occurred"

*controls
global controls "female aged18_22 aged23_29 aged30_49 aged50_plus works school workschool other_occ cdmx apartment noyard withyard hhm_onetwo hhm_two hhm_three hhm_fourp hhm_over70 hhm_60_70 hhm_50_60 sick_nothing sick_self"

*get date of survey
split timestamp
gen date_survey=date(timestamp1,"YMD")
format date_survey %tdMon/DD
drop timestamp1 timestamp2
order id timestamp date_survey

*histogram with dates of survey 
histogram date_survey, discret color(blue) fcolor(none) xti(" ") xscale(r(22063 22074)) xlab(22063(1)22074, angle(30))
graph export ${covidout}hist_survey_date.pdf, replace

*define low prior group based on cases 
gen low_prior=preg28<=2

*define complement and full sample
gen high_prior=1-low_prior
gen full_samp=1

*definne low prior group bases on deaths 
gen low_prior_deaths=preg29<=2

*get indicators for high risk of contagion 
gen hr1=preg44<=1
gen hr4=preg46<=1
label variable hr1 "High risk of contagion at gathering with 100 people next week"
label variable hr4 "High risk of contagion at gathering with 100 people in 4 weeks"

*get indicators for high perceived toll 
gen mt5c=preg36>3
gen mt5d=preg39>3
label variable mt5c "More than 500K predicted total cases"
label variable mt5d "More than 50K predicted total deaths"

*get indicators for high movement out of the house 
gen mdo1=daysout1>2
label variable mdo1 "Will leave house three or more times next week"
gen mdo4=daysout4>2
label variable mdo4 "Will leave house three or more times in four weeks"

*get indicators for times left the house last week 
tab preg31, gen(out_last_wk_)

*indicators for each category of the outcome variables 
tab p44v, gen(pc44)
tab p46v, gen(pc46)
tab preg36b, gen(pc36)
tab preg39b, gen(pc39)
tab preg43, gen(pcc43)
tab preg45, gen(pcc45)

*fix some labels 
label variable other_occ "Other occupation/employment status"
label variable hhm_over70 "Has HH members over 70 years old"
label variable hhm_60_70 "Has HH members 60-70 years old"
label variable hhm_50_60 "Has HH members 50-60 years old"
label variable sick_nothing "Does not seek healthcare when sick"


/* ++++++++++++++++++++++++++++++++++++++++++
			1. Balance tables
++++++++++++++++++++++++++++++++++++++++++ */

balancetable treat $controls using ${covidout}balance_full.tex, varlabels ///
	ctitles("Deaths by date reported" "Deaths by date occurred" "Difference in means") replace

balancetable treat $controls if low_prior==1 using ${covidout}balance_low.tex, varlabels ///
	ctitles("Deaths by date reported" "Deaths by date occurred" "Difference in means") replace

balancetable treat $controls if low_prior==0 using ${covidout}balance_high.tex, varlabels ///
	ctitles("Deaths by date reported" "Deaths by date occurred" "Difference in means") replace

	
	
/* ++++++++++++++++++++++++++++++++++++++++++
			2. Histograms 
++++++++++++++++++++++++++++++++++++++++++ */

***		questions on priors	***

histogram preg28, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8)  ///
	start(-0.5) yscale(r(0 50)) ylab(0(10)50) addlabop(yvarf(%9.1f)) xscale(r(-0.5 6.5)) ///
	xla(0 "<10K" 1 `""10K-" "25K""' 2 `""25K-" "50K""' 3 `""50K-" "75K""' 4 `""75K-" "100K""' 5 `""100K-" "150K""' 6 ">150K") ///
	xtitle("Prior of total COVID-19 cases as of May 20", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}prior_cases_may20.pdf, replace

histogram preg29, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8)  ///
	start(-0.5) yscale(r(0 50)) ylab(0(10)50) addlabop(yvarf(%9.1f)) xscale(r(-0.5 6.5)) ///
	xla(0 "<1K" 1 `""1K-" "2.5K""' 2 `""2.5K-" "5K""' 3 `""5K-" "7.5K""' 4 `""7.5K-" "10K""' 5 `""10K-" "15K""' 6 ">15K") ///
	xtitle("Prior of total COVID-19 deaths as of May 20", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}prior_deaths_may20.pdf, replace


***		outcomes all participants	***

histogram p44v, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)  yscale(r(0 70)) ylab(0(10)70) xscale(r(-0.5 4)) ///
	xla(0 "Low" 1 `""Moderately" "high""' 2 "High" 3 `""Extremely" "high""') ///
	xtitle("Risk of contagion at event with 100 people: {bf:next week}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}risk_1w.pdf, replace

histogram p46v, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)  yscale(r(0 70)) ylab(0(10)70) xscale(r(-0.5 4))  ///
	xla(0 "Low" 1 `""Moderately" "high""' 2 "High" 3 `""Extremely" "high""') ///
	xtitle("Risk of contagion at event with 100 people: {bf:in 4 weeks}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}risk_4w.pdf, replace

histogram preg36b, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8) start(0.5)  ///
	yscale(r(0 40)) ylab(0(10)40) xscale(r(0.5 7)) ///
	xla(1 2 `""150K-" "250K""' 3 `""250K-" "500K""' 4 5 6, valuelabel) ///
	xtitle("Beliefs about total COVID-19 cases", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}beliefs_cases.pdf, replace
	
histogram preg39b, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8) start(0.5)  ///
	yscale(r(0 40)) ylab(0(10)40) xscale(r(0.5 7)) ///
	xla(1/3 4 `""50K-" "100K""' 5 `""100K-" "200K""' 6, valuelabel) ///
	xtitle("Beliefs about total COVID-19 deaths", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}beliefs_deaths.pdf, replace

histogram preg43, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8) start(-0.5)  ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(-0.5 5)) ///
	xla(0 "None" 1 "One" 2 "Two" 3 `""Three" "or four""' 4 `""More than" "four""') ///
	xtitle("Number of times will leave home: {bf:next week}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}daysout_1w.pdf, replace
  
histogram preg45, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8)  start(-0.5)  ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(-0.5 5)) ///
	xla(0 "None" 1 "One" 2 "Two" 3 `""Three" "or four""' 4 `""More than" "four""') ///
	xtitle("Number of times will leave home: {bf:in 4 weeks}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}daysout_4w.pdf, replace


***		outcomes by informational treatments	***

twoway (histogram p44v if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)) (histogram p44v if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(-0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 70)) ylab(0(10)70) xscale(r(-0.5 4)) ///
	xla(0 "Low" 1 `""Moderately" "high""' 2 "High" 3 `""Extremely" "high""')  ///
	xtitle("Risk of contagion at event with 100 people: {bf:next week}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_risk_1w_treat.pdf, replace

twoway (histogram p46v if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)) (histogram p46v if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(-0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 70)) ylab(0(10)70) xscale(r(-0.5 4)) ///
	xla(0 "Low" 1 `""Moderately" "high""' 2 "High" 3 `""Extremely" "high""')  ///
	xtitle("Risk of contagion at event with 100 people: {bf:in 4 weeks}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_risk_4w_treat.pdf, replace

twoway (histogram preg36b if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(0.5)) (histogram preg36b if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 40)) ylab(0(10)40) xscale(r(0.5 7)) ///
	xla(1 2 `""150K-" "250K""' 3 `""250K-" "500K""' 4 5 6, valuelabel) /// 
	xtitle("Beliefs about total COVID-19 cases", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_cases_treat.pdf, replace

twoway (histogram preg39b if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(0.5)) (histogram preg39b if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 40)) ylab(0(10)40) xscale(r(0.5 7)) ///
	xla(1/3 4 `""50K-" "100K""' 5 `""100K-" "200K""' 6, valuelabel) ///
	xtitle("Beliefs about total COVID-19 deaths", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_deaths_treat.pdf, replace

twoway (histogram preg43 if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)) (histogram preg43 if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(-0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(-0.5 5)) ///
	xla(0 "None" 1 "One" 2 "Two" 3 `""Three" "or four""' 4 `""More than" "four""') ///
	xtitle("Number of times will leave home: {bf:next week}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_out_1w_treat.pdf, replace

twoway (histogram preg45 if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)) (histogram preg45 if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(-0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(-0.5 5)) ///
	xla(0 "None" 1 "One" 2 "Two" 3 `""Three" "or four""' 4 `""More than" "four""') ///
	xtitle("Number of times will leave home: {bf:in 4 weeks}", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_out_4w_treat.pdf, replace


/* ++++++++++++++++++++++++++++++++++++++++++
			3. Main regressions
++++++++++++++++++++++++++++++++++++++++++ */

*program
cap prog drop mainregs
prog def mainregs

	syntax, outcome(namelist) controls(string) sample(namelist) saving(string) ap(string)
	
	reg `outcome' treat `controls' if `sample'==1, robust
	
	qui sum `outcome' if e(sample)==1, det
	local mdv=r(mean)

	outreg2 treat using ${covidout}`saving', keep(treat) nocons tex sdec(3) adec(2) ///
		addstat(Mean dependent variable, `mdv') addtext("Sample", `sample') label nonotes `ap'
	
end

*run program 
mainregs, outcome(hr1) controls($controls) sample(full_samp) saving(regs_risk) ap(replace)
mainregs, outcome(hr1) controls($controls) sample(low_prior) saving(regs_risk) ap(append)
mainregs, outcome(hr1) controls($controls) sample(high_prior) saving(regs_risk) ap(append)
mainregs, outcome(hr4) controls($controls) sample(full_samp) saving(regs_risk) ap(append)
mainregs, outcome(hr4) controls($controls) sample(low_prior) saving(regs_risk) ap(append)
mainregs, outcome(hr4) controls($controls) sample(high_prior) saving(regs_risk) ap(append)

mainregs, outcome(mt5c) controls($controls) sample(full_samp) saving(regs_toll) ap(replace)
mainregs, outcome(mt5c) controls($controls) sample(low_prior) saving(regs_toll) ap(append)
mainregs, outcome(mt5c) controls($controls) sample(high_prior) saving(regs_toll) ap(append)
mainregs, outcome(mt5d) controls($controls) sample(full_samp) saving(regs_toll) ap(append)
mainregs, outcome(mt5d) controls($controls) sample(low_prior) saving(regs_toll) ap(append)
mainregs, outcome(mt5d) controls($controls) sample(high_prior) saving(regs_toll) ap(append)

mainregs, outcome(mdo1) controls($controls) sample(full_samp) saving(regs_behavior) ap(replace)
mainregs, outcome(mdo1) controls($controls) sample(low_prior) saving(regs_behavior) ap(append)
mainregs, outcome(mdo1) controls($controls) sample(high_prior) saving(regs_behavior) ap(append)
mainregs, outcome(mdo4) controls($controls) sample(full_samp) saving(regs_behavior) ap(append)
mainregs, outcome(mdo4) controls($controls) sample(low_prior) saving(regs_behavior) ap(append)
mainregs, outcome(mdo4) controls($controls) sample(high_prior) saving(regs_behavior) ap(append)


/* ++++++++++++++++++++++++++++++++++++++++++
			4. Regressions by category 
++++++++++++++++++++++++++++++++++++++++++ */

*program 
cap prog drop regscat
prog def regscat 

	syntax, sample(namelist) color(string) marker(string) ymin(numlist) ymax(numlist) saving(string)

	
	* 1. risk contagion in 1 week 
	
	forval i=1(1)4 {
		reg pc44`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg44=`i'
			save ${covid}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)4 {
			append using ${covid}\cat_`i'.dta
		}	
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg44 , mc(`color') m(`marker')) (rcap max90 min90 preg44, lc(`color')) (rspike max95 min95 preg44, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 4.5)) ///
			xla(1 "Low" 2 `""Moderately" "high""' 3 "High" 4 `""Extremely" "high""') ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Risk of contagion at event with 100 people: {bf:next week}", height(5))
		graph export ${covidout}diff_risk_1w_treat_`saving'.pdf, replace
	restore

	forval i=1(1)4 {
		rm ${covid}\cat_`i'.dta
	}

	* 2. risk contagion in 4 weeks
	
	forval i=1(1)4 {
		reg pc46`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg46=`i'
			save ${covid}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)4 {
			append using ${covid}\cat_`i'.dta
		}
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg46 , mc(`color') m(`marker')) (rcap max90 min90 preg46, lc(`color')) (rspike max95 min95 preg46, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 4.5)) ///
			xla(1 "Low" 2 `""Moderately" "high""' 3 "High" 4 `""Extremely" "high""') ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Risk of contagion at event with 100 people: {bf:in 4 weeks}", height(5))
		graph export ${covidout}diff_risk_4w_treat_`saving'.pdf, replace
	restore

	forval i=1(1)4 {
		rm ${covid}\cat_`i'.dta
	}

	* 3. total cases 
	
	forval i=1(1)6 {
		reg pc36`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg36=`i'
			save ${covid}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)6 {
			append using ${covid}\cat_`i'.dta
		}
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg36 , mc(`color') m(`marker')) (rcap max90 min90 preg36, lc(`color')) (rspike max95 min95 preg36, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 6.5)) ///
			xlab(1 "<150K" 2 `""150K-" "250K""' 3 `""250K-" "500K""' 4 "500K-1M" 5 "1M-2M" 6 ">2M") ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Beliefs about total number of COVID-19 cases", height(5))
		graph export ${covidout}diff_cases_treat_`saving'.pdf, replace
	restore

	forval i=1(1)6 {
		rm ${covid}\cat_`i'.dta
	}
	
	* 4. total deaths
	
	forval i=1(1)6 {
		reg pc39`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg39=`i'
			save ${covid}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)6 {
			append using ${covid}\cat_`i'.dta
		}
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg39 , mc(`color') m(`marker')) (rcap max90 min90 preg39, lc(`color')) (rspike max95 min95 preg39, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 6.5)) ///
			xlab(1 "<15K" 2 "15K-25K" 3 "25K-50K" 4 `""50K-" "100K""' 5 `""100K-" "200K""' 6 ">200K") ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Beliefs about total number of COVID-19 deaths", height(5))
		graph export ${covidout}diff_deaths_treat_`saving'.pdf, replace
	restore

	forval i=1(1)6 {
		rm ${covid}\cat_`i'.dta
	}
	
	* 5. behavior in 1 week 
	
	forval i=1(1)5 {
		reg pcc43`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg43=`i'
			save ${covid}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)5 {
			append using ${covid}\cat_`i'.dta
		}
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg43 , mc(`color') m(`marker')) (rcap max90 min90 preg43, lc(`color')) (rspike max95 min95 preg43, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 5.5)) ///
			xlab(1 "None" 2 "One" 3 "Two" 4 `""Three" "or four""' 5 `""More than" "four""') ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Number of times will leave home: {bf:next week}", height(5))
		graph export ${covidout}diff_daysout1_treat_`saving'.pdf, replace
	restore

	forval i=1(1)5 {
		rm ${covid}\cat_`i'.dta
	}

	* 6. behavior in 4 weeks 
	
	forval i=1(1)5 {
		reg pcc45`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg45=`i'
			save ${covid}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)5 {
			append using ${covid}\cat_`i'.dta
		}	
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr	
		twoway (scatter estimate preg45 , mc(`color') m(`marker')) (rcap max90 min90 preg45, lc(`color')) (rspike max95 min95 preg45, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 5.5)) ///
			xlab(1 "None" 2 "One" 3 "Two" 4 `""Three" "or four""' 5 `""More than" "four""') ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Number of times will leave home: {bf:in 4 weeks}", height(5))
		graph export ${covidout}diff_daysout4_treat_`saving'.pdf, replace
	restore

	forval i=1(1)5 {
		rm ${covid}\cat_`i'.dta
	}
	
end 

*run program
regscat,  sample(full_samp) color(blue) marker(O) ymin(-0.15) ymax(0.1) saving(full)
regscat,  sample(low_prior) color(green) marker(Dh) ymin(-0.2) ymax(0.2) saving(low)
regscat,  sample(high_prior) color(purple) marker(Sh) ymin(-0.2) ymax(0.2) saving(high)


grstyle clear
