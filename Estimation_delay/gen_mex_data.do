/*
		this do-file takes the raw data from Mexico and generates a clean dataset
			with individual records as reported by the government 
			(data starts on April 19 and we should only consider data starting on the 20th)
			
		generated file:		clean_mx_full.dta
		
*/


clear 
set scheme s1color
set seed 1010

*set directory here:
global covid ""

local last_file=date("June-07-2020","MDY")


 
/* ++++++++++++++++++++++++++++++++++++++++
			useful locals for later
++++++++++++++++++++++++++++++++++++++++ */

*total files available until current date
local totfiles=`last_file'-date("Apr-19-2020","MDY")+1

*last day on file for the month of JUNE
local lastday=day(`last_file')


/* ++++++++++++++++++++++++++++++++++++++++
			convert csv to stata
++++++++++++++++++++++++++++++++++++++++ */

cap prog drop to_dta 
prog define to_dta 

	syntax, m(numlist) d(numlist)

	{
	if `m'==4 {
		local mes="abril"
	}
	else if `m'==5 {
		local mes="mayo"
	}
	else if `m'==6 {
		local mes="junio"
	}
	else if `m'==7 {
		local mes="julio"
	}
	else if `m'==8 {
		local mes="agosto"
	}
	else if `m'==9 {
		local mes="septiembre"
	}
	else if `m'==10 {
		local mes="octubre"
	}
	else if `m'==11 {
		local mes="noviembre"
	}
	else if `m'==12 {
		local mes="diciembre"
	}
	}
	
	if `m'<10 {
		local m2="0`m'"
	}
	else if `m'>=10 {
		local m2="`m'"
	}	
	
	if `d'<10 {
		local d2="0`d'"
	}
	else if `d'>=10 {
		local d2="`d'"
	}
		
	import delimited ${covid}Mexico_data/_`d'_`mes'/20`m2'`d2'COVID19MEXICO.csv, clear

	rename *, lower
	
	{
	if `m'==4 {
		local counter=`d'-18 
	}
	else if `m'==5 {
		local counter=12+`d'
	}
	else if `m'==6 {
		local counter=43+`d'
	}
	else if `m'==7 {
		local counter=73+`d'
	}
	else if `m'==8 {
		local counter=104+`d'
	}
	else if `m'==9 {
		local counter=135+`d'
	}
	else if `m'==10 {
		local counter=165+`d'
	}
	else if `m'==11 {
		local counter=196+`d'
	}
	else if `m'==12 {
		local counter=226+`d'
	}
	}
	
	save ${covid}stata_code/temp/mx_`counter'.dta, replace
	
end 


/* ++++++++++++++++++++++++++++++++++++++++
			process each file
++++++++++++++++++++++++++++++++++++++++ */

cap prog drop process_file 
prog define process_file 

	syntax, ct(numlist)
	
	use ${covid}stata_code/temp/mx_`ct'.dta, clear 

	gen date_report=date(fecha_actualizacion,"YMD")
	format date_report %td
	label variable date_report "Reporting date"

	gen id= id_registro
	label variable id "Patient id"
	bysort id: gen n=_n
	tab n
	drop n

	gen surv_unit=1 if origen==1
	replace surv_unit=0 if origen==2
	label variable surv_unit "Patient from surveillance health unit" 

	gen inst_rc=sector==1
	gen inst_dif=sector==2
	gen inst_state=sector==3
	gen inst_imss=sector==4
	gen inst_imssop=sector==5
	gen inst_issste=sector==6
	gen inst_mun=sector==7
	gen inst_pmx=sector==8
	gen inst_priv=sector==9
	gen inst_def=sector==10
	gen inst_mar=sector==11
	gen inst_ssa=sector==12
	gen inst_univ=sector==13
	foreach x of varlist inst_* {
		replace `x'=. if sector>13
	}
	label variable inst_rc "Healthcare institution: Red Cross"
	label variable inst_dif "Healthcare institution: DIF"
	label variable inst_state "Healthcare institution: State services"
	label variable inst_imss "Healthcare institution: IMSS"
	label variable inst_imssop "Healthcare institution: IMSS-Oportunidades"
	label variable inst_issste "Healthcare institution: ISSSTE"
	label variable inst_mun "Healthcare institution: Municipality services"
	label variable inst_pmx "Healthcare institution: Pemex"
	label variable inst_priv "Healthcare institution: Private"
	label variable inst_def "Healthcare institution: Ministry of Defense"
	label variable inst_mar "Healthcare institution: Marines"
	label variable inst_ssa "Healthcare institution: Ministry of Health"
	label variable inst_univ "Healthcare institution: Universities" 

	gen state_hc= entidad_um
	replace state_hc=. if state_hc>32
	label variable state_hc "State of healthcare unit"

	gen female=1 if sexo==1
	replace female=0 if sexo==2
	label variable female "Patient is female"

	gen state_birth= entidad_nac
	replace state_birth=. if state_birth>32
	label variable state_birth "State of birth"

	gen state_res= entidad_res
	replace state_res=. if state_res>32
	label variable state_res "State of residence"

	cap destring municipio_res, replace force
	gen mun_res=municipio_res
	replace mun_res=. if mun_res==999
	label variable mun_res "Municipality of residence"

	gen inpatient=1 if tipo_paciente==2
	replace inpatient=0 if tipo_paciente==1
	label variable inpatient "Patient hospitalized (inpatient care)"

	gen icu=1 if uci==1
	replace icu=0 if uci==2
	label variable icu "Patient in ICU"

	gen date_care=date(fecha_ingreso,"YMD")
	format date_care %td
	label variable date_care "Date patient sought care at healthcare facility"

	gen date_sympt=date(fecha_sintomas,"YMD")
	format date_sympt %td
	label variable date_sympt "Date patient first had symptoms"

	gen date_death=date(fecha_def,"YMD")
	format date_death %td
	label variable date_death "Date of death"

	gen dead=date_death!=.
	label variable dead "Patient is dead"

	gen intubated=1 if intubado==1
	replace intubated=0 if intubado==2
	label variable intubated "Patient is intubated"

	gen pneum=1 if neumonia ==1
	replace pneum=0 if neumonia ==2
	label variable pneum "Patient has pneumonia"

	gen preg=1 if embarazo==1
	replace preg=0 if embarazo==2
	label variable preg "Patient is pregnant"

	gen diab=1 if diabetes==1
	replace diab=0 if diabetes==2
	label variable diab "Patient has diabetes"

	gen copd=1 if epoc==1
	replace copd=0 if epoc==2
	label variable copd "Patient has chronic obstructive pulmonary disease"

	gen asthma=1 if asma==1
	replace asthma=0 if asma==2
	label variable asthma "Patient has asthma"

	gen immun=1 if inmusupr==1
	replace immun=0 if inmusupr==2
	label variable immun "Patient is immunocompromised"

	gen hbp=1 if hipertension==1
	replace hbp=0 if hipertension==2
	label variable hbp "Patient has high blood pressure"

	gen cardio=1 if cardiovascular==1
	replace cardio=0 if cardiovascular==2
	label variable cardio "Patient has cardiopathy"

	gen obese=1 if obesidad==1
	replace obese=0 if obesidad==2
	label variable obese "Patient is obese"

	gen ckd=1 if renal_cronica==1
	replace ckd=0 if renal_cronica==2
	label variable ckd "Patient has chronic kidney disease"

	gen smoker=1 if tabaquismo==1
	replace smoker=0 if tabaquismo==2
	label variable smoker "Patient is a smoker"

	gen other_dis=1 if otra_com==1
	replace other_dis=0 if otra_com==2
	label variable other_dis "Patient has another disease"

	gen age=edad
	replace age=. if age>150
	label variable age "Patient's age'"

	gen mexican=nacionalidad==1
	label variable mexican "Patient is Mexican"

	gen indig=1 if habla_lengua_indig==1
	replace indig=0 if habla_lengua_indig==2
	label variable indig "Patient speaks indigeneous language"

	gen contact=1 if otro_caso==1
	replace contact=0 if otro_caso==2
	label variable contact "Patient had contact with another COVID patient"

	gen covid_pos=resultado==1
	label variable covid_pos "COVID lab test: positive"

	gen covid_neg=resultado==2
	label variable covid_neg "COVID lab test: negative"

	gen covid_pending=resultado==3
	label variable covid_pending "COVID lab test: pending"

	keep id date_report date_death date_care date_sympt dead covid_pos covid_neg covid_pending inpatient icu intubated pneum contact female age state_res mun_res state_birth indig mexican preg smoker diab obese copd asthma immun cardio hbp ckd other_dis state_hc inst_rc inst_dif inst_state inst_imss inst_imssop inst_issste inst_mun inst_pmx inst_priv inst_def inst_mar inst_ssa inst_univ surv_unit

	order id date_report date_death date_care date_sympt dead covid_pos covid_neg covid_pending inpatient icu intubated pneum contact female age state_res mun_res state_birth indig mexican preg smoker diab obese copd asthma immun cardio hbp ckd other_dis state_hc inst_rc inst_dif inst_state inst_imss inst_imssop inst_issste inst_mun inst_pmx inst_priv inst_def inst_mar inst_ssa inst_univ surv_unit

	save ${covid}stata_code/temp/clean_mx_`ct'.dta, replace
	
	rm ${covid}stata_code/temp/mx_`ct'.dta
	
end 



* ==============================================================================

/* ++++++++++++++++++++++++++++++++++++++++
			run all programs
++++++++++++++++++++++++++++++++++++++++ */

*run conversion program by month

	*april
forval k=19(1)30 {
	to_dta, m(4) d(`k')
}

	*may
forval k=1(1)31 {
	to_dta, m(5) d(`k')
}

	*june
forval k=1(1)`lastday' {
	to_dta, m(6) d(`k')
}


*run file processing program 
forval k=1(1)`totfiles' {
	process_file, ct(`k')
}


/* ++++++++++++++++++++++++++++++++++++++++
			put together and format
++++++++++++++++++++++++++++++++++++++++ */
  
use ${covid}stata_code/temp/clean_mx_1.dta, clear

forval k=2(1)`totfiles' {
		
	append using ${covid}stata_code/temp/clean_mx_`k'.dta
}

*take the mode of dates to account for typos 
foreach x of varlist date_death date_care date_sympt {
	rename `x' `x'_0
	bysort id: egen `x'=mode(`x'_0), maxmode
	format `x' %td
}
label variable date_sympt "Date patient first had symptoms"
label variable date_death "Date of death"
label variable date_care "Date patient first sought care at healthcare facility"

*get the first time they appeared
bysort id: egen date_first=min(date_report)
format date_first %td
label variable date_first "Date patient first appeared in dataset"

*get the date death was reported 
sort id date_report
gen aux=0 if date_death_0==.
replace aux=1 if date_death_0!=.
bysort id: gen aux2=aux-aux[_n-1]
gen aux3=date_report if aux2==1
bysort id: egen date_death_rep=min(aux3)  
replace date_death_rep=date_first if date_death!=. & date_death_rep==.
format date_death_rep %td
	*fix this for deaths reported from the start but then taken back and reported again
gen aux4=aux2==-1
gen aux5=aux2==1
sort id date_report
bysort id: gen aux6=sum(aux4)
bysort id: gen aux7=sum(aux5)
gen aux8=aux6==1 & aux7==0 if aux2==-1
bysort id: egen aux9=mean(aux8)
bysort id: egen aux10=min(date_report) if aux9==1
format aux10 %td
replace date_death_rep=aux10 if aux9==1
drop aux*
label variable date_death_rep "Date death was reported"

*indicator for ever having covid
bysort id: egen covid_conf=max(covid_pos)
label variable covid_conf "Patient has COVID, lab-confirmed"

*get the date patient was confirmed covid 
sort id date_report
bysort id: gen aux=sum(covid_pos)
gen aux2=date_report if aux==1
bysort id: egen date_covid=mean(aux2) 
format date_covid %td
label variable date_covid "Data patient confirmed COVID (reported)"
drop aux*

*indicator for dying 
gen ever_dead=date_death!=.
label variable ever_dead "Patient ends up dying"

*get the date patient was first hospitalized
sort id date_report
bysort id: gen aux=sum(inpatient)
gen aux2=date_report if aux==1
bysort id: egen date_hosp=mean(aux2) 
format date_hosp %td
label variable date_hosp "Data patient hospitalized (reported)"
drop aux*

*get the date patient was first put in the icu  
sort id date_report
bysort id: gen aux=sum(icu)
gen aux2=date_report if aux==1
bysort id: egen date_icu=mean(aux2) 
format date_icu %td
label variable date_icu "Data patient put in ICU (reported)"
drop aux*

*get the date patient was first put on a ventilator  
sort id date_report
bysort id: gen aux=sum(intubated)
gen aux2=date_report if aux==1
bysort id: egen date_vent=mean(aux2) 
format date_vent %td
label variable date_vent "Data patient put on a ventilator (reported)"
drop aux*

*indicator for hospitalized
gen ever_hosp=date_hosp!=.
label variable ever_hosp "Patient is ever hospitalized"

*indicator for icu
gen ever_icu=date_icu!=.
label variable ever_icu "Patient is ever put in the ICU"

*indicator for intubation 
gen ever_vent=date_vent!=.
label variable ever_vent "Patient is ever put on a ventilator"

*keep one observation (first one chronologically for the time-invariant variables)
sort id date_report
bysort id: gen n=_n
keep if n==1
drop n

keep id date_first date_death date_death_rep date_care date_sympt date_covid date_hosp date_icu date_vent covid_conf ever_dead ever_hosp ever_icu ever_vent female age state_res mun_res state_birth indig mexican contact pneum copd asthma cardio hbp immun obese diab ckd preg smoker other_dis state_hc inst_rc inst_dif inst_state inst_imss inst_imssop inst_issste inst_mun inst_pmx inst_priv inst_def inst_mar inst_ssa inst_univ surv_unit

order id date_first date_death date_death_rep date_care date_sympt date_covid date_hosp date_icu date_vent covid_conf ever_dead ever_hosp ever_icu ever_vent female age state_res mun_res state_birth indig mexican contact pneum copd asthma cardio hbp immun obese diab ckd preg smoker other_dis state_hc inst_rc inst_dif inst_state inst_imss inst_imssop inst_issste inst_mun inst_pmx inst_priv inst_def inst_mar inst_ssa inst_univ surv_unit

save ${covid}stata_code/outdata/clean_mx_full.dta, replace

forval k=1(1)`totfiles' {
	rm ${covid}stata_code/temp/clean_mx_`k'.dta
}
