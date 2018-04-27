// Creates dataset of CPS turnout by race-year 

// Set working directory to location of replication folder
cd ""
cap log close
log using "cpsturnout.log", replace

clear all
set more off

import excel using "Nov2014excel.xls"

drop in 1/3
drop in 574/581
rename A state
rename B racehispanic
drop O-U
rename C totalpop
rename D citizenpop
rename E totalreg
rename F percenttotalreg
rename G MOEpercenttotalreg
rename H percentcitizensreg
rename I MOEpercentcitizensreg
rename J totalnumvoted
rename K percenttotalvoted
rename L MOEpercenttotalvoted
rename M percentcitizensvoted
rename N MOEpercentcitizensvoted

drop in 1

replace racehispanic = subinstr(racehispanic, ".", "", 1)

save "2014turnoutfile.dta", replace

import excel using "Nov2012excel.xls", clear

drop in 1/3
drop in 574/581
rename A state
rename B racehispanic
drop O-U
rename C totalpop
rename D citizenpop
rename E totalreg
rename F percenttotalreg
rename G MOEpercenttotalreg
rename H percentcitizensreg
rename I MOEpercentcitizensreg
rename J totalnumvoted
rename K percenttotalvoted
rename L MOEpercenttotalvoted
rename M percentcitizensvoted
rename N MOEpercentcitizensvoted

drop in 1

replace racehispanic = subinstr(racehispanic, ".", "", 1)

save "2012turnoutfile.dta", replace

import excel using "Nov2010excel.xls", clear

drop in 1/2
drop in 574/583
drop O-EL
rename A state
rename B racehispanic
rename C totalpop
rename D citizenpop
rename E totalreg
rename F percenttotalreg
rename G MOEpercenttotalreg
rename H percentcitizensreg
rename I MOEpercentcitizensreg
rename J totalnumvoted
rename K percenttotalvoted
rename L MOEpercenttotalvoted
rename M percentcitizensvoted
rename N MOEpercentcitizensvoted

drop in 1
save "2010turnoutfile.dta", replace

import excel using "Nov2008excel.xls", clear

drop in 1/5
drop in 626/725
drop N-S
rename A racehispanic
rename B totalpop
rename C citizenpop
rename D totalreg
rename E percenttotalreg
rename F MOEpercenttotalreg
rename G percentcitizensreg
rename H MOEpercentcitizensreg
rename I totalnumvoted
rename J percenttotalvoted
rename K MOEpercenttotalvoted
rename L percentcitizensvoted
rename M MOEpercentcitizensvoted

drop in 1

replace racehispanic = subinstr(racehispanic, ".", "", 2)
generate state = racehispanic[_n-1] if racehispanic == "Total"
order state

replace state = "US" in 2
drop if totalpop == "" & citizenpop == ""

save "2008turnoutfile.dta", replace


//appending turnout files, formatting properly and filtering irrelevant data

local files "2010turnoutfile.dta 2012turnoutfile.dta 2014turnoutfile.dta"

foreach file of local files{
	append using "`file'"
}

replace state = trim(state)
replace racehispanic = trim(racehispanic)
replace state = "UNITED STATES" if state == "US" | state == "All"
replace state = state[_n-1] if state == ""

gen year = 2008 in 1/572
replace year = 2010 in 573/1144
replace year = 2012 in 1145/1716
replace year = 2014 in 1717/2288

keep state racehispanic year percentcitizensvoted
keep if racehispanic == "White non-Hispanic alone" | racehispanic == "Hispanic (of any race)" | racehispanic == "Black alone or in combination"

gen race = "wht" if racehispanic == "White non-Hispanic alone"
replace race = "his" if racehispanic == "Hispanic (of any race)"
replace race = "blk" if racehispanic == "Black alone or in combination"
drop racehispanic

rename percentcitizensvoted y

reshape wide y, i(state race) j(year)
reshape wide y2008 y2010 y2012 y2014, i(state) j(race) string
drop if regexm(state, "UNITED STATES")

save "cpsturnout.dta", replace
rm "2008turnoutfile.dta"
rm "2010turnoutfile.dta" 
rm "2012turnoutfile.dta" 
rm "2014turnoutfile.dta"

log close
