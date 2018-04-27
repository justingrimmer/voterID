// Creates dataset of CCES primary election turnout by year
// Calculates turnout three ways: 
// 1) Missings dropped, conditioning on voteregpre, sample weights (Table 1)
// 2) Missings dropped, not conditioning on voteregpre, no sample weights (Figure 4)
// 3) Missings are non-voters, not conditioning on voteregorem, sample weights (Our preferred)

//Set working directory to location of replication folder
cd ""
cap log close
log using "ccesturnout_primary.log", replace

clear all
set more off

matrix turnout_missing = J(150, 4, -9)
matrix turnout_missing_noweights = J(150, 4, -9)
matrix turnout_nomissing = J(150, 4, -9)

//
// 1) Missings dropped, conditioning on voteregpre, sample weights (Table 1)
// 

use "jop final.dta", clear

drop if missing(voteprival) | voteregpre != 1

keep if mod(year, 2) == 0

keep state year voteprival weight stricty newstrict
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

local statecounter = 1
qui levelsof state, local(levels) 
foreach statename of local levels {
local yearcounter = 1
forvalues yearnum = 2008(2)2014 {
display "`statename'" "`yearcounter'"
cap regress voteprival [pweight = weight] if state == "`statename'" & year == `yearnum' , robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing[`statecounter', `yearcounter'] = _b[_cons] 
matrix turnout_missing[`statecounter' + 1, `yearcounter'] = _se[_cons]
matrix turnout_missing[`statecounter' + 2, `yearcounter'] = e(N)
}
local yearcounter = `yearcounter' + 1
}
local statecounter = `statecounter' + 3
}

//
// 2) Missings dropped, not conditioning on voteregpre, no sample weights (Figure 4)
// 

use "jop final.dta", clear

drop if missing(voteprival)

keep if mod(year, 2) == 0

keep state year voteprival weight stricty newstrict
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

local statecounter = 1
qui levelsof state, local(levels) 
foreach statename of local levels {
local yearcounter = 1
forvalues yearnum = 2008(2)2014 {
display "`statename'" "`yearcounter'"
cap regress voteprival if state == "`statename'" & year == `yearnum', robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing_noweights[`statecounter', `yearcounter'] = _b[_cons] 
matrix turnout_missing_noweights[`statecounter' + 1, `yearcounter'] = _se[_cons]
matrix turnout_missing_noweights[`statecounter' + 2, `yearcounter'] = e(N)
}
local yearcounter = `yearcounter' + 1
}
local statecounter = `statecounter' + 3
}

//
// 3) Missings are non-voters, not conditioning on voteregorem, sample weights (our preferred)
// 

use "jop final.dta", clear

replace voteprival = 0 if missing(voteprival)

keep if mod(year, 2) == 0

keep state year voteprival weight stricty newstrict
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

local statecounter = 1
qui levelsof state, local(levels) 
foreach statename of local levels {
local yearcounter = 1
forvalues yearnum = 2008(2)2014 {
display "`statename'" "`yearcounter'"
cap regress voteprival [pweight = weight] if state == "`statename'" & year == `yearnum', robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_nomissing[`statecounter', `yearcounter'] = _b[_cons] 
matrix turnout_nomissing[`statecounter' + 1, `yearcounter'] = _se[_cons]
matrix turnout_nomissing[`statecounter' + 2, `yearcounter'] = e(N)
}
local yearcounter = `yearcounter' + 1
}
local statecounter = `statecounter' + 3
}

clear
svmat turnout_missing 
svmat turnout_missing_noweights 
svmat turnout_nomissing

gen state =  ""
local statecounter = 1
foreach statename of local levels {
replace state = "`statename'" if _n == `statecounter'
local statecounter = `statecounter' + 3
}
replace state = state[_n-1] if missing(state)

forvalues i = 1(1)4 {
local j = 2008 + (`i' - 1)*2
rename turnout_missing`i' missing`j' 
rename turnout_missing_noweights`i' missing_noweights`j' 
rename turnout_nomissing`i' nomissing`j' 
}

gen row = mod((_n - 1), 3) + 1
reshape wide missing* nomissing*, i(state) j(row)

forvalues i = 2008(2)2014 {
rename missing`i'1 TURNOUTmissing`i'
rename missing`i'2 SEmissing`i'
rename missing`i'3 Nmissing`i'
rename missing_noweights`i'1 TURNOUTmissing_noweights`i'
rename missing_noweights`i'2 SEmissing_noweights`i'
rename missing_noweights`i'3 Nmissing_noweights`i'
rename nomissing`i'1 TURNOUTnomissing`i'
rename nomissing`i'2 SEnomissing`i'
rename nomissing`i'3 Nnomissing`i'
}

// Fix Virginia 2010 manually
replace TURNOUTmissing2010 = . if state == "Virginia"
replace TURNOUTmissing_noweights2010 = . if state == "Virginia"
replace TURNOUTnomissing2010 = . if state == "Virginia"
replace SEmissing2010 = . if state == "Virginia"
replace SEmissing_noweights2010 = . if state == "Virginia"
replace SEnomissing2010 = . if state == "Virginia"
replace Nmissing2010 = 0 if state == "Virginia"
replace Nmissing_noweights2010 = 0 if state == "Virginia"
replace Nnomissing2010 = 0 if state == "Virginia"

sort state
save "ccesaggregates_primary.dta", replace

log close

