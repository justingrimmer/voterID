// Creates dataset of CCES general election turnout by state by race by year
// Calculates turnout three ways: 
// 1) Missings dropped, conditioning on voteregpre, sample weights (Table 1)
// 2) Missings dropped, not conditioning on voteregpre, no sample weights (Figure 4)
// 3) Missings are non-voters, not conditioning on voteregorem, sample weights (our preferred)

// Set working directory to location of replication folder
cd ""
cap log close
log using "ccesturnoutbyrace.log", replace

clear all
set more off
set matsize 450

matrix turnout_missing = J(450, 5, -9)
matrix turnout_missing_noweights = J(450, 5, -9)
matrix turnout_nomissing = J(450, 5, -9)

//
// 1) Missings dropped, conditioning on voteregpre, sample weights (Table 1)
// 

use "jop final.dta", clear

drop if missing(votegenval) | voteregpre != 1

keep if mod(year, 2) == 0

keep state year votegenval weight stricty newstrict white black hispanic
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

local statecounter = 1
qui levelsof state, local(levels) 
foreach statename of local levels {
local yearcounter = 1
forvalues yearnum = 2006(2)2014 {
display "`statename'"  "`yearcounter'"
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & white, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing[`statecounter', `yearcounter'] = _b[_cons] 
matrix turnout_missing[`statecounter' + 1, `yearcounter'] = _se[_cons]
matrix turnout_missing[`statecounter' + 2, `yearcounter'] = e(N)
}
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & hispanic, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing[`statecounter' + 3, `yearcounter'] = _b[_cons] 
matrix turnout_missing[`statecounter' + 4, `yearcounter'] = _se[_cons]
matrix turnout_missing[`statecounter' + 5, `yearcounter'] = e(N)
}
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & black, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing[`statecounter' + 6, `yearcounter'] = _b[_cons] 
matrix turnout_missing[`statecounter' + 7, `yearcounter'] = _se[_cons]
matrix turnout_missing[`statecounter' + 8, `yearcounter'] = e(N)
}
local yearcounter = `yearcounter' + 1
}
local statecounter = `statecounter' + 9
}

//
// 2) Missings dropped, not conditioning on voteregpre, no sample weights (Figure 4)
// 

use "jop final.dta", clear

drop if missing(votegenval)

keep if mod(year, 2) == 0

keep state year votegenval weight stricty newstrict white black hispanic
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

local statecounter = 1
qui levelsof state, local(levels) 
foreach statename of local levels {
local yearcounter = 1
forvalues yearnum = 2006(2)2014 {
display "`statename'"  "`yearcounter'"
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & white, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing_noweights[`statecounter', `yearcounter'] = _b[_cons] 
matrix turnout_missing_noweights[`statecounter' + 1, `yearcounter'] = _se[_cons]
matrix turnout_missing_noweights[`statecounter' + 2, `yearcounter'] = e(N)
}
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & hispanic, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing_noweights[`statecounter' + 3, `yearcounter'] = _b[_cons] 
matrix turnout_missing_noweights[`statecounter' + 4, `yearcounter'] = _se[_cons]
matrix turnout_missing_noweights[`statecounter' + 5, `yearcounter'] = e(N)
}
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & black, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_missing_noweights[`statecounter' + 6, `yearcounter'] = _b[_cons] 
matrix turnout_missing_noweights[`statecounter' + 7, `yearcounter'] = _se[_cons]
matrix turnout_missing_noweights[`statecounter' + 8, `yearcounter'] = e(N)
}
local yearcounter = `yearcounter' + 1
}
local statecounter = `statecounter' + 9
}

//
// 3) Missings are non-voters, not conditioning on voteregorem, sample weights (Our preferred)
// 

use "jop final.dta", clear

replace votegenval = 0 if missing(votegenval)

keep if mod(year, 2) == 0

keep state year votegenval weight stricty newstrict white black hispanic
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

local statecounter = 1
qui levelsof state, local(levels) 
foreach statename of local levels {
local yearcounter = 1
forvalues yearnum = 2006(2)2014 {
display "`statename'"  "`yearcounter'"
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & white, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_nomissing[`statecounter', `yearcounter'] = _b[_cons] 
matrix turnout_nomissing[`statecounter' + 1, `yearcounter'] = _se[_cons]
matrix turnout_nomissing[`statecounter' + 2, `yearcounter'] = e(N)
}
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & hispanic, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_nomissing[`statecounter' + 3, `yearcounter'] = _b[_cons] 
matrix turnout_nomissing[`statecounter' + 4, `yearcounter'] = _se[_cons]
matrix turnout_nomissing[`statecounter' + 5, `yearcounter'] = e(N)
}
cap regress votegenval [pweight = weight] if state == "`statename'" & year == `yearnum' & black, robust
display "`statename'" "`yearcounter'"
display _rc
if _rc == 0 {
matrix turnout_nomissing[`statecounter' + 6, `yearcounter'] = _b[_cons] 
matrix turnout_nomissing[`statecounter' + 7, `yearcounter'] = _se[_cons]
matrix turnout_nomissing[`statecounter' + 8, `yearcounter'] = e(N)
}
local yearcounter = `yearcounter' + 1
}
local statecounter = `statecounter' + 9
}
clear
svmat turnout_missing 
svmat turnout_missing_noweights 
svmat turnout_nomissing

gen state =  ""
local statecounter = 1
foreach statename of local levels {
replace state = "`statename'" if _n == `statecounter'
local statecounter = `statecounter' + 9
}
replace state = state[_n-1] if missing(state)

forvalues i = 1(1)5 {
local j = 2006 + (`i' - 1)*2
rename turnout_missing`i' missing`j' 
rename turnout_missing_noweights`i' missing_noweights`j' 
rename turnout_nomissing`i' nomissing`j' 
}

gen row = mod((_n - 1), 9) + 1

reshape wide missing* nomissing*, i(state) j(row)

forvalues i = 2006(2)2014 {
rename missing`i'1 TURNOUTWHTmissing`i'
rename missing`i'2 SEWHTmissing`i'
rename missing`i'3 NWHTmissing`i'
rename missing_noweights`i'1 TURNOUTWHTmissing_noweights`i'
rename missing_noweights`i'2 SEWHTmissing_noweights`i'
rename missing_noweights`i'3 NWHTmissing_noweights`i'
rename nomissing`i'1 TURNOUTWHTnomissing`i'
rename nomissing`i'2 SEWHTnomissing`i'
rename nomissing`i'3 NWHTnomissing`i'
rename missing`i'4 TURNOUTHSPmissing`i'
rename missing`i'5 SEHSPmissing`i'
rename missing`i'6 NHSPmissing`i'
rename missing_noweights`i'4 TURNOUTHSPmissing_noweights`i'
rename missing_noweights`i'5 SEHSPmissing_noweights`i'
rename missing_noweights`i'6 NHSPmissing_noweights`i'
rename nomissing`i'4 TURNOUTHSPnomissing`i'
rename nomissing`i'5 SEHSPnomissing`i'
rename nomissing`i'6 NHSPnomissing`i'
rename missing`i'7 TURNOUTBLKmissing`i'
rename missing`i'8 SEBLKmissing`i'
rename missing`i'9 NBLKmissing`i'
rename missing_noweights`i'7 TURNOUTBLKmissing_noweights`i'
rename missing_noweights`i'8 SEBLKmissing_noweights`i'
rename missing_noweights`i'9 NBLKmissing_noweights`i'
rename nomissing`i'7 TURNOUTBLKnomissing`i'
rename nomissing`i'8 SEBLKnomissing`i'
rename nomissing`i'9 NBLKnomissing`i'
}

// Fix Virginia 2010 manually
local racegroups = "WHT HSP BLK"
foreach race of local racegroups {
replace TURNOUT`race'missing2010 = . if state == "Virginia"
replace TURNOUT`race'missing_noweights2010 = . if state == "Virginia"
replace TURNOUT`race'nomissing2010 = . if state == "Virginia"
replace SE`race'missing2010 = . if state == "Virginia"
replace SE`race'missing_noweights2010 = . if state == "Virginia"
replace SE`race'nomissing2010 = . if state == "Virginia"
replace N`race'missing2010 = 0 if state == "Virginia"
replace N`race'missing_noweights2010 = 0 if state == "Virginia"
replace N`race'nomissing2010 = 0 if state == "Virginia"
}

foreach var of varlist TURNOUTWHTmissing2006-NBLKnomissing2014 {
replace `var' = . if `var' == -9
}

sort state
save "ccesaggregatesbyrace.dta", replace

log close
