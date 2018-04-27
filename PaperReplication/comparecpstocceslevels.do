// Plots CPS vs CCES estimates of racial difference in turnout levels
// Generates Figure A2 from JOP R&R 
// Must install ivreg2, ranktest, grc1leg 

// Set working directory to location of replication folder
cd ""
cap log close
log using "comparecpstocceslevels.log", replace

clear all
set more off

use "ccesaggregatesbyrace.dta", clear
keep state TURNOUTWHTnomissing* TURNOUTHSPnomissing* TURNOUTBLKnomissing* /*
*/ NWHTnomissing* NHSPnomissing* NBLKnomissing*
replace state = upper(state)
merge 1:1 state using "cpsturnout.dta"
// Drop DC & US
drop if _merge == 2
drop _merge

local racegroups = "wht his blk"
foreach race of local racegroups {
forvalues i = 2008(2)2014 {
destring  y`i'`race', force replace
if "`race'" ~= "his" {
rename y`i'`race' cps`race'`i'
}
if "`race'" == "his" {
rename y`i'`race' cpshsp`i'
}
}
}

forvalues i = 2006(2)2014 {
rename TURNOUTWHTnomissing`i' cceswht`i'
rename TURNOUTHSPnomissing`i' cceshsp`i' 
rename TURNOUTBLKnomissing`i' ccesblk`i' 
rename NWHTnomissing`i' ccesNwht`i'
rename NHSPnomissing`i' ccesNhsp`i' 
rename NBLKnomissing`i' ccesNblk`i'
}

reshape long cceswht cceshsp ccesblk cpswht cpshsp cpsblk /*
*/ ccesNwht ccesNhsp ccesNblk , i(state) j(year)
drop if year == 2006 | (state == "Virginia" & year <= 2010)

//
// Hispanics
//

gen ccesdiffhsp = (cceswht - cceshsp)*100
gen cpsdiffhsp = cpswht - cpshsp
gen nomissing = (missing(ccesdiffhsp) == 0 & missing(cpsdiffhsp) == 0)

sum ccesdiffhsp cpsdiffhsp if nomissing

// No weights

regress ccesdiffhsp cpsdiffhsp if nomissing 
// Gets partial r^2 after controlling for year dummies
ivreg2 ccesdiffhsp cpsdiffhsp i.year if nomissing,  partial(i.year) 

// Weighting by CCES sample size of Hispanics

regress ccesdiffhsp cpsdiffhsp [aweight = ccesNhsp] if nomissing 
// Gets partial r^2 after controlling for year dummies
ivreg2 ccesdiffhsp cpsdiffhsp i.year [aweight = ccesNhsp] if nomissing,  partial(i.year) 

twoway (scatter ccesdiffhsp cpsdiffhsp if nomissing, mlcolor(black) msymbol(circle_hollow)) /*
*/ (lfit ccesdiffhsp cpsdiffhsp if nomissing, range(-30 60) lcolor(black) lpattern(dash)) /*
*/ (function y = x, range(-30 60) lcolor(black)), /*
*/ xtitle("White - Hispanic Turnout Levels in CPS", size(small)) /*
*/ ytitle("White - Hispanic Turnout Levels in CCES", size(small)) /*
*/ xlabel(-30(15)60) ylabel(-30(15)60) aspectratio(1) legend(off) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g1)
drop nomissing

//
// Blacks
//

gen ccesdiffblk = (cceswht - ccesblk)*100
gen cpsdiffblk = cpswht - cpsblk
gen nomissing = (missing(ccesdiffblk) == 0 & missing(cpsdiffblk) == 0)

sum ccesdiffblk cpsdiffblk  if nomissing

// No weights

regress ccesdiffblk cpsdiffblk if nomissing 
// Gets partial r^2 after controlling for year dummies
ivreg2 ccesdiffblk cpsdiffblk i.year if nomissing,  partial(i.year) 

// Weighting by CCES sample size of Blacks

regress ccesdiffblk cpsdiffblk [aweight = ccesNblk] if nomissing 
// Gets partial r^2 after controlling for year dummies
ivreg2 ccesdiffblk cpsdiffblk i.year [aweight = ccesNblk] if nomissing,  partial(i.year) 

twoway (scatter ccesdiffblk cpsdiffblk if nomissing, mlcolor(black) msymbol(circle_hollow)) /*
*/ (lfit ccesdiffblk cpsdiffblk if nomissing, range(-30 60) lcolor(black) lpattern(dash)) /*
*/ (function y = x, range(-30 60) lcolor(black)), /*
*/ xtitle("White - Black Turnout Levels in CPS", size(small)) /*
*/ ytitle("White - Black Turnout Levels in CCES", size(small)) /*
*/ xlabel(-30(15)60) ylabel(-30(15)60) aspectratio(1) /*
*/ legend(label(3 "45 Degree Line") label(2 "Best Linear Fit") order(3 2) rows(1)) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g2)
drop nomissing

grc1leg g1 g2, legendfrom(g2) plotregion(color(white)) /*
*/ graphregion(color(white)) rows(1) name(g3)
graph export "comparecpstocceslevels.eps", replace

log close
