// Plots Actual vs CCES estimates of turnout levels/changes
// Generates Figure 1 from JOP R&R 
// Must install ivreg2, ranktest, grc1leg 

// Set working directory to location of replication folder
cd ""
cap log close
log using "compareactualtoccesall.log", replace

clear all
set more off


use "ccesaggregates.dta", replace
keep state TURNOUTmissing* SEmissing*
drop TURNOUTmissing_* SEmissing_*
merge 1:1 state using "actualturnout.dta"
// Drop DC
drop if _merge == 2
drop _merge
keep state TURNOUTmissing* SEmissing* stateabbev VEPhighestoffice*
merge 1:1 state using "treatmentstatus.dta"
drop _merge newstrict*

reshape long TURNOUTmissing SEmissing VEPhighestoffice stricty, /*
*/ i(state stateabbev) j(year)

//
// Levels, HLN Sample, Missings Dropped 
//

replace TURNOUTmissing = TURNOUTmissing * 100
rename TURNOUTmissing CCES
replace VEPhighestoffice = VEPhighestoffice * 100
rename VEPhighestoffice Actual
gen CCESvariance = 1 / SEmissing^2

regress CCES Actual
// Gets partial r^2 after controlling for year dummies
ivreg2 CCES Actual i.year,  partial(i.year) 

twoway (scatter CCES Actual if year ~= 2006 & (year ~= 2008 & state ~= "Virginia"), /*
*/ mlcolor(black) msymbol(circle_hollow)) (scatter CCES Actual if year == 2006 /*
*/ | (year == 2008 & state == "Virginia"), mcolor(black)) /*
*/ (function y = x, range(0 100) lcolor(black)) /*
*/ (lfit CCES Actual, range(0 100) lcolor(black) lpattern(dash)), /*
*/ xlabel(0(20)100) ylabel(0(20)100, angle(horizontal)) fxsize(100) /*
*/ title("HLN Sample" "Missings Dropped", color(black) size(small)) /*
*/ xtitle("Turnout Level (VEP)", size(small)) /*
*/ ytitle("Turnout Level (CCES)", size(small)) legend(off) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g1)

//
// Levels, Our Preferred Sample, Missings Dropped 
//

drop if year == 2006 | (year == 2008 & state == "Virginia")

// No weights

regress CCES Actual
// Gets partial r^2 after controlling for year dummies
ivreg2 CCES Actual i.year,  partial(i.year) 

// Weights

regress CCES Actual [aweight = CCESvariance]
// Gets partial r^2 after controlling for year dummies
ivreg2 CCES Actual i.year [aweight = CCESvariance], partial(i.year) 

twoway (scatter CCES Actual if year ~= 2006 & (year ~= 2008 & state ~= "Virginia"), /*
*/ mlcolor(black) msymbol(circle_hollow)) (scatter CCES Actual if year == 2006 /*
*/ | (year == 2008 & state == "Virginia"), mcolor(black)) /*
*/ (function y = x, range(0 100) lcolor(black)) /*
*/ (lfit CCES Actual, range(0 100) lcolor(black) lpattern(dash)), /*
*/ xlabel(0(20)100) ylabel(0(20)100, angle(horizontal)) fxsize(100) /*
*/ title("Our Preferred Sample" "Missings Dropped", color(black) size(small)) /*
*/ xtitle("Turnout Level (VEP)", size(small)) /*
*/ ytitle("Turnout Level (CCES)", size(small)) legend(off) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g2)

//
// Levels, Our Preferred Sample, Missings are Non-Voters
//

use "ccesaggregates.dta", replace
keep state TURNOUTnomissing* SEnomissing*
merge 1:1 state using "actualturnout.dta"
// Drop DC
drop if _merge == 2
drop _merge
keep state TURNOUTnomissing* SEnomissing* stateabbev VEPhighestoffice*
merge 1:1 state using "treatmentstatus.dta"
drop _merge newstrict*

reshape long TURNOUTnomissing SEnomissing VEPhighestoffice stricty, /*
*/ i(state stateabbev) j(year)

replace TURNOUTnomissing = TURNOUTnomissing * 100
rename TURNOUTnomissing CCES
replace VEPhighestoffice = VEPhighestoffice * 100
rename VEPhighestoffice Actual
gen CCESvariance = 1 / SEnomissing^2

drop if year == 2006 | (year == 2008 & state == "Virginia")

regress CCES Actual
// Gets partial r^2 after controlling for year dummies
ivreg2 CCES Actual i.year,  partial(i.year) 

regress CCES Actual [aweight = CCESvariance]
// Gets partial r^2 after controlling for year dummies
ivreg2 CCES Actual i.year [aweight = CCESvariance],  partial(i.year) 

twoway (scatter CCES Actual if year ~= 2006 & (year ~= 2008 & state ~= "Virginia"), /*
*/ mlcolor(black) msymbol(circle_hollow)) (scatter CCES Actual if year == 2006 /*
*/ | (year == 2008 & state == "Virginia"), mcolor(black)) /*
*/ (function y = x, range(0 100) lcolor(black)) /*
*/ (lfit CCES Actual, range(0 100) lcolor(black) lpattern(dash)), /*
*/ xlabel(0(20)100) ylabel(0(20)100, angle(horizontal)) fxsize(100) /*
*/ title("Our Preferred Sample" "Missings are Non-Voters", color(black) size(small)) /*
*/ xtitle("Turnout Level (VEP)", size(small)) /*
*/ ytitle("Turnout Level (CCES)", size(small)) legend(off) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g3)

//
// Differences, Our Preferred Sample, Missings are Non-Voters
//

gen CCESDiff = CCES - CCES[_n-2] if state == state[_n-2]
gen ActualDiff = Actual - Actual[_n-2] if state == state[_n-2]
gen CCESvarianceDiff = CCESvariance + CCESvariance[_n-2] if state == state[_n-2]

regress CCESDiff ActualDiff
// Gets partial r^2 after controlling for year dummies
ivreg2 CCESDiff ActualDiff i.year,  partial(i.year) 

regress CCESDiff ActualDiff [aweight = CCESvarianceDiff]
// Gets partial r^2 after controlling for year dummies
ivreg2 CCESDiff ActualDiff i.year [aweight = CCESvarianceDiff],  partial(i.year) 

twoway (scatter CCESDiff ActualDiff if year ~= 2006 & (year ~= 2008 & state ~= "Virginia"), /*
*/ mlcolor(black) msymbol(circle_hollow)) (scatter CCESDiff ActualDiff if year == 2006 /*
*/ | (year == 2008 & state == "Virginia"), mcolor(black)) /*
*/ (function y = x, range(-20 45) lcolor(black)) /*
*/ (lfit CCESDiff ActualDiff, range(-20 45) lcolor(black) lpattern(dash)), /*
*/ xlabel(-20(10)45) ylabel(-20(10)45, angle(horizontal)) fxsize(100) /*
*/ title("Our Preferred Sample" "Missings are Non-Voters", color(black) size(small)) /*
*/ xtitle("Turnout Change from 4 Years Prior (VEP)", size(small)) ytitle("Turnout Change from 4 Years Prior (CCES)", size(small)) /*
*/ legend(label(2 "We Drop") label(3 "45 Degree Line") label(4 "Best Linear Fit") order(3 4 2) rows(1)) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g4)

grc1leg g1 g2 g3 g4, legendfrom(g4) plotregion(color(white)) /*
*/ graphregion(color(white)) rows(2) name(g5)
graph display g5, xsize(5.5) ysize(6.15)
graph export "compareacutaltoccesall.eps", replace

log close
