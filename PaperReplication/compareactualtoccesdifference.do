// Plots Actual vs CCES estimates of turnout changes
// Generates Figure A1 from JOP R&R 
// Must install grc1leg 

// Set working directory to location of replication folder
cd ""
cap log close
log using "compareactualtoccesdifference.log", replace

clear all
set more off

//
// HLN's method 
//

use "ccesaggregates.dta", replace
keep state TURNOUTmissing* SEmissing*
drop TURNOUTmissing_* SEmissing_*
merge 1:1 state using "actualturnout.dta"
// Drop DC
drop if _merge == 2
drop _merge
keep state TURNOUTmissing* SEmissing* VEPhighestoffice*

// 2012 vs 2008

gen CCES = (TURNOUTmissing2012 - TURNOUTmissing2008) * 100
gen CCES_var = (SEmissing2012^2 + SEmissing2008^2)
gen Actual = (VEPhighestoffice2012  - VEPhighestoffice2008) * 100

regress CCES Actual
regress CCES Actual [aweight = 1/CCES_var]

twoway (scatter CCES Actual if state ~= "Virginia", mlcolor(black) msymbol(circle_hollow)) /*
*/ (scatter CCES Actual if state == "Virginia", mcolor(black)) /*
*/ (function y = x, range(-10 5) lcolor(black)) /*
*/ (lfit CCES Actual [aweight = 1/CCES_var], range(-10 5) lcolor(black) lpattern(dash)), /*
*/ xlabel(-10(5)5) ylabel(-20(20)90, angle(horizontal)) /*
*/ title("HLN Data (2012 - 2008)", color(black) size(medium)) /*
*/ xtitle("Change in VEP Turnout") ytitle("Change in CCES Turnout") /*
*/ legend(label(2 "We Drop") label(3 "45 Degree Line") label(4 "Best Linear Fit") order(3 4 2) rows(1)) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g1)

drop CCES CCES_var Actual

// 2014 vs 2010

gen CCES = (TURNOUTmissing2014 - TURNOUTmissing2010) * 100
gen CCES_var = (SEmissing2014^2 + SEmissing2010^2)
gen Actual = (VEPhighestoffice2014  - VEPhighestoffice2010) * 100

regress CCES Actual
regress CCES Actual [aweight = 1/CCES_var]

twoway (scatter CCES Actual, mlcolor(black) msymbol(circle_hollow)) /*
*/ (function y = x, range(-15 5) lcolor(black)) /*
*/ (lfit CCES Actual, range(-15 5) lcolor(black) lpattern(dash)), /*
*/ xlabel(-15(5)5) ylabel(-20(10)45, angle(horizontal)) /*
*/ title("HLN Data (2014 - 2010)", color(black) size(medium)) /*
*/ xtitle("Change in VEP Turnout") ytitle("Change in CCES Turnout") /*
*/ legend(label(2 "45 Degree Line") label(3 "Best Linear Fit") order(2 3) rows(1)) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g2)

drop CCES CCES_var Actual

//
// Our preferred method 
//

use "ccesaggregates.dta", replace
keep state TURNOUTnomissing* SEnomissing*
merge 1:1 state using "actualturnout.dta"
// Drop DC
drop if _merge == 2
drop _merge
keep state TURNOUTnomissing* SEnomissing* VEPhighestoffice*
drop if state == "Virginia"

// 2012 vs 2008

gen CCES = (TURNOUTnomissing2012 - TURNOUTnomissing2008) * 100
gen CCES_var = (SEnomissing2012^2 + SEnomissing2008^2)
gen Actual = (VEPhighestoffice2012  - VEPhighestoffice2008) * 100

regress CCES Actual  
regress CCES Actual  [aweight = 1/CCES_var]

twoway (scatter CCES Actual, mlcolor(black) msymbol(circle_hollow)) /*
*/ (function y = x, range(-10 5) lcolor(black)) /*
*/ (lfit CCES Actual, range(-10 5) lcolor(black) lpattern(dash)), /*
*/ xlabel(-10(5)5) ylabel(-20(20)90, angle(horizontal)) /*
*/ title("Our Preferred Data (2012 - 2008)", color(black) size(medium)) /*
*/ xtitle("Change in VEP Turnout") ytitle("Change in CCES Turnout") /*
*/ legend(label(2 "45 Degree Line") label(3 "Best Linear Fit") order(2 3) rows(1)) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g3)

drop CCES CCES_var Actual

// 2014 vs 2010

gen CCES = (TURNOUTnomissing2014 - TURNOUTnomissing2010) * 100
gen CCES_var = (SEnomissing2014^2 + SEnomissing2010^2)
gen Actual = (VEPhighestoffice2014  - VEPhighestoffice2010) * 100

regress CCES Actual
regress CCES Actual [aweight = 1/CCES_var]

twoway (scatter CCES Actual, mlcolor(black) msymbol(circle_hollow)) /*
*/ (function y = x, range(-15 5) lcolor(black)) /*
*/ (lfit CCES Actual [aweight = 1/CCES_var], range(-15 5) lcolor(black) lpattern(dash)), /*
*/ xlabel(-15(5)5) ylabel(-20(10)45, angle(horizontal)) /*
*/ title("Our Preferred Data (2014 - 2010)", color(black) size(medium)) /*
*/ xtitle("Change in VEP Turnout") ytitle("Change in CCES Turnout") /*
*/ legend(label(2 "45 Degree Line") label(3 "Best Linear Fit") order(2 3) rows(1)) /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,grid) name(g4)

drop CCES CCES_var Actual

grc1leg g1 g3 g2 g4, legendfrom(g1) plotregion(color(white)) graphregion(color(white))
graph export "compareactualtoccesdifference.eps", replace

log close
