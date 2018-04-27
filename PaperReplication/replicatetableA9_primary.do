// Replicates and expands the primary election analysis in Table A9, col 2
cd ""
cap log close
log using "replicatetableA9_primary.log", replace

clear all
set more off

// Stores data for graph
matrix estimates = J(8, 3, -9)
// Lower bound on CI for graph
local lb = .025
// Lower bound on CI for graph
local ub = .975

use "jop final.dta", clear

// Only keep even yeared elections
keep if mod(year, 2) == 0

// Replicates Main Analysis

regress voteprival stricty newstrict blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/ y2008 y2010 i.inputstate if voteregpre==1 
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", replace dec(3) noaster

matrix temp = e(V) 
local i = 1
local pointestimate = _b[stricty]
local sigma = (temp[1, 1])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 2
local pointestimate = _b[stricty] + _b[hispstricty]
local sigma = (temp[1, 1] + 2*temp[1, 4] + temp[4, 4])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// w/ clustering

regress voteprival stricty newstrict blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/ y2008 y2010 i.inputstate if voteregpre==1, cluster(inputstate)
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", append dec(3) noaster

// w/o newstrict

regress voteprival stricty blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/ y2008 y2010 i.inputstate if voteregpre==1, cluster(inputstate)
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", append dec(3) noaster

// drop LA and VA

gen goodstate = 1
replace goodstate = 0 if state == "Louisiana"
replace goodstate = 0 if state == "Virginia"
replace goodstate = 0 if year == 2006

regress voteprival stricty blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/ y2008 y2010 i.inputstate if voteregpre==1 & goodstate == 1, cluster(inputstate)
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", append dec(3) noaster

// w/ weights

regress voteprival stricty blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/ y2008 y2010 i.inputstate if voteregpre==1 & goodstate == 1 [pw=weight], cluster(inputstate)
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 3
local pointestimate = _b[stricty]
local sigma = (temp[1, 1])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 4
local pointestimate = _b[stricty] + _b[hispstricty]
local sigma = (temp[1, 1] + 2*temp[1, 3] + temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// don't condition on voteregpre

regress voteprival stricty blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/  y2008 y2010 i.inputstate if goodstate == 1  [pw=weight], cluster(inputstate)
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 5
local pointestimate = _b[stricty]
local sigma = (temp[1, 1])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 6
local pointestimate = _b[stricty] + _b[hispstricty]
local sigma = (temp[1, 1] + 2*temp[1, 3] + temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// replace missings w/ zeros 
replace voteprival = 0 if missing(voteprival)
regress voteprival stricty blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/  y2008 y2010 i.inputstate if goodstate == 1  [pw=weight], cluster(inputstate)
outreg2 stricty newstrict blackstricty hispstricty asianstricty mixedracestricty using "TableA9_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 7
local pointestimate = _b[stricty]
local sigma = (temp[1, 1])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 8
local pointestimate = _b[stricty] + _b[hispstricty]
local sigma = (temp[1, 1] + 2*temp[1, 3] + temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// Plots 95% CIs from Selected Models

clear 
svmat estimates

// Scales by 100
forvalues i = 1(1)3 {
replace estimates`i' = estimates`i'*100
}

rename estimates1 coefficients
rename estimates2 lowerbound
rename estimates3 upperbound

gen model = 4 if _n == 8
replace model = 6 if _n == 7
replace model = 14 if _n == 6
replace model = 16 if _n == 5
replace model = 24 if _n == 4
replace model = 26 if _n == 3
replace model = 34 if _n == 2
replace model = 36 if _n == 1

label define model 36 "Hajnal, Lajevardi, and Nielson", replace
label define model 34 "Table A9, Column 2", add
label define model 28 "+ Cluster standard errors,", add
label define model 26 "apply sampling weights,", add
label define model 24 "include single treatment, &", add
label define model 22 "drop Louisiana and Virginia", add
label define model 16 "+ Retain self-classified", add
label define model 14 "unregistered respondents", add
label define model 6 "+ Treat respondents who don't", add
label define model 4 "match to voter file as nonvoters", add

label values model model

twoway (scatter model coefficients if mod(model, 10) == 6, mcolor(black)) /*
*/ (scatter  model coefficients if mod(model, 10) == 4, mlcolor(black) mfcolor(white)) /*
*/ (rspike lowerbound upperbound model, horizontal lcolor(black)), /*
*/ plotregion(color(white)) graphregion(color(white)) xlabel(-10(5)15) /*
*/ xtitle("Primary elections" " " "{&Delta} turnout percentage after strict voter ID implemented") /*
*/ ylabel(4 6 14 16 22 24 26 28 34 36, nogrid valuelabel angle(0) noticks) xline(0) ytitle("") /*
*/ legend(label(1 "Whites") label(2 "Hispanics") order(1 2))
graph export "TableA9_primary.eps", replace

twoway (scatter model coefficients if mod(model, 10) == 6, mcolor(black)) /*
*/ (scatter  model coefficients if mod(model, 10) == 4, mlcolor(black) mfcolor(white)) /*
*/ (rspike lowerbound upperbound model, horizontal lcolor(black)), /*
*/ plotregion(color(white)) graphregion(color(white)) xlabel(-10(5)15) /*
*/ xtitle("Primary elections") /*
*/ ylabel(4 6 14 16 22 24 26 28 34 36, nogrid valuelabel angle(0) noticks) xline(0) ytitle("") /*
*/ legend(label(1 "Whites") label(2 "Hispanics") order(1 2))
graph save "TableA9_primary.gph", replace

log close

