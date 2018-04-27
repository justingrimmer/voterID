// Looks at different specifications of results presented in Figure 4, primary election
//Set working directory to location of replication folder
cd ""
cap log close
log using "replicatefigure4_expanded_primary.log", replace

clear all
set more off

// Stores data for graph
matrix estimates = J(15, 3, -9)
// Lower bound on CI for graph
local lb = .025
// Lower bound on CI for graph
local ub = .975

use "jop final.dta", clear
keep if year == 2010 | year == 2014
keep if race == 1 | race == 2 | race == 3

gen treated = 0
replace treated = 1 if state == "Mississippi"
replace treated = 1 if state == "North Dakota"
replace treated = 1 if state == "Texas"

gen y2014 = (year == 2014)
gen treatedXy2014 = treated * y2014

gen hispanicXy2014 = hispanic * y2014
gen blackXy2014 = black * y2014

gen treatedXhispanic = treated * hispanic
gen treatedXhispanicXy2014  = treated * hispanic * y2014

gen treatedXblack = treated * black
gen treatedXblackXy2014  = treated * black * y2014

// Replicates Figure 4 using regression

regress voteprival treated y2014 treatedXy2014 hispanic treatedXhispanic /*
*/ hispanicXy2014 treatedXhispanicXy2014 black treatedXblack blackXy2014 /*
*/ treatedXblackXy2014, robust cluster(inputstate)
outreg2 using "Figure4_primary.xls", replace dec(3) noaster

matrix temp = e(V) 
local i = 1
local pointestimate = _b[treatedXy2014]
local sigma = (temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 2
local pointestimate = _b[treatedXy2014] + _b[treatedXhispanicXy2014]
local sigma = (temp[3, 3] + 2*temp[3, 7] + temp[7, 7])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 3
local pointestimate = _b[treatedXy2014] + _b[treatedXblackXy2014]
local sigma = (temp[3, 3] + 2*temp[3, 11] + temp[11, 11])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// + Sample Weights

regress voteprival treated y2014 treatedXy2014 hispanic treatedXhispanic /*
*/ hispanicXy2014 treatedXhispanicXy2014 black treatedXblack blackXy2014 /*
*/ treatedXblackXy2014 [pweight = weight], robust cluster(inputstate)
outreg2 using "Figure4_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 4
local pointestimate = _b[treatedXy2014]
local sigma = (temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 5
local pointestimate = _b[treatedXy2014] + _b[treatedXhispanicXy2014]
local sigma = (temp[3, 3] + 2*temp[3, 7] + temp[7, 7])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 6
local pointestimate = _b[treatedXy2014] + _b[treatedXblackXy2014]
local sigma = (temp[3, 3] + 2*temp[3, 11] + temp[11, 11])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// + All 6 Treated States

replace treated = 1 if state == "Alabama"
replace treated = 1 if state == "Kansas"
replace treated = 1 if state == "Tennessee"

replace treatedXy2014 = treated * y2014

replace treatedXhispanic = treated * hispanic
replace treatedXhispanicXy2014  = treated * hispanic * y2014

replace treatedXblack = treated * black
replace treatedXblackXy2014  = treated * black * y2014

regress voteprival treated y2014 treatedXy2014 hispanic treatedXhispanic /*
*/ hispanicXy2014 treatedXhispanicXy2014 black treatedXblack blackXy2014 /*
*/ treatedXblackXy2014 [pweight = weight], robust cluster(inputstate)
outreg2 using "Figure4_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 7
local pointestimate = _b[treatedXy2014]
local sigma = (temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 8
local pointestimate = _b[treatedXy2014] + _b[treatedXhispanicXy2014]
local sigma = (temp[3, 3] + 2*temp[3, 7] + temp[7, 7])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 9
local pointestimate = _b[treatedXy2014] + _b[treatedXblackXy2014]
local sigma = (temp[3, 3] + 2*temp[3, 11] + temp[11, 11])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// + State Fixed Effects

regress voteprival y2014 treatedXy2014 hispanic treatedXhispanic /*
*/ hispanicXy2014 treatedXhispanicXy2014 black treatedXblack blackXy2014 /*
*/ treatedXblackXy2014 i.inputstate [pweight = weight], robust cluster(inputstate)
outreg2 using "Figure4_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 10
local pointestimate = _b[treatedXy2014]
local sigma = (temp[2, 2])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 11
local pointestimate = _b[treatedXy2014] + _b[treatedXhispanicXy2014]
local sigma = (temp[2, 2] + 2*temp[2, 6] + temp[6, 6])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 12
local pointestimate = _b[treatedXy2014] + _b[treatedXblackXy2014]
local sigma = (temp[2, 2] + 2*temp[2, 10] + temp[10, 10])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// + Make Missings Non-Voters

replace voteprival = 0 if missing(voteprival) == 1

regress voteprival y2014 treatedXy2014 hispanic treatedXhispanic /*
*/ hispanicXy2014 treatedXhispanicXy2014 black treatedXblack blackXy2014 /*
*/ treatedXblackXy2014 i.inputstate [pweight = weight], robust cluster(inputstate)
outreg2 using "Figure4_primary.xls", append dec(3) noaster

matrix temp = e(V) 
local i = 13
local pointestimate = _b[treatedXy2014]
local sigma = (temp[2, 2])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 14
local pointestimate = _b[treatedXy2014] + _b[treatedXhispanicXy2014]
local sigma = (temp[2, 2] + 2*temp[2, 6] + temp[6, 6])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 15
local pointestimate = _b[treatedXy2014] + _b[treatedXblackXy2014]
local sigma = (temp[2, 2] + 2*temp[2, 10] + temp[10, 10])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

matlist estimates

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

gen model = 3 if _n == 15
replace model = 5 if _n == 14
replace model = 7 if _n == 13
replace model = 13 if _n == 12
replace model = 15 if _n == 11
replace model = 17 if _n == 10
replace model = 23 if _n == 9
replace model = 25 if _n == 8
replace model = 27 if _n == 7
replace model = 33 if _n == 6
replace model = 35 if _n == 5
replace model = 37 if _n == 4
replace model = 43 if _n == 3
replace model = 45 if _n == 2
replace model = 47 if _n == 1

label define model 47 "Diffrence-in-differences", replace
label define model 45 "underlying Figure 4 of", add
label define model 43 "Hajnal, Lajevardi, and Nielson", add
label define model 35 "+ Apply sampling weights", add
label define model 25 "+ AL, KS, TN also treated", add
label define model 15 "+ State fixed effects", add
label define model 6 "+ Treat respondents who don't", add
label define model 4 "match to voter file as nonvoters", add

label values model model

twoway (scatter model coefficients if mod(model, 10) == 7, mcolor(black)) /*
*/ (scatter  model coefficients if mod(model, 10) == 5, mlcolor(black) mfcolor(white)) /*
*/ (scatter  model coefficients if mod(model, 10) == 3, mlcolor(black) mfcolor(gray)) /*
*/ (rspike lowerbound upperbound model, horizontal lcolor(black)), /*
*/ plotregion(color(white)) graphregion(color(white)) ylabel(,nogrid) /*
*/ xlabel(-10(10)30) xtitle("Primary elections" " " /*
*/ "Estimated {&Delta} turnout percentage" "after strict voter ID implemented") /*
*/ ylabel(4 6 15 25 35 43 45 47, valuelabel angle(0) noticks) xline(0) ytitle("") /*
*/ legend(row(1) label(1 "Whites") label(2 "Hispanics") label(3 "Blacks") order(1 2 3))
graph export "Figure4_primary.eps", replace

log close

