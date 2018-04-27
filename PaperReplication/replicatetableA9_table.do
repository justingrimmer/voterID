// Reports point estaimtes implied by analysis in Table A9, Col. 1 and 2
//Set working directory to location of replication folder
cd ""
cap log close
log using "replicatetableA9_table.log", replace

clear all
set more off

// Stores data for graph
matrix estimates = J(10, 3, -9)
// Lower bound on CI for graph
local lb = .025
// Lower bound on CI for graph
local ub = .975

use â"jop final.dta", clear

// Only keep even yeared elections
keep if mod(year, 2) == 0

// General Elections

regress votegenval stricty newstrict blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/  y2006 y2008 y2010 i.inputstate if voteregpre==1 

matrix temp = e(V) 
local i = 1
local pointestimate = _b[stricty]
local sigma = (temp[1, 1])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 2
local pointestimate = _b[stricty] + _b[blackstricty]
local sigma = (temp[1, 1] + 2*temp[1, 3] + temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 3
local pointestimate = _b[stricty] + _b[hispstricty]
local sigma = (temp[1, 1] + 2*temp[1, 4] + temp[4, 4])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 4
local pointestimate = _b[stricty] + _b[asianstricty]
local sigma = (temp[1, 1] + 2*temp[1, 5] + temp[5, 5])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 5
local pointestimate = _b[stricty] + _b[mixedracestricty]
local sigma = (temp[1, 1] + 2*temp[1, 6] + temp[6, 6])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

// Primary Elections

regress voteprival stricty newstrict blackstricty hispstricty asianstricty mixedracestricty  /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist  days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew  /*
*/  y2006 y2008 y2010 i.inputstate if voteregpre==1 

matrix temp = e(V) 
local i = 6
local pointestimate = _b[stricty]
local sigma = (temp[1, 1])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 7
local pointestimate = _b[stricty] + _b[blackstricty]
local sigma = (temp[1, 1] + 2*temp[1, 3] + temp[3, 3])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 8
local pointestimate = _b[stricty] + _b[hispstricty]
local sigma = (temp[1, 1] + 2*temp[1, 4] + temp[4, 4])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 9
local pointestimate = _b[stricty] + _b[asianstricty]
local sigma = (temp[1, 1] + 2*temp[1, 5] + temp[5, 5])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')
local i = 10
local pointestimate = _b[stricty] + _b[mixedracestricty]
local sigma = (temp[1, 1] + 2*temp[1, 6] + temp[6, 6])^(1/2)
matrix estimates[`i', 1] = `pointestimate'
matrix estimates[`i', 2] = `pointestimate' + `sigma' * invt(e(df_r), `lb')
matrix estimates[`i', 3] = `pointestimate' + `sigma' * invt(e(df_r), `ub')

clear 
svmat estimates

forvalues i = 1(1)3 {
replace estimates`i' = round(estimates`i'*100, .1)
}

rename estimates1 coefficients
rename estimates2 lowerbound
rename estimates3 upperbound

gen v1 = string(coefficients)
gen v2 = "[" + string(lowerbound) + ", " + string(upperbound) + "]"
drop coefficients lowerbound upperbound

gen electype = 1 if _n >= 1 & _n <= 5
replace electype = 2 if _n >= 6 & _n <= 10

gen racetype = mod(_n - 1, 5) + 1

reshape long v, i(racetype electype) j(temp)
reshape wide v, i(racetype temp) j(electype)

gen race = "White/Other" if _n == 1
replace race = "Black" if _n == 3
replace race = "Hispanic" if _n == 5
replace race = "Asian" if _n == 7
replace race = "Mixed Race" if _n == 9

gen tab = "&"
gen endline = "\\"
replace endline = endline + " [.02in]" if temp == 2

list race tab v1 tab v2 endline, noobs clean

log close

