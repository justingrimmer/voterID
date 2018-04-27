// Makes table of primary election turnout by state by year
//Set working directory to location of replication folder
cd ""
cap log close
log using "primaryturnoutbystateyear.log", replace

clear all
set more off

matrix turnout = J(150, 4, -9)

use "ccesaggregates_primary.dta", clear

keep state TURNOUTmissing* SEmissing* Nmissing*
reshape long TURNOUTmissing SEmissing Nmissing, i(state) j(year)
replace TURNOUTmissing = round(TURNOUTmissing*1000, 1)
rename TURNOUTmissing y1
replace SEmissing = round(SEmissing*1000, 1)
rename SEmissing y2
rename Nmissing y3
reshape long y, i(state year) j(q)
tostring y, force replace
reshape wide y, i(state q) j(year)

forvalues i = 2008(2)2014 {
gen temp = length(y`i')
replace y`i' = substr(y`i', 1, temp - 1) + "." + substr(y`i', temp, 1) if mod(_n, 3) ~= 0
replace y`i' = "0" if y`i' == ".0"
replace y`i' = "(" + y`i' + ")" if mod(_n, 3) == 2
replace y`i' = "N = " + y`i' if mod(_n, 3) == 0
drop temp
}


// Fix Virginia 2010 manually
replace y2010 = "" if state == "Virginia" & (q == 1 | q == 2)

merge n:1 state using "treatmentstatus.dta"
drop _merge

// Strict and not first year
forvalues i = 2008(2)2014 {
replace y`i' = "\cellcolor{light-gray}{" + y`i' + "}" if stricty`i' == 1 & newstrict`i' == 0
}

// Strict and first year
forvalues i = 2008(2)2014 {
replace y`i' = "\cellcolor{dark-gray}{" + y`i' + "}" if stricty`i' == 1 & newstrict`i' == 1
}

gen tab = "&"
gen endline = "\\"
replace endline = endline + "*" if mod(_n, 3) ~= 0
replace endline = endline + " [.01in]" if mod(_n, 3) == 0
replace state = "" if q ~= 1

list state tab y2008 tab y2010 tab y2012 tab y2014 endline, noobs clean

log close

