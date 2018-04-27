// Makes table of obs. missing voter validation by racial status and year
//Set working directory to location of replication folder
cd ""
cap log close
log using "missingtable.log", replace

clear all
set more off

use "jop final.dta", clear

keep if mod(year, 2) == 0
keep race year votegenval weight

gen missingreg = (missing(votegenval))*weight
gen totalreg = weight

collapse (sum) missingreg totalreg, by(race year)

list 
drop if missing(race)

reshape wide missingreg totalreg, i(year) j(race)

egen missingreg0 = rsum(missingreg*)
egen totalreg0 = rsum(totalreg*)

forvalues i = 0(1)8 {
gen y`i' = missingreg`i' / totalreg`i'
replace y`i' = round(100*y`i', .1)
}
drop missingreg* totalreg*

reshape long y, i(year) j(race)
reshape wide y, i(race) j(year)

label define race 0 "All", replace
label define race 1 "White", add
label define race 2 "Black", add
label define race 3 "Hispanic", add
label define race 4 "Asian", add
label define race 5 "Native American", add
label define race 6 "Mixed", add
label define race 7 "Other", add
label define race 8 "Middle Eastern", add

label values race race

gen tab = "&"
gen endline = "\\"

list race tab y2006 tab y2008 tab y2010 tab y2012 tab y2014 endline, noobs clean

log close
