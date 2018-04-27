// Makes table of Hajnal et al's treatment status by state and year
//Set working directory to location of replication folder
cd ""
cap log close
log using "treatmentstatus.log", replace

use "jop final.dta", clear

// Only keep even yeared elections
keep if mod(year, 2) == 0

keep state year stricty newstrict
drop if missing(state)
replace state = "West Virginia" if state == "West Virgina"

collapse (mean) stricty newstrict, by(state year)

reshape wide stricty newstrict, i(state) j(year)

sort state 
save "treatmentstatus.dta", replace

log close
