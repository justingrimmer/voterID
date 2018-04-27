// Replicates DD internals of the DDD presented in Figure 4
//Set working directory to location of replication folder
cd ""
cap log close
log using "replicatefigure4.log", replace

clear all
set more off

matrix spikeplot = J(10, 3, -9)

use "jop final.dta", clear
keep if year == 2010 | year == 2014

gen y2014 = (year == 2014)


gen treated = 0 
replace treated = 1 if state == "Mississippi"
replace treated = 1 if state == "North Dakota"
replace treated = 1 if state == "Texas"

gen treatedXy2014 = treated * y2014

regress votegenval treated y2014 treatedXy2014 if white == 1, robust cluster(inputstate) 
local i = 1
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014] 
regress votegenval treated y2014 treatedXy2014 if hispanic == 1, robust cluster(inputstate) 
local i = 2
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014] 
regress votegenval treated y2014 treatedXy2014 if black == 1, robust cluster(inputstate)
local i = 3
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014] 
regress votegenval treated y2014 treatedXy2014 if asian == 1, robust cluster(inputstate) 
local i = 4
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]
regress votegenval treated y2014 treatedXy2014 if mixedrace == 1, robust cluster(inputstate) 
local i = 5
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]

regress voteprival treated y2014 treatedXy2014 if white == 1, robust cluster(inputstate) 
local i = 6
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]
regress voteprival treated y2014 treatedXy2014 if hispanic == 1, robust cluster(inputstate) 
local i = 7
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]
regress voteprival treated y2014 treatedXy2014 if black == 1, robust cluster(inputstate)
local i = 8
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]
regress voteprival treated y2014 treatedXy2014 if asian == 1, robust cluster(inputstate) 
local i = 9
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]
regress voteprival treated y2014 treatedXy2014 if mixedrace == 1, robust cluster(inputstate) 
local i = 10
matrix spikeplot[`i', 1] = _b[treatedXy2014]
matrix spikeplot[`i', 2] = _b[treatedXy2014] - 1.96*_se[treatedXy2014] 
matrix spikeplot[`i', 3] = _b[treatedXy2014] + 1.96*_se[treatedXy2014]

matlist spikeplot

clear 
svmat spikeplot

// Scales by 100
forvalues i = 1(1)3 {
replace spikeplot`i' = spikeplot`i'*100
}

gen group = "White" if _n == 1 | _n == 6
replace group = "Hispanic" if _n == 2 | _n == 7
replace group = "Black" if _n == 3 | _n == 8
replace group = "Asian" if _n == 4 | _n == 9
replace group = "Mixed" if _n == 5 | _n == 10

gen election = "General" if _n >= 1 & _n <= 5
replace election = "Primary" if _n >= 6 & _n <= 10

drop if group == "Asian" | group == "Mixed"

graph bar spikeplot1, over(group) over(election) bar(1, bcolor(gs8)) /*
*/ yline(0, lcolor(black)) ytitle("{&Delta} turnout percentage from implementing strict voter ID" /*
*/ "Difference-in-differece estimates by race and election type") /*
*/ ylabel(-2(2)8,nogrid) plotregion(color(white)) graphregion(color(white))
graph export "figure4DDpointestimates.eps", replace

log close
