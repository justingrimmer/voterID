// Combines general and primary election analysis in Table A9
cd ""
cap log close
log using "replicatetableA9combine.log", replace

clear all
set more off

local spacing = "                                            "
graph combine TableA9.gph TableA9_primary.gph, /*
*/ altshrink imargin(zero) graphregion(color(white)) /*
*/ b2("`spacing'{&Delta} turnout percentage after strict voter ID implemented", size(small))
graph export "figs/TableA9_combined.eps", replace

log close
