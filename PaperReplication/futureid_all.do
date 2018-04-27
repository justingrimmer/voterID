// Looks at future ID law on turnout
cd ""
cap log close
log using "futureid_all.log", replace

clear all
set more off

use "jop final.dta", clear

// Did state ever adopt strict ID
egen IDstate = max(stricty), by(state)
gen blackIDstate = black * IDstate
gen hispanicIDstate = hispanic * IDstate
gen asianIDstate = asian * IDstate
gen mixedraceIDstate = mixedrace * IDstate

// Does state currently have strict ID
gen AlreadyIDstate = stricty


// Only keep 2008-2012 elections
drop if year == 2006 | year == 2014

//
// General Election
// 

// Drops Virginia 
drop if state == "Virginia"

// Baseline analysis

logit votegenval IDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008 /*
*/ if voteregpre==1 & AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate  using "futureIDall.xls", replace dec(3) noaster
margins, dydx(IDstate)

logit votegenval IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008   /*
*/ if voteregpre==1 & AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate using "futureIDall.xls", append dec(3) noaster

// Don't condition on voteregpre

logit votegenval IDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate  using "futureIDall.xls", append dec(3) noaster

logit votegenval IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate using "futureIDall.xls", append dec(3) noaster

// Replace missings w/ zeros 

replace votegenval = 0 if missing(votegenval)
logit votegenval IDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate  using "futureIDall.xls", append dec(3) noaster

logit votegenval IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate using "futureIDall.xls", append dec(3) noaster

//
// Primary Election
// 

// Drops Virginia 
drop if state == "Louisiana"

// Baseline analysis

logit voteprival IDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if voteregpre==1 & AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate  using "futureIDall_primary.xls", replace dec(3) noaster

logit voteprival IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if voteregpre==1 & AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate using "futureIDall_primary.xls", append dec(3) noaster

// Don't condition on voteregpre

logit voteprival IDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate  using "futureIDall_primary.xls", append dec(3)

logit voteprival IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate using "futureIDall_primary.xls", append dec(3) noaster

// Replace missings w/ zeros 

replace voteprival = 0 if missing(voteprival)
logit voteprival IDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate  using "futureIDall_primary.xls", append dec(3) noaster

logit voteprival IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate   /*
*/ black hispanic asian mixedrace foreignb firstgen age educ inc male married childrenz unionz /*
*/ unemp  ownhome protestant catholic jewish atheist days_before_election early_in_person vote_by_mail /*
*/ no_excuse_absence_ presidentialelectionyear gubernatorialelectionyear senateelectionyear marginpnew y2008  /*
*/ if AlreadyIDstate == 0 [pw=weight], cluster(inputstate)
outreg2 IDstate blackIDstate hispanicIDstate asianIDstate mixedraceIDstate using "futureIDall_primary.xls", append dec(3) noaster


log close
