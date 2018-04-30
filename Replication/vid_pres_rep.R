####This file replicates the analysis in 
####``Misrepresentations in Hajnal, Kuk, and Lajevardi (2018)" from 
####Grimmer, Hersh, Meredith, Mummolo, and Nall (2018)



# vcovCluster.r 
# function to compute var-cov matrix using clustered robust standart errors
# inputs:
# model = model object from call to lm or glm
# cluster = vector with cluster ID indicators
# output:
# cluster robust var-cov matrix
# to call this for a model directly use:
# coeftest(model,vcov = vcovCluster(model, cluster))
# formula is similar to Stata's , cluster command
# NOTE: Remember to check whether estimating the model has caused groups to be dropped from the dataset
vcovCluster <- function(
                        model,
                        cluster
                        )
  {
require(sandwich)
require(lmtest)
if(nrow(model.matrix(model))!=length(cluster)){
  stop("check your data: cluster variable has different N than model")
}
M <- length(unique(cluster))
N <- length(cluster)           
K <- model$rank   
if(M<50){
  warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
}
dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
return(rcse.cov)
}


###############Replicating analysis of Virginia Turnout

##Same Result with HKL data
##dd<- read.delim('HKL_V2_data.tab', sep='\t')
##loading the HLN data
dd<- read.delim('HLN_orig.tab', sep='\t')


out<- which(dd$state=='Virginia')
vas<- table(dd$votegenval[out], dd$year[out])

turnout_rate<- vas[2,]/(vas[1,] + vas[2,])

plot(turnout_rate~c(2006,2007, 2008, 2009, 2010, 2011, 2012, 2014), xlab = 'Year', ylab = 'Turnout Rate', pch='', main = 'Virginia Turnout, HLN Data')

points(turnout_rate[c(1,3)]~c(2006, 2008), type='b', pch=20, lwd = 3)
points(turnout_rate[c(7,8)]~c(2012, 2014), type='b', pch=20, lwd = 3)

abline(v = 2010, lwd = 3)
arrows(2011, 0.4, 2010, 0.3, len = 0.1, lwd = 3)
text(2012, 0.425, 'Strict ID Law Passed', cex = 2)
dev.copy(device = pdf, file='VAPlot.pdf', height = 6 , width = 8)
dev.off()


###############Replicating analysis demonstrating Virginia's influence
####Note, that we stipulate to HLN and HKL's specification of the difference-in-differences
####Model, including newstrict and require individuals to be registered. 
####We use our corrected coding of state partisanship of governor, house, and senate (this does not affect results as we show below)
####We also do not worry about clustering standard errors because we're focusing on just
####effect estimates, not inference in this plot.  


##loading data, using HKL's replciation code
##same result using the data from HLN
dd<- read.delim('HKL_V2_data.tab', sep='\t')
##dd<- read.delim('HLN_orig.tab', sep='\t')
##correcting the coding of state partisanship 


new_rep_gov<- dd$rep_governor

new_rep_house<- dd$rep_house

new_rep_senate<- dd$rep_senate

##loading our spreadsheet with partisan control of legislatures, governor, by year, based on Ballotpedia
up_var<- read.delim('NewFixStates.csv', sep=',')

####for loop to remove NAs and correct mistakes 
for(z in 1:nrow(up_var)){
	new_rep_gov[which(dd$state==as.character(up_var[z,1]) & dd$year==up_var[z,2])]<- up_var[z,3]
}

for(z in 1:nrow(up_var)){
	new_rep_house[which(dd$state==as.character(up_var[z,1]) & dd$year==up_var[z,2])]<- up_var[z,5]
}

for(z in 1:nrow(up_var)){
	new_rep_senate[which(dd$state==as.character(up_var[z,1]) & dd$year==up_var[z,2])]<- up_var[z,4]
}



##identifying states
un_state<- unique(dd$state)
un_state<- sort(un_state)
states<- as.character(rev(un_state))

##storing the models
state_early<- list()

for(z in states){

##dropping one state at a time
drops<- rep(0, nrow(dd))
drops[which(dd$state==z)]<- 1

	temp<- lm(votegenval~stricty + newstrict+ blackstricty+ hispstricty + asianstricty+ mixedracestricty+ 
 + black + hispanic +  asian+  mixedrace+  foreignb+  firstgen + age + educ+  inc + male+  married + childrenz+  unionz + 
+  unemp +  ownhome+  protestant+  catholic + jewish + atheist+ 
+ days_before_election +  early_in_person +  vote_by_mail +  no_excuse_absence_ + 
+  presidentialelectionyear +   gubernatorialelectionyear  + senateelectionyear +  marginpnew + 
+  y2006+  y2008+  y2010+ new_rep_gov + new_rep_senate + new_rep_house  + as.factor(inputstate), subset= which( drops==0 & voteregpre==1), weight = weight, data = dd) 

	state_early[[z]]<- temp


print(z)
}

White<- Black<- Hispanic<- Asian<- c()

for(z in 1:len(states)){
	White[z]<- summary(state_early[[z]])$coefficients['stricty', 'Estimate'] #+ summary(state_early[[z]])$coefficients['blackstricty', 'Estimate']
	Black[z]<- summary(state_early[[z]])$coefficients['stricty', 'Estimate'] + summary(state_early[[z]])$coefficients['blackstricty', 'Estimate']
	Hispanic[z]<- summary(state_early[[z]])$coefficients['stricty', 'Estimate'] + summary(state_early[[z]])$coefficients['hispstricty', 'Estimate']
	Asian[z]<- summary(state_early[[z]])$coefficients['stricty', 'Estimate'] + summary(state_early[[z]])$coefficients['asianstricty', 'Estimate']

}

boxplot(cbind(White, Black, Hispanic, Asian), cex = 2,main = "Boxplot of Effect Estimates Shows \nVirginia Exerts Substantial Influence\non Estimated Effect Voter ID Laws", pch=20)
abline(h = 0, lwd = 3, lty = 2, col=gray(0.5))
arrows(1.1, -0.1, 1, -0.06, len = 0.1, lwd = 2)
text(1.1, -0.1, 'Estimate Dropping\nVirginia')


arrows(2.1, -0.05, 2, -0.093, len = 0.1, lwd = 2)
text(2.1, -0.04, 'Estimate Dropping\nVirginia')

arrows(3.1, -0.07, 3, -0.11, len = 0.1, lwd = 2)
text(3.1, -0.05, 'Estimate Dropping\nVirginia')

arrows(4.1, -0.04, 4, -0.083, len = 0.1, lwd = 2)
text(4.1, -0.03, 'Estimate Dropping\nVirginia')
dev.copy(device=pdf, file='DropOneStateBoxPlot.pdf', height = 6, width = 8)
dev.off()



###################Replicating analysis showing effect of various specifications

###################This analysis uses HLN's original model and then shows how 
###################various changes affect the coefficient
###################We consider the specifications sequentially. The changes are cumulative, 
###################but this does not alter the conclusions

##Loading the data from HKL 
dd<- read.delim('HKL_V2_data.tab', sep='\t')

eff_estimates<- matrix(NA, nrow = 5, ncol = 6)
se_estimates<- matrix(NA, nrow = 5, ncol = 6)
###Replication Column 1, Table A9 HLN
model1<- lm(votegenval~stricty + black + hispanic + asian + mixedrace + blackstricty + hispstricty + asianstricty +  mixedracestricty + y2006 + y2008 + y2010 + as.factor(state) + foreignb+ firstgen +  age +  educ +  inc +  male +  married + childrenz + unionz + unemp +  ownhome +    protestant + catholic  + jewish + atheist +  days_before_election + 
					early_in_person + vote_by_mail + no_excuse_absence_ + presidentialelectionyear +    gubernatorialelectionyear + senateelectionyear + marginpnew + newstrict, data = dd, subset = which(voteregpre==1) )

eff_estimates[1,1]<-  summary(model1)$coefficients['stricty', 'Estimate']
eff_estimates[2,1]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['blackstricty', 'Estimate']
eff_estimates[3,1]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['hispstricty', 'Estimate']
eff_estimates[4,1]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['asianstricty', 'Estimate']
eff_estimates[5,1]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['mixedracestricty', 'Estimate']

##standard errors 
v_model<- vcov(model1) 

se_estimates[1,1]<- sqrt(v_model['stricty', 'stricty'])
se_estimates[2,1]<- sqrt(v_model['stricty', 'stricty'] + v_model['blackstricty', 'blackstricty'] + 2*v_model['stricty', 'blackstricty'])
se_estimates[3,1]<- sqrt(v_model['stricty', 'stricty'] + v_model['hispstricty', 'hispstricty'] + 2*v_model['stricty', 'hispstricty'])
se_estimates[4,1]<- sqrt(v_model['stricty', 'stricty'] + v_model['asianstricty', 'asianstricty'] + 2*v_model['stricty', 'asianstricty'])
se_estimates[5,1]<- sqrt(v_model['stricty', 'stricty'] + v_model['mixedracestricty', 'mixedracestricty'] + 2*v_model['stricty', 'mixedracestricty'])




###Clustering Standard errors

eff_estimates[1,2]<-  summary(model1)$coefficients['stricty', 'Estimate']
eff_estimates[2,2]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['blackstricty', 'Estimate']
eff_estimates[3,2]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['hispstricty', 'Estimate']
eff_estimates[4,2]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['asianstricty', 'Estimate']
eff_estimates[5,2]<- summary(model1)$coefficients['stricty', 'Estimate'] + summary(model1)$coefficients['mixedracestricty', 'Estimate']

##clustering, standard errors
##sub out the clustering
v_model<- vcov(model1) 

se_estimates[1,2]<- sqrt(v_model['stricty', 'stricty'])
se_estimates[2,2]<- sqrt(v_model['stricty', 'stricty'] + v_model['blackstricty', 'blackstricty'] + 2*v_model['stricty', 'blackstricty'])
se_estimates[3,2]<- sqrt(v_model['stricty', 'stricty'] + v_model['hispstricty', 'hispstricty'] + 2*v_model['stricty', 'hispstricty'])
se_estimates[4,2]<- sqrt(v_model['stricty', 'stricty'] + v_model['asianstricty', 'asianstricty'] + 2*v_model['stricty', 'asianstricty'])
se_estimates[5,2]<- sqrt(v_model['stricty', 'stricty'] + v_model['mixedracestricty', 'mixedracestricty'] + 2*v_model['stricty', 'mixedracestricty'])

###Including survey weights 
model2<- lm(votegenval~stricty + black + hispanic + asian + mixedrace + blackstricty + hispstricty + asianstricty +  mixedracestricty + y2006 + y2008 + y2010 + as.factor(state) + foreignb+ firstgen +  age +  educ +  inc +  male +  married + childrenz + unionz + unemp +  ownhome +    protestant + catholic  + jewish + atheist +  days_before_election + 
					early_in_person + vote_by_mail + no_excuse_absence_ + presidentialelectionyear +    gubernatorialelectionyear + senateelectionyear + marginpnew + newstrict, data = dd, subset = which(voteregpre==1), weight = weight )



eff_estimates[1,3]<-  summary(model2)$coefficients['stricty', 'Estimate']
eff_estimates[2,3]<- summary(model2)$coefficients['stricty', 'Estimate'] + summary(model2)$coefficients['blackstricty', 'Estimate']
eff_estimates[3,3]<- summary(model2)$coefficients['stricty', 'Estimate'] + summary(model2)$coefficients['hispstricty', 'Estimate']
eff_estimates[4,3]<- summary(model2)$coefficients['stricty', 'Estimate'] + summary(model2)$coefficients['asianstricty', 'Estimate']
eff_estimates[5,3]<- summary(model2)$coefficients['stricty', 'Estimate'] + summary(model2)$coefficients['mixedracestricty', 'Estimate']

##
v_model<- vcov(model2) 

se_estimates[1,3]<- sqrt(v_model['stricty', 'stricty'])
se_estimates[2,3]<- sqrt(v_model['stricty', 'stricty'] + v_model['blackstricty', 'blackstricty'] + 2*v_model['stricty', 'blackstricty'])
se_estimates[3,3]<- sqrt(v_model['stricty', 'stricty'] + v_model['hispstricty', 'hispstricty'] + 2*v_model['stricty', 'hispstricty'])
se_estimates[4,3]<- sqrt(v_model['stricty', 'stricty'] + v_model['asianstricty', 'asianstricty'] + 2*v_model['stricty', 'asianstricty'])
se_estimates[5,3]<- sqrt(v_model['stricty', 'stricty'] + v_model['mixedracestricty', 'mixedracestricty'] + 2*v_model['stricty', 'mixedracestricty'])




###Conditioning on political variables.
###First, we have to clean the variables.  

new_rep_gov<- dd$rep_governor

new_rep_house<- dd$rep_house

new_rep_senate<- dd$rep_senate

##loading our spreadsheet with partisan control of legislatures, governor, by year, based on Ballotpedia
up_var<- read.delim('NewFixStates.csv', sep=',')

####for loop to remove NAs and correct mistakes 
for(z in 1:nrow(up_var)){
	new_rep_gov[which(dd$state==as.character(up_var[z,1]) & dd$year==up_var[z,2])]<- up_var[z,3]
}

for(z in 1:nrow(up_var)){
	new_rep_house[which(dd$state==as.character(up_var[z,1]) & dd$year==up_var[z,2])]<- up_var[z,5]
}

for(z in 1:nrow(up_var)){
	new_rep_senate[which(dd$state==as.character(up_var[z,1]) & dd$year==up_var[z,2])]<- up_var[z,4]
}


###running the regression with political controls

model3<- lm(votegenval~stricty + black + hispanic + asian + mixedrace + blackstricty + hispstricty + asianstricty +  mixedracestricty + y2006 + y2008 + y2010 + as.factor(state) + foreignb+ firstgen +  age +  educ +  inc +  male +  married + childrenz + unionz + unemp +  ownhome +    protestant + catholic  + jewish + atheist +  days_before_election + 
					early_in_person + vote_by_mail + no_excuse_absence_ + presidentialelectionyear +    gubernatorialelectionyear + senateelectionyear 
						+ marginpnew + newstrict + new_rep_gov + new_rep_house + new_rep_senate, data = dd, subset = which(voteregpre==1), weight = weight )

eff_estimates[1,4]<-  summary(model3)$coefficients['stricty', 'Estimate']
eff_estimates[2,4]<- summary(model3)$coefficients['stricty', 'Estimate'] + summary(model3)$coefficients['blackstricty', 'Estimate']
eff_estimates[3,4]<- summary(model3)$coefficients['stricty', 'Estimate'] + summary(model3)$coefficients['hispstricty', 'Estimate']
eff_estimates[4,4]<- summary(model3)$coefficients['stricty', 'Estimate'] + summary(model3)$coefficients['asianstricty', 'Estimate']
eff_estimates[5,4]<- summary(model3)$coefficients['stricty', 'Estimate'] + summary(model3)$coefficients['mixedracestricty', 'Estimate']

###Standard errors

v_model<- vcov(model3) 

se_estimates[1,4]<- sqrt(v_model['stricty', 'stricty'])
se_estimates[2,4]<- sqrt(v_model['stricty', 'stricty'] + v_model['blackstricty', 'blackstricty'] + 2*v_model['stricty', 'blackstricty'])
se_estimates[3,4]<- sqrt(v_model['stricty', 'stricty'] + v_model['hispstricty', 'hispstricty'] + 2*v_model['stricty', 'hispstricty'])
se_estimates[4,4]<- sqrt(v_model['stricty', 'stricty'] + v_model['asianstricty', 'asianstricty'] + 2*v_model['stricty', 'asianstricty'])
se_estimates[5,4]<- sqrt(v_model['stricty', 'stricty'] + v_model['mixedracestricty', 'mixedracestricty'] + 2*v_model['stricty', 'mixedracestricty'])


###Finally, dropping Virginia and 2006 observations
good_obs<- rep(1, nrow(dd))
good_obs[which(dd$year==2006)]<- 0
good_obs[which(dd$year==2008 & dd$state=='Virginia')]<- 0 


###running the regression

model4<- lm(votegenval~stricty + black + hispanic + asian + mixedrace + blackstricty + hispstricty + asianstricty +  mixedracestricty + y2006 + y2008 + y2010 + as.factor(state) + foreignb+ firstgen +  age +  educ +  inc +  male +  married + childrenz + unionz + unemp +  ownhome +    protestant + catholic  + jewish + atheist +  days_before_election + 
					early_in_person + vote_by_mail + no_excuse_absence_ + presidentialelectionyear +    gubernatorialelectionyear + senateelectionyear 
						+ marginpnew + newstrict + new_rep_gov + new_rep_house + new_rep_senate, data = dd, subset = which(voteregpre==1 & good_obs==1), weight = weight )


eff_estimates[1,5]<-  summary(model4)$coefficients['stricty', 'Estimate']
eff_estimates[2,5]<- summary(model4)$coefficients['stricty', 'Estimate'] + summary(model4)$coefficients['blackstricty', 'Estimate']
eff_estimates[3,5]<- summary(model4)$coefficients['stricty', 'Estimate'] + summary(model4)$coefficients['hispstricty', 'Estimate']
eff_estimates[4,5]<- summary(model4)$coefficients['stricty', 'Estimate'] + summary(model4)$coefficients['asianstricty', 'Estimate']
eff_estimates[5,5]<- summary(model4)$coefficients['stricty', 'Estimate'] + summary(model4)$coefficients['mixedracestricty', 'Estimate']



v_model<- vcov(model4) 

se_estimates[1,5]<- sqrt(v_model['stricty', 'stricty'])
se_estimates[2,5]<- sqrt(v_model['stricty', 'stricty'] + v_model['blackstricty', 'blackstricty'] + 2*v_model['stricty', 'blackstricty'])
se_estimates[3,5]<- sqrt(v_model['stricty', 'stricty'] + v_model['hispstricty', 'hispstricty'] + 2*v_model['stricty', 'hispstricty'])
se_estimates[4,5]<- sqrt(v_model['stricty', 'stricty'] + v_model['asianstricty', 'asianstricty'] + 2*v_model['stricty', 'asianstricty'])
se_estimates[5,5]<- sqrt(v_model['stricty', 'stricty'] + v_model['mixedracestricty', 'mixedracestricty'] + 2*v_model['stricty', 'mixedracestricty'])


##now moving closer to our preferred specification
##replacing missing with zero and not conditioning on prereg

fix_vote<- ifelse(is.na(dd$votegenval)==T, 0, dd$votegenval)
model5<- lm(fix_vote~stricty + black + hispanic + asian + mixedrace + blackstricty + hispstricty + asianstricty +  mixedracestricty + y2006 + y2008 + y2010 + as.factor(state) + foreignb+ firstgen +  age +  educ +  inc +  male +  married + childrenz + unionz + unemp +  ownhome +    protestant + catholic  + jewish + atheist +  days_before_election + 
					early_in_person + vote_by_mail + no_excuse_absence_ + presidentialelectionyear +    gubernatorialelectionyear + senateelectionyear 
						+ marginpnew  + new_rep_gov + new_rep_house + new_rep_senate, data = dd, subset = which(good_obs==1), weight = weight )

eff_estimates[1,6]<-  summary(model5)$coefficients['stricty', 'Estimate']
eff_estimates[2,6]<- summary(model5)$coefficients['stricty', 'Estimate'] + summary(model5)$coefficients['blackstricty', 'Estimate']
eff_estimates[3,6]<- summary(model5)$coefficients['stricty', 'Estimate'] + summary(model5)$coefficients['hispstricty', 'Estimate']
eff_estimates[4,6]<- summary(model5)$coefficients['stricty', 'Estimate'] + summary(model5)$coefficients['asianstricty', 'Estimate']
eff_estimates[5,6]<- summary(model5)$coefficients['stricty', 'Estimate'] + summary(model5)$coefficients['mixedracestricty', 'Estimate']



v_model<- vcov(model5) 

se_estimates[1,6]<- sqrt(v_model['stricty', 'stricty'])
se_estimates[2,6]<- sqrt(v_model['stricty', 'stricty'] + v_model['blackstricty', 'blackstricty'] + 2*v_model['stricty', 'blackstricty'])
se_estimates[3,6]<- sqrt(v_model['stricty', 'stricty'] + v_model['hispstricty', 'hispstricty'] + 2*v_model['stricty', 'hispstricty'])
se_estimates[4,6]<- sqrt(v_model['stricty', 'stricty'] + v_model['asianstricty', 'asianstricty'] + 2*v_model['stricty', 'asianstricty'])
se_estimates[5,6]<- sqrt(v_model['stricty', 'stricty'] + v_model['mixedracestricty', 'mixedracestricty'] + 2*v_model['stricty', 'mixedracestricty'])



##putting together the plot now
par(mfrow = c(1, 5))
par(mar = c(5, 5.5, 3, 4))
par(las = 1)
plot(c(0,1), c(0,1), xlim=c(-0.15, 0.15), xlab = 'Effect Estimate', ylab = '', ylim=c(1,5), axes = F, frame.plot=T)
axis(1, seq(-0.15, 0.15, by = 0.05))
title(main = 'White')
axis(2, c(6,5, 4, 3, 2, 1), c('HLN Table\nA9', '+Clustering\nSEs', '+Survey\nWeights', '+Political\nControls', '+Drop 06\nVA 08', '+No Match as\nNon-Voter'))
abline(v = 0)

##draw arrow
draws<- function(row, col, ylim){
	points(eff_estimates[row, col], ylim, cex = 1.25, pch = 20)
	arrows(eff_estimates[row, col] - 1.96*se_estimates[row,col], ylim, eff_estimates[row, col]  + 1.96*se_estimates[row,col], ylim, len = 0 , lwd = 2)

}
draws(1, 1, 5)
draws(1, 2, 4)
draws(1, 3, 3)
draws(1, 4, 2)
draws(1, 5, 1)



par(mar = c(5, 2.5, 3, 1))
par(las = 1)
plot(c(0,1), c(0,1), xlim=c(-0.15, 0.15), xlab = 'Effect Estimate', ylab = '', ylim=c(1,6), axes = F, frame.plot=T)
axis(1, seq(-0.15, 0.15, by = 0.05))
title(main = 'Black')
axis(2, c(6, 5, 4, 3, 2, 1), c('', '', '', '', '', ''))#, c('HLN Table\nA9', '+Clustering\nSEs', '+Survey\nWeights', '+Political\nControls', '+Drop 06\nVA 08'))
abline(v = 0)

##conf ints
draws(2, 1, 5)
draws(2, 2, 4)
draws(2, 3, 3)
draws(2, 4, 2)
draws(2, 5, 1)


par(mar = c(5, 2.5, 3, 1))
par(las = 1)
plot(c(0,1), c(0,1), xlim=c(-0.15, 0.15), xlab = 'Effect Estimate', ylab = '', ylim=c(1,6), axes = F, frame.plot=T)
axis(1, seq(-0.15, 0.15, by = 0.05))
title(main = 'Latino')
axis(2, c(5, 4, 3, 2, 1), c('', '', '', '', ''))#, c('HLN Table\nA9', '+Clustering\nSEs', '+Survey\nWeights', '+Political\nControls', '+Drop 06\nVA 08'))
abline(v = 0)

##conf ints
draws(3, 1, 5)
draws(3, 2, 4)
draws(3, 3, 3)
draws(3, 4, 2)
draws(3, 5, 1)


par(mar = c(5, 2.5, 3, 1))
par(las = 1)
plot(c(0,1), c(0,1), xlim=c(-0.15, 0.15), xlab = 'Effect Estimate', ylab = '', ylim=c(1,6), axes = F, frame.plot=T)
axis(1, seq(-0.15, 0.15, by = 0.05))
title(main = 'Asian')
axis(2, c(5, 4, 3, 2, 1), c('', '', '', '', ''))#, c('HLN Table\nA9', '+Clustering\nSEs', '+Survey\nWeights', '+Political\nControls', '+Drop 06\nVA 08'))
abline(v = 0)

##conf ints
draws(4, 1, 5)
draws(4, 2, 4)
draws(4, 3, 3)
draws(4, 4, 2)
draws(4, 5, 1)



par(mar = c(5, 2.5, 3, 1))
par(las = 1)
plot(c(0,1), c(0,1), xlim=c(-0.15, 0.15), xlab = 'Effect Estimate', ylab = '', ylim=c(1,6), axes = F, frame.plot=T)
axis(1, seq(-0.15, 0.15, by = 0.05))
title(main = 'Mixed Race')
axis(2, c(5, 4, 3, 2, 1), c('', '', '', '', ''))#, c('HLN Table\nA9', '+Clustering\nSEs', '+Survey\nWeights', '+Political\nControls', '+Drop 06\nVA 08'))
abline(v = 0)
###conf ints
draws(5, 1, 5)
draws(5, 2, 4)
draws(5, 3, 3)
draws(5, 4, 2)
draws(5, 5, 1)
