#### Purchases of Ice Cream
ice = read.csv("ice_cream.csv")

## explore
names(ice)

## create a new variable for price per unit
priceper1 = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity
y <- log(1 + priceper1)

## collect some variables of interest
## create a data.frame
x <- ice[ ,c("flavor_descr", "size1_descr", "household_income", "household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr, "VAN")
## coupon usage
x$usecoup = factor(ice$coupon_value > 0)
x$couponper1 <- ice$coupon_value / ice$quantity
## organize some demographics
x$region <- factor(ice$region, levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(ice$marital_status==1)
x$race <- factor(ice$race, levels=1:4, labels=c("white", "black", "asian", "other"))
x$hispanic_origin <- ice$hispanic_origin==1
x$microwave <- ice$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- ice$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- ice$type_of_residence == 1
x$internet <- ice$household_internet_connection == 1
x$tvcable <- ice$tv_items > 1

## combine x and y
## cbind is "column bind".  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- glm(y~., data=xy) 

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 

## source the fdr_cut function
source("fdr.R")




summary(fit)
cat("Two variables are not significant and AIC of this model is -21721.")

#use stepwise algorithm to improve the model.
tstep <- step(fit)
summary(tstep)
cat("According to the result, two variables are removed, including dishwasher and hispanic_origin. After the removal, the AIC reaches the lowest(-21724).")

#add some possibly significant variables
age_of_male_head <- factor(ice$age_of_male_head)
age_of_female_head <- factor(ice$age_of_female_head)
male_head_employment <- factor(ice$male_head_employment)
female_head_employment <- factor(ice$female_head_employment)
male_head_education <- factor(ice$male_head_education)
female_head_education <- factor(ice$female_head_education)
male_head_occupation <- factor(ice$male_head_occupation)
female_head_occupation <- factor(ice$female_head_occupation)
age_and_presence_of_children <- factor(ice$age_and_presence_of_children)

#create new dataframe, build model and use stepwise to improve it
xy_added <- cbind(xy,ice$formula_descr, age_of_female_head, age_of_male_head, age_and_presence_of_children, male_head_employment, female_head_employment, male_head_education, female_head_education, male_head_occupation, female_head_occupation)
fit_added <- glm(y~., data=xy_added) 
summary(fit_added)
added <- step(fit_added)
summary(added)

#retain some significant variables:
#age_of_male_head
#age_and_presence_of_children
#male_head_employment
#female_head_education
#male_head_occupation
#female_head_occupation(removed, because of large proportion of missing value)

miss <- list()
for (i in 1:26) {
  a <- xy_added[,i]
  miss[[i]] <- sum(is.na(a))/length(a)
  i=i+1
}
str(miss)
cat("The proportion of missing value ofthe 26th variable is larger than 10%, so even if the female_head_occupation variable is significant, we still remove this variable.")

fit_added1 <- glm(y~internet+tvcable+sfh+race+married+household_size+microwave+region+flavor_descr+household_income+usecoup+couponper1+size1_descr+age_of_male_head+male_head_employment+female_head_education+male_head_occupation+age_and_presence_of_children, data=xy_added) 
added1 <- step(fit_added1)
summary(added1)
#After adding five variables, AIC becomes lower(-21873).

# also use R-square to test the model
library(rsq)
rsq(fit,adj=T,type=c('sse'))
rsq(fit_added,adj=T,type=c('sse'))
rsq(fit_added1,adj=T,type=c('sse'))
#Compared with R-square of the original model, R-square of the new model increases.

#after FDR control, number of variables that are significant
pvals_added1 <- summary(fit_added1)$coef[-1,4] 
fd <- fdr(pvals_added1, 0.01)
sum(ifelse(pvals_added1 < fd, 1, 0))
