## load in libraries

library("dplyr")
library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
library("psych")


## Question setup: 
## Controlling for State, does the type of health condition increase the chance for a COVID related fatality?


## read in dataset
cdcConditions <- read.csv("CDCconditions.csv", header = TRUE)
head(cdcConditions, n=20)


## filter dataset

#### For this set I have taken out the conditions which will skew our data.  We are looking at health conditions, 
#### not random accidents, not "all other causes" (too vague), and not COVID-19 because all these deaths are COVID related.
#### we also want to take out the US data because that will over inflate our values because it is split by state too. Also remove PR and YC.

## looking at all ages by state.  (((NOT DIFFERENT AGE GROUPS)))
cdc.con <- cdcConditions %>% filter((cdcConditions$Age.Group == 'All Ages') & 
                                      (cdcConditions$State != 'US') &
                                      (cdcConditions$State != 'PR') &
                                      (cdcConditions$State != 'YC') &
                                      (cdcConditions$Condition != 'Intentional and unintentional injury, poisoning, and other adverse events') &
                                      (cdcConditions$Condition != 'All other conditions and causes (residual)') &
                                      (cdcConditions$Condition != 'COVID-19'))

## remove NAs
cdc.con1 <- NaRV.omit(cdc.con)




# covariates
str(cdc.con1$State)
str(cdc.con1$Age.Group)
# IV
str(cdc.con1$Condition)

# setting them up as factors
cdc.con1$State <- as.factor(cdc.con1$State)
cdc.con1$Age.Group <- as.factor(cdc.con1$Age.Group)
cdc.con1$Condition <- as.factor(cdc.con1$Condition)


# Test Assumptions 

## Normality

## cdc.con1 data
plotNormalHistogram(cdc.con1$Number.of.COVID.19.Deaths)
cdc.con1$Number.of.COVID.19.Deaths.LOG <- log(cdc.con1$Number.of.COVID.19.Deaths)
plotNormalHistogram(cdc.con1$Number.of.COVID.19.Deaths.LOG)

## Homogeneity of Variance

leveneTest(Number.of.COVID.19.Deaths.LOG~Condition, data = cdc.con1)
## not significant, WE PASS HOMOGENEITY OF VARIANCE!

homogeneity_RegrSlp = lm(Number.of.COVID.19.Deaths.LOG~State, data = cdc.con1)
anova(homogeneity_RegrSlp)

## we have significance, so this means state should NOT be a covariate, but we can however look at this as another IV





# Back to ANOVA test
## 
fTEST <- fligner.test(Number.of.COVID.19.Deaths.LOG ~ State, data = cdc.con1)
fTEST

cdcANOVA <- aov(cdc.con1$Number.of.COVID.19.Deaths ~ cdc.con1$State)
summary(cdcANOVA)

pairwise.t.test(cdc.con1$Number.of.COVID.19.Deaths, cdc.con1$State, p.adjust="bonferroni")
## this omitted 30 rows

## CA, FL, and some of IL seem to be significant compared to the other states
# Let's see if there are more states

cdc.statedeath.means <- cdc.con1 %>% group_by(State) %>% summarize(Mean = mean(Number.of.COVID.19.Deaths))
View(cdc.statedeath.means)

## Looking at the means you can see that CA, TX, FL, NJ, and NY are the top 5 states with significantly higher COVID fatalities.






####################################################################################
###########  Let's look at the ratios of Conditions each of these top 5 states has
target.states <- c('CA', 'TX', 'FL', 'NJ', 'NY')

cdcbystate <- cdcConditions %>% filter((cdcConditions$Age.Group == 'All Ages') & 
                                         (State %in% target.states) &
                                         (cdcConditions$Condition != 'Intentional and unintentional injury, poisoning, and other adverse events') &
                                         (cdcConditions$Condition != 'All other conditions and causes (residual)') &
                                         (cdcConditions$Condition != 'COVID-19'))

cdc.state.IV.DV <- cdcbystate[, c("State", "Condition","Number.of.COVID.19.Deaths")] 

cdc.state.IV.DV.expanded <- cdc.state.IV.DV[rep(row.names(cdc.state.IV.DV), cdc.state.IV.DV$Number.of.COVID.19.Deaths), 1:2]
View(cdc.state.IV.DV.expanded)

CrossTable(cdc.state.IV.DV.expanded$State, cdc.state.IV.DV.expanded$Condition, fisher = TRUE, 
           chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
#####  data is too big hahahaha
#########################################################################################################






## Code below this line is irrelevant.  Nothing significant because of the integrity of the dataset we are working with
## We would need a better way to transform the data and make it normalized without taking out almost half the data.


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

# Here we are looking at even less data to see if the ANCOVA works by taking out the zeros as well
# But we are testing Age group as a covariate

## this filtering shows individual age groups and filters out zeros
cdc.0 <- cdcConditions %>% filter((cdcConditions$Age.Group != 'All Ages') & 
                                    (cdcConditions$State != 'US') &
                                    (cdcConditions$State != 'PR') &
                                    (cdcConditions$State != 'YC') &
                                    (cdcConditions$Number.of.COVID.19.Deaths != 0) &
                                    (cdcConditions$Condition != 'Intentional and unintentional injury, poisoning, and other adverse events') &
                                    (cdcConditions$Condition != 'All other conditions and causes (residual)') &
                                    (cdcConditions$Condition != 'COVID-19'))


# covariates
str(cdc.0$State)
str(cdc.0$Age.Group)
# IV
str(cdc.0$Condition)

# setting them up as factors
cdc.0$State <- as.factor(cdc.0$State)
cdc.0$Age.Group <- as.factor(cdc.0$Age.Group)
cdc.0$Condition <- as.factor(cdc.0$Condition)




# Test Assumptions 

## Normality

## 
plotNormalHistogram(cdc.0$Number.of.COVID.19.Deaths)
cdc.0$Number.of.COVID.19.Deaths.LOG <- log(cdc.0$Number.of.COVID.19.Deaths)
plotNormalHistogram(cdc.0$Number.of.COVID.19.Deaths.LOG)


## Homogeneity of Variance

leveneTest(Number.of.COVID.19.Deaths.LOG~Condition, data = cdc.0)
##  significant, WE FAIL HOMOGENEITY OF VARIANCE!


homogeneity_RegrSlp = lm(Number.of.COVID.19.Deaths.LOG~Age.Group, data = cdc.0)
anova(homogeneity_RegrSlp)
## we have significance, so this means state should NOT be a covariate, but we can however look at this as another IV

## this begins to run the same analysis as above.




################################################################################
################################################################################
# the difference here is going to be not filtering out the zeros

cdc.1 <- cdcConditions %>% filter((cdcConditions$Age.Group != 'All Ages') & 
                                    (cdcConditions$State != 'US') &
                                    (cdcConditions$State != 'PR') &
                                    (cdcConditions$State != 'YC') &
                                    (cdcConditions$Condition != 'Intentional and unintentional injury, poisoning, and other adverse events') &
                                    (cdcConditions$Condition != 'All other conditions and causes (residual)') &
                                    (cdcConditions$Condition != 'COVID-19'))

#remove Na's
cdc.11 <- NaRV.omit(cdc.1)
## this removal of NA's cuts our dataset observations by almost 1/3

plotNormalHistogram(cdc.11$Number.of.COVID.19.Deaths)
cdc.11$Number.of.COVID.19.Deaths.LOG <- (cdc.11$Number.of.COVID.19.Deaths)**(1/20)
plotNormalHistogram(cdc.11$Number.of.COVID.19.Deaths.LOG)




# Back to ANOVA test
## 
f2TEST <- fligner.test(Number.of.COVID.19.Deaths.LOG ~ Age.Group, data = cdc.11)
f2TEST

cdcANOVA <- aov(cdc.11$Number.of.COVID.19.Deaths ~ cdc.11$Age.Group)
summary(cdcANOVA)

pairwise.t.test(cdc.11$Number.of.COVID.19.Deaths, cdc.11$Age.Group, p.adjust="bonferroni")
