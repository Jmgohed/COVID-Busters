# Import required packages:
Packages <- c('dplyr', 'rcompanion', 'car', 'IDPmisc')
lapply(Packages, library, character.only = TRUE)

# Step 1.Read in data set from external csv file:
cdcConditions <- read.csv("CDCconditions.csv", header = TRUE)
head(cdcConditions, n=20)

# Question Setup: Determine if there is a difference in the number of deaths (Number.of.COVID.19.Deaths) between all the conditions (Condition)? Note that the 
# DV here is of course the number of deaths which is continuous and has the one level. The IV, on the other hand, is the Condition variable which is categorical,
# and has multiple levels.

#--------------------------------------------------------- 2. Data Wrangling----------------------------------------------------------
####################
####################
####################
##  This analysis uses the "All Ages" field so that we look at the deaths as a whole without migrating through between age groups

####  For this set I have taken out the conditions which will skew our data.  We are looking at health conditions, 
####  not random accidents, not "all other causes" (too vague), and not COVID-19 because all these deaths are COVID related.
####  we also want to take out the US data because that will over inflate our values because it is split by state too.
cdcConditions3 <- cdcConditions %>% filter((cdcConditions$Age.Group == 'All Ages') & 
                                             (cdcConditions$State != 'US') &
                                             (cdcConditions$Condition != 'Intentional and unintentional injury, poisoning, and other adverse events') &
                                             (cdcConditions$Condition != 'All other conditions and causes (residual)') &
                                             (cdcConditions$Condition != 'COVID-19'))
head(cdcConditions3, n=20) 

### removing NA values from DV
cdcConditions.nona <- NaRV.omit(cdcConditions3)

## this is positively skewed. 
plotNormalHistogram(cdcConditions.nona$Number.of.COVID.19.Deaths)

## Here is the LOG transformation
cdcConditions.nona$Number.of.COVID.19.DeathsLOG <- log(cdcConditions.nona$Number.of.COVID.19.Deaths)
plotNormalHistogram(cdcConditions.nona$Number.of.COVID.19.DeathsLOG)


### FLIGNERS test due to data not being normalized from start.
flignerTest <- fligner.test(Number.of.COVID.19.DeathsLOG ~ Condition, data=cdcConditions.nona)
flignerTest

### this is not significant, SO WE PASS HOMOGENEITY OF VARIANCE!!***

# Analysis
ConditionsANOVA <- aov(cdcConditions.nona$Number.of.COVID.19.Deaths ~ cdcConditions.nona$Condition)
summary(ConditionsANOVA)
## WE HAVE SIGNIFICANCE!!

# post hocs and drawing conclusions
pairwise.t.test(cdcConditions.nona$Number.of.COVID.19.Deaths, cdcConditions.nona$Condition, p.adjust="bonferroni")

cdcConditions3Means <- cdcConditions.nona %>% group_by(Condition) %>% summarize(Mean = mean(Number.of.COVID.19.Deaths))
cdcConditions3Means

## The group Influenza and Pneumonia is significantly different compared to all other groups except Respiratory failure.
## Respiratory failure looks to be standing itself out from the rest of each group except hypertensive diseases and influenza/pneumonia
##

## Because of the significance, we can say the symptoms of Influenza and Pneumonia and 
## Respiratory failure have the highest probability of leading to a COVID death.

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################

## Extra code from Devin that shows us some clean tables and frequencies...

State.group.count <- cdcConditions3 %>% group_by(cdcConditions3$State) %>% summarize(count=n())

IV.group.count <- cdcConditions3 %>% group_by(cdcConditions3$Condition) %>% summarize(count=n())

cdc.IV.DV <- cdcConditions3[, c("Condition","Number.of.COVID.19.Deaths")] 

which(is.na(cdc.IV.DV$Condition))

NA.DV <- cdc.IV.DV[which(is.na(cdc.IV.DV$Number.of.COVID.19.Deaths)),]

NA.DV %>% group_by(Condition) %>% summarize(count=n())

NA.DV.all <- cdcConditions3[which(is.na(cdcConditions3$Number.of.COVID.19.Deaths)),]

NA.DV.all %>% group_by(State) %>% summarize(count=n())





