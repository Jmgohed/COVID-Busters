# Import required packages:
Packages <- c('dplyr', 'rcompanion', 'car', 'IDPmisc')
lapply(Packages, library, character.only = TRUE)

# Step 1.Read in data set from external csv file:
cdcConditions <- read.csv("/Users/reubenolumese/Desktop/final project/CDCconditions.csv", header = TRUE)
head(cdcConditions, n=20)

# Question Setup: Determine if there is a difference in the number of deaths (Number.of.COVID.19.Deaths) between all the conditions (Condition)? Note that the 
# DV here is of course the number of deaths which is continuous and has the one level. The IV, on the other hand, is the Condition variable which is categorical,
# and has multiple levels.

#--------------------------------------------------------- 2. Data Wrangling----------------------------------------------------------
# First filter out all the rows where Age Group is 'All ages' (as their corresponding death counts are invalid for the 
# question being analysed), as well as all 'zeros' in the Number.of.COVID.19.Deaths variable as zeros become '-inf' and throws a not finite error later on.
cdcConditions2 <- cdcConditions %>% filter((cdcConditions$Age.Group != 'All Ages') & (cdcConditions$Number.of.COVID.19.Deaths !=0))
head(cdcConditions2, n=20) 

# Secondly, drop all rows equal to 'blanks'.
cdcConditions3 <- NaRV.omit(cdcConditions2)

#--------------------------------------------------------- 3. TEST ASSUMPTIONS ----------------------------------------------------------
# If the assumptions are not met for ANOVA, but you proceeded anyway, you run the risk of biasing your results.

# Assumption 1 - Normality
# You only need to test for the normality of the dependent variable (Number.of.COVID.19.Deaths), since the IV (Condition) is categorical.
plotNormalHistogram(cdcConditions3$Number.of.COVID.19.Deaths)
# Result: Histogram is positively skewed, so use square root and/or log to correct it.

# Square Root:
cdcConditions3$Number.of.COVID.19.DeathsSQRT <- sqrt(cdcConditions3$Number.of.COVID.19.Deaths)
plotNormalHistogram(cdcConditions3$Number.of.COVID.19.DeathsSQRT)
# Result: Marginal improvement but not good enough. Let's try log.

# Log:
cdcConditions3$Number.of.COVID.19.DeathsLOG <- log(cdcConditions3$Number.of.COVID.19.Deaths)
plotNormalHistogram(cdcConditions3$Number.of.COVID.19.DeathsLOG)
# Result: Log is by far the best of the three, so we'll stick with it.


# Assumption 2 - Homogeneity of Variance
# Bartlett's Test:
bTest <- bartlett.test(cdcConditions3$Number.of.COVID.19.DeathsLOG ~ Condition, data=cdcConditions3)
bTest
# The p value associated with this test is < .05, which means that unfortunately, you have violated the assumption of homogeneity of variance.

# Test with Unequal Variance
ANOVA <- lm(cdcConditions3$Number.of.COVID.19.DeathsLOG ~ Condition, data=cdcConditions3)
Anova(ANOVA, Type="II", white.adjust=TRUE)

# Post Hocs with Unequal Variance
pairwise.t.test(cdcConditions3$Number.of.COVID.19.DeathsLOG, cdcConditions3$Condition, p.adjust="bonferroni", pool.sd = FALSE)


# Determine Means and Draw Conclusions
cdcConditions3Means <- cdcConditions3 %>% group_by(Condition) %>% summarize(Mean = mean(Number.of.COVID.19.Deaths))
cdcConditions3Means

# Result: # The condition differ in varying degrees from each other in terms of their related number of COVID-19 deaths. The differences are
# quite significant in some cases (E.g. Cerebrovascular diseases compared to Diabetes) and others less so. The higher means of course equate to
# higher death counts. 























