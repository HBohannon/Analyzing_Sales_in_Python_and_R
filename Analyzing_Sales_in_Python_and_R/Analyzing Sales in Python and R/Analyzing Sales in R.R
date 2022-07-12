# PART 1:  ANOVAs in R

# Load Libraries

library("dplyr")
library("rcompanion")
library("car")
library("IDPmisc")

#Question: Does the average price of avocados differ between Albany, Houston, and Seattle?



#Test Assumptions

plotNormalHistogram(avocados$AveragePrice)

## SQRT

avocados$AveragePriceSQRT <- sqrt(avocados$AveragePrice)
plotNormalHistogram(avocados$AveragePriceSQRT)

## Log

avocados$AveragePriceLog <- log(avocados$AveragePrice)
plotNormalHistogram(avocados$AveragePriceLog)

## Homogeneity of Variance

bartlett.test(AveragePriceLog ~ region , data=avocados)

## ANOVA without Homogeneity of Variance

ANOVA <- lm(AveragePriceLog ~ region , data=avocados)
Anova(ANOVA, Type="II", white.adjust=TRUE)

## Post Hocs

pairwise.t.test(avocados$AveragePrice, avocados$region, p.adjust="bonferroni", pool.sd = FALSE)

# Determine Means and Draw Conclusions

avocadosMeans <- avocados %>% group_by(region) %>% summarize(Mean = mean(AveragePrice)) %>% arrange(desc(Mean))
View(avocadosMeans)

# Yes there is a significant difference in price with Albany having the highest price and Houston having less.


