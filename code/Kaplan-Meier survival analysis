## Makes a survival plot 

library(reshape2)
library(survminer)
library(survival)
library(dplyr)

surv1 <- pairwise_survdiff(Surv(Duration,Death)~Treatment.Group,data = n1)

surv1

fit <- survfit(Surv(Duration, Death) ~ Treatment.Group, data = n2)

fit

ggsurvplot(fit,
           pval = FALSE, xlim = c(0,12), legend.labs = c("3.17:1","6.63:1","13.32:1","Control","4.77:1","1.27:1"))

##prints a summary of a survival curve

print(fit, print.rmean=TRUE)

## cox model

fit.coxph <- coxph(Surv(Duration, Death) ~ Colony, data = coxalt)

ggforest(fit.coxph, data = coxalt)

## makes a correlation table 

library("Hmisc")

res <- cor(`Honey.bee.diet.data.2019_2020...Sheet12.(3)`, method = "spearman")       
res

library("ggpubr")

ggscatter(cor3, x = "Lipids", y = "rmean", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
## Makes a heat map 

install.packages("corrplot")
library(corrplot)

corrplot(res, type = "upper")

---------------------------------------------------------------------------

##making model for osmia mortality 

attach(OsmiaM)

##makes Control the comparison diet
nat2$Diet <- as.factor(nat2$Diet)

nat2$Diet <- relevel(nat2$Diet, "red maple ")

levels(nat2$Diet)

##Makes the GLM with diet as a factor
model4 <- glm(formula = Average.broodcell ~ Viral.presence + Floral.area + Resource.mass, family = "poisson", data = dat3)
summary(model4)

anova(model4)

##Compares between treatment groups

library(multcomp)

summary(glht(model4, linfct=mcp(Diet="Tukey")))

cld(glht(model4, linfct=mcp(Diet="Tukey")))

#compare mass variation between groups 

ggplot(test1, aes(x = Start.mass)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Diet ~ .)

fligner.test(Start.mass ~ Diet, data = test1)
