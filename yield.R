install.packages("readxl")  
library(readxl)
library(LabApplStat)
library(lme4)


yield <- read_excel("yield.xlsx", sheet = "yield")
summary (yield)
str(yield)

m0 <- lmer(yield ~ pd * environment * genotype + (1|environment:pd:block:rep), data = yield)
names(ranef(m0))

# Residual plot
plot(m0) 
# Normal quantile plot for residuals
qqnorm(residuals(m0))
qqline(residuals(m0))   # to add the line
# Normal quantile plot for random effects
qqnorm(ranef(m0)[["pd:environment:block:rep"]][, 1], asp = 1)
qqline(ranef(m0)[["pd:environment:block:rep"]][, 1], asp = 1)   # to add the line

"""
our disrtibution looks good, no apparent outliers and no special trends or 
patterns followed by the observations. QQ plot looks almost perfect for residuals, but a bit
questionable for random effects

"""
