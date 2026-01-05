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

#since plot is a bit questionable check that residuals are normally distributed with Shapiro test
# p-value of 3.221*10^-7 so residuals are not normally distributed which would mean we could do a Box-Cox transformation??
shapiro.test(residuals(m0)) 
#based on the result of the boxcox, I think it is very close to 1, so idk if it would be good to do the transformation
boxcox(lm(yield ~ pd * environment * genotype,data=yield),lambda = seq(-3,2,0.1))
#WHAT IF WE USE LOG TRANFORMATION, WOULD IT BE SIMPLER?


"""
our disrtibution looks good, no apparent outliers and no special trends or 
patterns followed by the observations. QQ plot looks almost perfect for residuals, but a bit
questionable for random effects

"""

# now we move on to the model reduction
# our model:
# m0 <- lmer(yield ~ pd * environment * genotype + (1|environment:pd:block:rep), data = yield)

drop1(m0)

# interaction pd:environment:genotype can be deleted
m1 <- update(m0, . ~ . - pd:environment:genotype)
drop1(m1)

# interaction pd:environment:genotype can be deleted
m2 <- update(m1, . ~ . - pd:genotype)
drop1(m2)

#Since AIC of /none/ is the lowest, we stop reducing it, this meaning:
# WE FOUND THE MINIMAL ADEQUATE MODEL:
# m2 <- lmer( yield ~ pd + environment + genotype + pd:environment + environment:genotype + 
#     (1 | pd:environment:block:rep), data = yield)

# NOW WE TEST validity OF THE MODEL with the reduction

# Residual plot
plot(m2) 
# Normal quantile plot for residuals
qqnorm(residuals(m2))
qqline(residuals(m2))   # to add the line
# Normal quantile plot for random effects
qqnorm(ranef(m2)[["pd:environment:block:rep"]][, 1], asp = 1)
qqline(ranef(m2)[["pd:environment:block:rep"]][, 1], asp = 1)   # to add the line

# model fits even better than in the beginning 





# ARE THERE EFFECTS??
