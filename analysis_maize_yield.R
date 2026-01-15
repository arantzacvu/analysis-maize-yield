library(LabApplStat)
library(lme4)
library(readxl)
library(MASS)
library(emmeans)
library(ggplot2)
library(readr)
library(multcomp)
library(dplyr)
library(tidytext)

yield <- read.delim("dataM.txt")
names(yield) <- tolower(names(yield))

yield$pd <- factor(yield$pd, levels = c("Low", "Medium", "High"))
yield$environment <- factor(yield$environment)
yield$genotype <- factor(yield$genotype)
yield$block <- factor(yield$block)
yield$rep <- factor(yield$rep)
summary (yield)
str(yield)


plot(DD(yield~ pd * environment * genotype, random=~environment:pd: block: rep,data=yield),"MSS")
#plot(DD(yield~ pd * environment * genotype, random=~environment:pd: block: rep,data=yield))
m0 <- lmer(yield ~ pd * environment * genotype + (1|environment:pd:block:rep) + (1|environment:rep), data = yield)
#see which variables have effects 
#environment:pd:block:rep
names(ranef(m0))

# Residual plot
plot(m0, main="Residual plot- INITIAL MODEL (m0)", 
     ylab = expression("Standarized residuals"), 
     xlab = expression("Fitted values")) #looks ok 

# Normal quantile plot for residuals
qqnorm(residuals(m0), main="Normal quantile plot for residuals (m0)", 
       ylab = expression("Sample quantiles"), 
       xlab = expression("Theoretical Quantiles"))
qqline(residuals(m0))   # to add the line
# Normal quantile plot for random effects
qqnorm(ranef(m0)[["environment:pd:block:rep"]][, 1], asp = 1)
qqline(ranef(m0)[["environment:pd:block:rep"]][, 1], asp = 1)   # to add the line

"""
to run in console:
qqnorm(rnorm(192)) graphical examples of how qqnorm should look like if it was normaly distributed
qqnorm(rnorm(192))
with this we obtain simulations of how it should look with 192 obs and 
after running a few we can decide wether ours looks appropriate or not
[enter>up]
"""


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

# interaction pd:genotype can be deleted
m2 <- update(m1, . ~ . - pd:genotype)
drop1(m2)

#Since AIC of /none/ is the lowest, we stop reducing it, this meaning:
# WE FOUND THE MINIMAL ADEQUATE MODEL:
#m2  <- lmer(yield ~ pd + environment + genotype + pd:environment + environment:genotype + (1 | environment:pd:block:rep), data = yield)
# NOW WE TEST validity OF THE MODEL with the reduction

# Residual plot
plot(m2, main="Residual plot- REDUCED MODEL (m2)",    
     ylab = expression("Standarized residuals"), 
     xlab = expression("Fitted values")) 
# Normal quantile plot for residuals
qqnorm(residuals(m2), main="Normal quantile plot for residuals (m2)", 
       ylab = expression("Sample quantiles"), 
       xlab = expression("Theoretical Quantiles"))
qqline(residuals(m2))   # to add the line

# Normal quantile plot for random effects
qqnorm(ranef(m2)[["environment:pd:block:rep"]][, 1], asp = 1)
qqline(ranef(m2)[["environment:pd:block:rep"]][, 1], asp = 1)   # to add the line

# model fits even better than in the beginning 
# ARE THERE EFFECTS??

# Interaction comparisons (pd × environment)
# Which is the best plant density for each environment to obtain the highest yield? 
emmeans(m2, ~ pd | environment)
#Are those differences statistically meaningful within each environment?
# (to do) get the letters to see if they are different based on p-values
#pairs(emmeans(m2, ~ pd | environment))
#mult_comp<- multcomp::cld(emmeans, Letters = letters, adjust = "tukey")

mult_comp_pd<- multcomp::cld(emmeans(m2, ~ pd | environment), Letters = letters, decreasing = TRUE, adjust = "tukey")
print(mult_comp_pd)


##Plant density × Environment interaction
## (Does the effect of plant density change across environments)

#Yield is lowest at low density across environments, with environment-specific response magnitudes.
emm_pd_env <- emmeans(m2, ~ pd | environment)

cld_pd_env <- cld( emm_pd_env,by = "environment",adjust = "sidak", Letters = letters,
                   decreasing = TRUE)

cld_df <- as.data.frame(cld_pd_env)
names(cld_df) <- tolower(names(cld_df))
cld_df$.group <- gsub(" ", "", cld_df$.group)
cld_df$pd <- factor(cld_df$pd, levels = c("Low", "Medium", "High"))

p_pd_env_faceted <- ggplot(
  cld_df,
  aes(x = pd, y = emmean)
) +
  geom_line(group = 1, size = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = lower.cl, ymax = upper.cl),
    width = 0.1
  ) +
  geom_text(
    aes(label = .group),
    vjust = -0.8,
    nudge_x = -0.15,
    size = 4
  ) +
  facet_wrap(~ environment, nrow = 2) +
  labs(
    x = "Plant density",
    y = "Predicted mean yield"
  ) +
  theme_minimal()

p_pd_env_faceted

" 
Fumesua   high > medium > low
Legon_mi   high>medium>low
legon_off    high>medium>low
nyankpala   medium>high>low

In Fumesua, yield differed significantly among all three plant densities
(high > medium > low) [explain parenthesis in words].

In Legon_Mi, (sample:high>medium>low) The high plant density produced the highest yield.
Medium and low densities did not differ significantly from each other and both were
significantly lower than high density.

A similar pattern was observed in Legon_off, (sample:high>medium>low)
where the yield across the different plant densities was the highest in the high plantdensity,
while medium and low densities did not differ significantly from each other and both led to the lowest yield.

In Nyankpala, The highest yields were obtained with medium and high density, 
with high density not differing significantly with low density, that led to the lowest yield.
"



# Interaction comparisons (genotype × environment)
#Which are the best genotypes for each environment to obtain the highest yield?
#emmeans(m2, ~ genotype | environment)
#pairs(emmeans(m2, ~ genotype | environment))
mult_comp_gen<- multcomp::cld(emmeans(m2, ~ genotype | environment), Letters = letters, decreasing = TRUE, adjust = "tukey")
print(mult_comp_gen)



# Estimated marginal means
emm_gen_env <- emmeans(m2, ~ genotype | environment)

# CLD (a = highest group)
cld_gen_env <- cld(emm_gen_env,by = "environment",adjust = "sidak",Letters = letters,
                   decreasing = TRUE)

cld_df <- as.data.frame(cld_gen_env)
cld_df$.group <- gsub(" ", "", cld_df$.group)

# Reorder genotypes within environment
cld_df <- cld_df %>% mutate(genotype_reordered = reorder_within(genotype, emmean, environment))

plot_env <- function(env_name) {
  df_env <- cld_df %>%
    filter(environment == env_name)
  
  ggplot(df_env, aes(x = emmean, y = genotype_reordered)) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(xmin = lower.CL, xmax = upper.CL),
      width = 0.2
    ) +
    geom_text(
      aes(x = upper.CL + 0.25, label = .group),
      size = 3
    ) +
    scale_y_reordered() +
    labs(
      title = env_name,
      x = "Predicted mean yield",
      y = "Genotype"
    ) +
    theme_minimal()
}

p_fumesua    <- plot_env("Fumesua") 
p_legon_mi   <- plot_env("Legon_Mi")
p_legon_off  <- plot_env("Legon_off")
p_nyankpala  <- plot_env("Nyankpala")

p_fumesua
p_legon_mi
p_legon_off
p_nyankpala

"""
Fumesua

Genotype performance differed significantly in Fumesua. The highest yields were 
observed for CML16 × 87036, ENT11 × 87036, TZEI1 X 87036, M131 X CML16, CML16 X 1368, PAN53, 1368 X 
87036, TZMI740 X CML16, M131 X TZdEI501, CML16 X ENT11, CML16 x TZEI7, M131 x 1368, TZdEI501 x CML16, 
with no significant differences amongst them.(showed comparable performance 
and did not differ significantly from the top group -- ask Jonas)


Legon_Mi

In Legon_Mi, significant differences among genotypes were also observed. Hybrids
M131 x CML16, TZdEI501 x ENT11, CML16 x ENT11, ENT11 × 87036, and CML16 x 87036 
led to the highest yields.


Legon_off

The genotype with the highest yield in Legon_off was CML16 × 87036. Moreover,
other hybrids such as: CML16-derived hybrids (except for TZdEI501 x CML16),
M131 × ENT11, TZdEI525 × M131, TZEI1 × 87036, TZdEI501 × ENT11, M131 × 1368,
TZdEI501 × 87036, ENT11 × TZEI1, TZEI387 × 87036, M131 × TZEI, M131 × TZEI387, 
ENT11 × TZEI7, TZM740 × ENT11 did not differed statistically from the top genotype. 


Nyankpala
PAN53 was the genotype that gave the highest yield; however, TZdEI501 × CML16,
ENT11 × TZEI7, TZEI387 × 87036, M131 × TZEI387, TZEI7 × 87036, TZdEI501 × ENT11, 
CML16 × TZEI387, TZdEI501 × 87036, CML16 × TZEI1, M131 × TZEI7, CML16 × ENT11, and
TZM740 × M131 were not significantly different from PAN53.

"""










