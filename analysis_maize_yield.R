library(LabApplStat)
library(lme4)
library(readxl)
library(MASS)
library(emmeans)
library(ggplot2)
library(readr)
library(multcomp)
library(dplyr)
library(ggplot2)
library(tidytext)

yield <- read.delim("dataM.txt")
yield$pd <- factor(yield$pd)
yield$environment <- factor(yield$environment)
yield$genotype <- factor(yield$genotype)
yield$block <- factor(yield$block)
yield$rep <- factor(yield$rep)
summary (yield)
str(yield)


plot(DD(yield~ pd * environment * genotype, random=~environment:pd: block: rep,data=yield),"MSS")
#plot(DD(yield~ pd * environment * genotype, random=~environment:pd: block: rep,data=yield))
m0 <- lmer(yield ~ pd * environment * genotype + (1|environment:pd:block:rep), data = yield)
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
###

# Ensure factors
yield$environment <- factor(yield$environment)
yield$pd          <- factor(yield$pd)
yield$genotype    <- factor(yield$genotype)
yield$rep         <- factor(yield$rep)
yield$block       <- factor(yield$block)

m2 <- lmer(
  yield ~ pd + environment + genotype +
    pd:environment + environment:genotype +
    (1 | environment:pd:rep:block),
  data = yield
)

##Plant density × Environment interaction
## (Does the effect of plant density change across environments)

#Yield is lowest at low density across environments, with environment-specific response magnitudes.
colnames(emm_pd_env_df)
p1 <- ggplot(emm_pd_env_df,
             aes(x = pd,
                 y = emmean,
                 group = environment,
                 colour = environment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1) +
  labs(
    x = "Plant density",
    y = "Predicted mean yield",
    colour = "Environment"
  ) +
  theme_minimal()

)
p1

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
cld_gen_env <- cld(
  emm_gen_env,
  by = "environment",
  adjust = "sidak",
  Letters = letters,
  decreasing = TRUE
)

cld_df <- as.data.frame(cld_gen_env)
cld_df$.group <- gsub(" ", "", cld_df$.group)

# Reorder genotypes within environment
cld_df <- cld_df %>%
  mutate(
    genotype_reordered = reorder_within(genotype, emmean, environment)
  )

# -------------------------------
# Function to plot ONE environment
# -------------------------------
plot_env <- function(env_name) {
  
  df_env <- cld_df %>%
    filter(environment == env_name)
  
  ggplot(
    df_env,
    aes(x = emmean, y = genotype_reordered)
  ) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(xmin = asymp.LCL, xmax = asymp.UCL),
      width = 0.2
    ) +
    geom_text(
      aes(x = asymp.UCL + 0.25, label = .group),
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

# ===============================
# Generate plots (one by one)
# ===============================
p_fumesua    <- plot_env("Fumesua")
p_legon_mi   <- plot_env("Legon_Mi")
p_legon_off  <- plot_env("Legon_off")
p_nyankpala  <- plot_env("Nyankpala")

# Print 
p_fumesua
p_legon_mi
p_legon_off
p_nyankpala
"Fumesua

Genotype performance differed significantly in Fumesua. The highest yields were 
observed for CML16 × 87036 and ENT11 × 87036, which formed the top statistical group. 
Several other genotypes, including PAN53 and M131 x CML16, showed comparable performance 
and did not differ significantly from the top group. Pairwise comparisons confirmed that high-performing 
genotypes yielded significantly more than the lowest-yielding genotypes.

Legon_Mi

In Legon_Mi, significant differences among genotypes were also observed. Hybrids
M131 x CML16 and TZdEI501 x ENT11 ranked among the highest yielding entries, whereas 
TZEI1-based genotypes were among the lowest. However, differences among the 
top-performing genotypes were generally not significant, indicating similar yield 
potential within this group.


Legon_off

Genotype rankings in Legon_off followed a pattern similar to Legon_Mi, with 
CML16-derived hybrids showing superior performance. Several genotypes formed a broad 
high-yielding group, while TZEI1-based genotypes consistently produced lower yields.
These results highlight moderate genotype differentiation within this environment.


Nyankpala

In Nyankpala, genotype performance differed markedly from the other environments. 
Overall yields were lower, and genotype rankings changed substantially. Although 
some genotypes maintained intermediate performance, no single genotype consistently 
dominated. Pairwise comparisons indicated fewer significant differences among genotypes, 
suggesting a stronger environmental constraint on yield expression.
"







