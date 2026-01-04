library(lme4)
library(emmeans)
library(ggplot2)
library(readr)

setwd("C:/Users/jayes/Downloads")
yield <- read_csv("project_data(yield).csv")


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


colnames(emm_gen_env_df)
emm_gen_env <- emmeans(m2, ~ genotype | environment, infer = TRUE)
emm_gen_env_df <- as.data.frame(emm_gen_env)

p2 <- ggplot(emm_gen_env_df,
             aes(x = genotype,
                 y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2) +
  facet_wrap(~ environment, scales = "free_x") +
  labs(
    x = "Genotype",
    y = "Predicted mean yield"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p2

##The GxE faceted plot looks good but to many genotypes. 
##A GxE interaction profile plot (emmip) communicates  the same result more clearly.

p3 <- emmip(m2, genotype ~ environment, CIs = TRUE) +
  labs(
    x = "Environment",
    y = "Predicted mean yield"
  )

p3


ggsave("Figure_pd_environment.png", p1, width = 8, height = 5)
ggsave("Figure_genotype_environment.png", p2, width = 10, height = 6)
