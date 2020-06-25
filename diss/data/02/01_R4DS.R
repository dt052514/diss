# R for Data Science
# Modeling, Chapter 23-25

# Package Load ####

library(tidyverse)
library(modelr)
library(ggthemes)

# Residuals ####

# It’s also useful to see what the model doesn’t capture, the so-called residuals which are left after subtracting the predictions from the data. Residuals are powerful because they allow us to use models to remove striking patterns so we can study the subtler trends that remain.

sim1_mod <- lm(y ~ x, data = sim1)

sim1 %>%
  add_residuals(sim1_mod) %>% 
  ggplot(aes(resid)) +
  geom_histogram()

sim1 %>%
  add_residuals(sim1_mod) %>% 
  ggplot(aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

# If a model with one predictor fits the data well, the residuals will be randomy distributed at values of the predictor. If however it does not capture a key pattern or source of variation in the data, the residuals will not be evenly distributed.

# Interactions ####

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point()

sim3 %>% 
  gather_residuals(mod1, mod2) %>% 
  ggplot(aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)


