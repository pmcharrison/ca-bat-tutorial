# Load packages
library(lme4)
library(magrittr)
library(psyphy)

# Load data
dat <- read.csv("datasets/study-2-data.csv", stringsAsFactors = FALSE)

# Test several candidate models to see which best describes the data.
# glmer fits a generalised linear mixed model.
# mafc.logit(2) provides a guessing parameter of 0.5.
mod.a <- glmer(score ~ accuracy + (1|p_ID),
               data = dat,
               family = binomial(mafc.logit(2)))
mod.b <- glmer(score ~ accuracy + (1|p_ID) + (1 + accuracy|track_name),
               data = dat,
               family = binomial(mafc.logit(2)))
mod.c <- glmer(score ~ accuracy + behind_beat + (1|p_ID) + (1 + accuracy|track_name),
               data = dat,
               family = binomial(mafc.logit(2)))
# The anova function compares the fit of the different models.
anova(mod.a, mod.b, mod.c)

# mod.c is our best model. 
# We use it for our automatic item generation procedure.

# Here's an example of how we might generate parameters for a (toy) item bank.
items <- data.frame(
  accuracy = seq(from = 0.5, to = 0.9, length.out = 10),
  behind_beat = rep_len(c(TRUE, FALSE), length.out = 10),
  track_name = unique(dat$track_name)[1:10]
)

# The discrimination parameter is equal to the standard deviation 
# of the test-taker random intercept.
# It's slightly awkward to extract this from the lme4 model.
# One way is simply to read it off the model summary: summary(mod.c).
# Here we instead show an exact method.
discrimination <- mod.c %>% VarCorr %>% as.data.frame %>% 
  (function (x) x$sdcor[x$grp == "p_ID"])
items$discrimination <- discrimination

# The difficulty parameter is derived as follows:
items$difficulty <- predict(
  mod.c,
  newdata = items,
  re.form = NA # this instructs the model to predict without random effects
) %>% 
  as.numeric %>% 
  multiply_by(- 1) %>% 
  divide_by(discrimination)

# We can verify this against the difficulty equation provided in the paper:
plot(items$difficulty,
     - 5.393 + 7.247 * items$accuracy - 0.576 * items$behind_beat,
     xlab = "This script", ylab = "Paper")

# We know the guessing and inattention parameters a priori.
items$guessing <- 1 / 2
items$inattention <- 1

# If we wanted to predict difficulty using random effects for items, 
# we could do it as follows.
# Note that we have to make up some participant IDs.
items$difficulty_with_item_ranef <- predict(
  mod.c,
  newdata = cbind(items, p_ID = sample(100, nrow(items), replace = TRUE)),
  re.form = ~ (1 + accuracy|track_name)
) %>% 
  as.numeric %>% 
  multiply_by(- 1) %>% 
  divide_by(discrimination)

plot(items$difficulty,
     items$difficulty_with_item_ranef,
     xlab = "Without random effects for items",
     ylab = "With random effects for items")
