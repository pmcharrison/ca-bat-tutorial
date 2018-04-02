########################################################
# Calibrating the item response model
########################################################

# Load packages ####
library(ltm)

# Load data ####

items <- read.csv("datasets/study-1-item-bank.csv",
                  stringsAsFactors = FALSE)
responses <- read.csv("datasets/study-1-responses.csv",
                      stringsAsFactors = FALSE)

# <items> is a data.frame where each row is an item in the item bank.
# It corresponds to the dataset used in Study 1 but with 
# the 5 outliers excluded.

# <responses> is a data.frame where each row is a participant,
# each column is an item, and each cell is the scored responses
# by the participant to that item (1 = correct, 0 = incorrect).

# Make an IRT model
mod <- tpm(
  responses,
  type = "rasch", # => equal discrimination parameter across all items
  constraint = cbind(c(1:27), 1, 0.5) # fix guessing parameter to 0.5
)
# Get the item parameters
item_param <- as.data.frame(coef(mod))
names(item_param) <- plyr::revalue(names(item_param),
                                   c(Gussng = "guessing",
                                     Dffclt = "difficulty",
                                     Dscrmn = "discrimination"))

# Standard errors of item parameters are also available 
# via summary(mod)$coefficients, but these are more awkward to extract.

# Merge the estimated item parameters into the <items> data.frame
items <- cbind(items, item_param)

# Exclude items with accuracies less than 0.55
items <- items[items$accuracy >= 0.55, ]

# Fit a linear regression model to predict difficulty from beep-track accuracy
mod_lm <- lm(difficulty ~ accuracy, data = items)

# Plot the items
plot(items$accuracy, items$difficulty,
     xlab = "Beep-track accuracy",
     ylab = "Difficulty")

# Plot the regression model
abline(mod_lm)

# We can now predict the difficulty of new items from our regression model.
# For example, let's predict the difficulty of an item with 
# beep-track accuracy 0.7.
as.numeric(predict(mod_lm, newdata = data.frame(accuracy = 0.7)))

########################################################
# Checking IRT assumptions
########################################################
res <- list()

# Margins ####
message("Checking model fit on the margins")
margin.threshold <- 4 # threshold sourced from Bartholomew (1998)
## Two-way margins
# Compute the margins
two.way.margins <- margins(mod, type = "two-way")
# Extract the statistics
two.way.stats <- two.way.margins$margins[, 5, ]
# Count how many pairs there were 
num.two.way <- length(two.way.stats)
# Count how many surpass the threshold
num.flag.two.way <- sum(two.way.stats > margin.threshold)
# Calculate proportion
prop.flag.two.way <- num.flag.two.way / num.two.way
# Save this info
res$margins$two.way <- list(full.results = two.way.margins,
                            threshold = margin.threshold,
                            num.pairs = num.two.way,
                            num.pairs.flagged = num.flag.two.way,
                            proportion.pairs.flagged = prop.flag.two.way,
                            pct.pairs.flagged = 100 * prop.flag.two.way)
message("% of pairs flagged = ",
        round(res$margins$two.way$pct.pairs.flagged, digits = 2))


## Three-way margins
# Compute the margins
three.way.margins <- margins(mod, type = "three-way")
# Extract the statistics
three.way.stats <- three.way.margins$margins[, 6, ]
# Count how many triples there were 
num.three.way <- length(three.way.stats)
# Count how many surpass the threshold
num.flag.three.way <- sum(three.way.stats > margin.threshold)
# Calculate proportion
prop.flag.three.way <- num.flag.three.way / num.three.way
# Save this info
res$margins$three.way <- list(full.results = three.way.margins,
                              threshold = margin.threshold,
                              num.triples = num.three.way,
                              num.triples.flagged = num.flag.three.way,
                              proportion.triples.flagged = prop.flag.three.way,
                              pct.triples.flagged = 100 * prop.flag.three.way)
message("% of triples flagged = ",
        round(res$margins$three.way$pct.triples.flagged, digits = 2))

# Person fit can help find participants with strange response patterns.
# We don't explore it here though.
res$person.fit <- person.fit(mod)

# Item fit
# Each item is tested for poor fit.
# Significant results suggests that the item fits poorly,
# though beware for multiple-comparison false-positives.
set.seed(1)
message("Testing item fit. Warning - this will take a few minutes!")
res$item.fit <- suppressWarnings(item.fit(mod,
                                          simulate.p.value = TRUE,
                                          B = 500))

# Unidimensionality
# Significant result indicates that the underlying construct may
# be multidimensional.
# We use a modified version of the unidimensionality test provided in
# the ltm package that adds error catching.
revised_unidimTest <- function(mod, num.MC.samples) {
  # Introduces error catching
  t_boot <- list()
  pb <- txtProgressBar(max = num.MC.samples, style = 3)
  while (length(t_boot) < num.MC.samples) {
    tmp <- 
      tryCatch({
        # message("Running a bootstrap sample")
        unidimTest(mod, B = 1)
      }, error = function (e) NULL)
    if (is.null(tmp)) {
      # message("Bootstrap sample failed, trying again")
    } else {
      t_obs <- tmp$Tobs[2]
      t_boot[[length(t_boot) + 1]] <- tmp$T.boot[2]
      setTxtProgressBar(pb, length(t_boot))
    }
  }
  t_boot <- as.numeric(t_boot)
  unidim_p <- (1 + sum(t_boot >= t_obs)) / (1 + length(t_boot))
  list(observed_eigenvalue = t_obs,
       simulated_eigenvalue = mean(t_boot),
       p = unidim_p,
       num_samples = num.MC.samples)
}
set.seed(1)
message("Testing unidimensionality. Warning - this will take a few minutes!")
res$unidim_test <- suppressWarnings(revised_unidimTest(mod, num.MC.samples = 500))
