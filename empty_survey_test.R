# 0.libraries and functions
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)


setwd("../..")  

source("src/model.R")
source("src/prior_to_post/prior2post.R")


# 1. Read & prep your prior draws
priordraws <- fread("../results/MC Runs/parameter_tune.csv")[, 1:9]  # drop sampleweight

priordraws <- as.data.frame(lapply(priordraws, as.numeric)) %>% na.omit()

# --- STEP 1: EMPTY‐SURVEY TEST (Beta(1,1)) ---
param_names <- c(
    "Homophily", "Strong.Force", "Weak.Force",
    "Evidence", "Pol.Opinion", "Status.Quo.Bias",
    "Pol.Int.Feedback", "Biased.Assimilation", "Shifting.Baselines"
)
surveyresponse_list <- lapply(param_names, function(x) character(0))
names(surveyresponse_list) <- param_names

postdraws <- as.data.frame(
    prior2post(as.matrix(priordraws), surveyresponse_list)
)

# Binary-ify Shifting.Baselines
postdraws$Shifting.Baselines <- as.integer(postdraws$Shifting.Baselines > 0.5)

# Align sizes just in case
ndraws     <- min(nrow(priordraws), nrow(postdraws))
priordraws <- priordraws[1:ndraws, ]
postdraws  <- postdraws[1:ndraws, ]

# Check that priors == posteriors under an empty survey
cat("Empty-survey check (TRUE means no change):\n",
    all.equal(priordraws, postdraws), "\n\n")

# 2. Initialize your delay matrix
years        <- 2020:2100
nyears       <- length(years)
delay_matrix <- matrix(
    0L,
    nrow = ndraws, ncol = nyears,
    dimnames = list(NULL, as.character(years))
)

# 3. Helper to run Moore's model from one draw
run_model_from_draw <- function(draw) {
    model(
        homophily_param             = draw[[1]],
        forcestrong                 = draw[[2]],
        forceweak                   = draw[[3]],
        evidenceeffect              = draw[[4]],
        policyopinionfeedback_param = draw[[5]],
        pol_response                = draw[[6]],
        pol_feedback                = draw[[7]],
        biassedassimilation         = draw[[8]],
        shiftingbaselines           = draw[[9]],
        time                        = 1:81,
        year0                       = 2020,
        frac_opp_0                  = 0.07,
        frac_neut_0                 = 0.22
    )
}

# 4. Fill in delay_matrix
for (i in seq_len(ndraws)) {
    prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
    post_model  <- tryCatch(run_model_from_draw(postdraws[i,  ]), error = function(e) NULL)
    if (is.null(prior_model) || is.null(post_model)) next
    
    pe <- prior_model$totalemissions
    me <- post_model$totalemissions
    
    for (y in seq_len(nyears - 1)) {
        eY <- pe[y]
        idx <- which(me[(y + 1):nyears] <= eY + 1e-10)[1]
        if (!is.na(idx)) delay_matrix[i, y] <- idx
    }
    
}

# 5. Quick sanity checks
cat("Total zero (never catch up) entries: ", sum(delay_matrix == 0L), "\n")
cat("Mean delay (yrs, ignoring zeros):  ",
    mean(delay_matrix[delay_matrix > 0], na.rm = TRUE), "\n")
print(table(delay_matrix, useNA = "always"))
