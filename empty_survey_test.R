# 0. Libraries and functions
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/Other computers/My Laptop/UDel/Taky_research/CCAC_taky/prior_to_post")

source("prior2post.R")
source("../../moore_2022/code/src/model.R")



# 1. Read & prep your prior draws
priordraws <- fread("/Users/taky/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/Other computers/My Laptop/UDel/Taky_research/CCAC_taky/prior_to_post/moore_2022/results/MC Runs/parameter_tune.csv")[, 1:9]  # drop sampleweight

priordraws <- as.data.frame(lapply(priordraws, as.numeric)) %>% na.omit()

# --- STEP 1: EMPTY-SURVEY TEST (Beta(1,1)) ---
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
#postdraws$Shifting.Baselines <- as.integer(postdraws$Shifting.Baselines > 0.5)

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
    NA,
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
    
    nonzeros.pe = pe[pe != 0]
    lastchange.pe = tail(which(diff(sign(diff(nonzeros.pe))) != 0), 1)
    nonzeros.me = me[me != 0]
    lastchange.me = tail(which(diff(sign(diff(nonzeros.me))) != 0), 1)
    
    if (length(lastchange.pe) == 0 || length(lastchange.me) == 0)
        next
    
    for (y in max(lastchange.pe, lastchange.me):max(length(nonzeros.pe), length(nonzeros.me))) {
        eY  <- pe[y]
        idx <- which(me <= eY + 1e-10)[1] - y
        delay_matrix[i, y] <- idx
    }
}

# 5. Quick sanity checks
cat("Total zero (never catch up) entries: ", sum(delay_matrix == 0L, na.rm=T), "\n")
cat("Mean delay (yrs, ignoring zeros):  ",
    mean(delay_matrix, na.rm = TRUE), "\n")
cat("Mean delay (yrs, ignoring zeros):  ",
    mean(delay_matrix[, colnames(delay_matrix) == "2050"], na.rm = TRUE), "\n")
print(table(delay_matrix, useNA = "always"))
