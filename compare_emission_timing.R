
# ############################################
# 
# ## PREVIOUS CODE
# ############################################


# library(data.table)
# library(ggplot2)
# library(reshape2)
# 
# # Load Moore model and prior2post logic
# source("src/model.R")
# source("src/model_analysis/prior2post.R")
# 
# # Read prior draws and convert to numeric
# priordraws <- fread("../results/priordraws_moore.csv")
# priordraws <- as.data.frame(lapply(priordraws, as.numeric))
# priordraws <- na.omit(priordraws)
# 
# 
# # Parameter names (in order)
# param_names <- c("Homophily", "Strong.Force", "Weak.Force", "Evidence",
#                  "Pol.Opinion", "Status.Quo.Bias", "Pol.Int.Feedback",
#                  "Biased.Assimilation", "Shifting.Baselines")
# 
# # Simulate survey responses for each parameter
# set.seed(123)
# # surveyresponse_list <- list(
# #     Homophily           = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.2, 0.4, 0.3, 0.1)),
# #     Strong.Force        = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.25, 0.35, 0.3, 0.1)),
# #     Weak.Force          = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.3, 0.4, 0.2, 0.1)),
# #     Evidence            = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.15, 0.25, 0.4, 0.2)),
# #     Pol.Opinion         = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.25, 0.25, 0.3, 0.2)),
# #     Status.Quo.Bias     = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.3, 0.3, 0.2, 0.2)),
# #     Pol.Int.Feedback    = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.25, 0.35, 0.3, 0.1)),
# #     Biased.Assimilation = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.3, 0.3, 0.3, 0.1)),
# #     Shifting.Baselines  = sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, c(0.35, 0.3, 0.2, 0.15))
# # )
# # names(surveyresponse_list) <- param_names
# 
# 
# ## SANITY Check
# surveyresponse_list <- lapply(1:9, function(i) rep("Not a barrier", 30))
# names(surveyresponse_list) <- param_names
# 
# 
# priordraws <- subset(priordraws,
#                      Status.Quo.Bias >= 1 &
#                          Pol.Opinion >= 0 &
#                          Homophily > 0 &
#                          Strong.Force >= 0 & Strong.Force <= 1 &
#                          Weak.Force >= 0 & Weak.Force <= Strong.Force
# )
# 
# 
# postdraws <- as.data.frame(prior2post(as.matrix(priordraws), surveyresponse_list))
# postdraws$Shifting.Baselines <- as.numeric(postdraws$Shifting.Baselines > 0.5)
# 
# # Clip to valid ranges
# postdraws$Status.Quo.Bias <- pmax(postdraws$Status.Quo.Bias, 1)
# postdraws$Strong.Force <- pmin(pmax(postdraws$Strong.Force, 0), 1)
# postdraws$Weak.Force <- pmin(postdraws$Weak.Force, postdraws$Strong.Force)
# 
# # Filter to working model draws only
# good_indices <- c()
# for (i in 1:2000) {
#     prior_ok <- tryCatch(!is.null(run_model_from_draw(priordraws[i, ])), error = function(e) FALSE)
#     post_ok  <- tryCatch(!is.null(run_model_from_draw(postdraws[i, ])),  error = function(e) FALSE)
#     
#     if (prior_ok && post_ok) {
#         good_indices <- c(good_indices, i)
#     }
#     if (length(good_indices) >= 1000) break
# }
# 
# priordraws <- priordraws[good_indices, ]
# postdraws  <- postdraws[good_indices, ]
# 
# # Final cleanup
# postdraws <- na.omit(postdraws)
# priordraws <- priordraws[1:nrow(postdraws), ]
# postdraws  <- postdraws[1:nrow(postdraws), ]
# ndraws <- nrow(postdraws)
# 
# 
# 
# 
# # Set up model dimensions
# years <- 2020:2100
# nyears <- length(years)
# ndraws <- nrow(priordraws)
# 
# # Initialize delay matrix
# #delay_matrix <- matrix(NA, nrow = ndraws, ncol = nyears)
# delay_matrix <- matrix(NA_real_, nrow = ndraws, ncol = nyears)
# 
# colnames(delay_matrix) <- years
# 
# # Wrapper to run Moore's model
# run_model_from_draw <- function(draw_row) {
#     model(
#         homophily_param = as.numeric(draw_row[[1]]),
#         forcestrong = as.numeric(draw_row[[2]]),
#         forceweak = as.numeric(draw_row[[3]]),
#         evidenceeffect = as.numeric(draw_row[[4]]),
#         policyopinionfeedback_param = as.numeric(draw_row[[5]]),
#         pol_response = as.numeric(draw_row[[6]]),
#         pol_feedback = as.numeric(draw_row[[7]]),
#         biassedassimilation = as.numeric(draw_row[[8]]),
#         shiftingbaselines = as.numeric(draw_row[[9]]),
#         time = 1:81,
#         year0 = 2020,
#         frac_opp_0 = 0.07,
#         frac_neut_0 = 0.22
#     )
# }
# 
# # Simulate emissions for priors and posteriors
# for (i in seq_len(ndraws)) {
#     # prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
#     # post_model  <- tryCatch(run_model_from_draw(postdraws[i, ]),  error = function(e) NULL)
#     
#     prior_model <- tryCatch({
#         m <- run_model_from_draw(priordraws[i, ])
#         if (any(is.na(m$distributions))) stop("NA in prior model")
#         m
#     }, error = function(e) NULL)
#     
#     post_model <- tryCatch({
#         m <- run_model_from_draw(postdraws[i, ])
#         if (any(is.na(m$distributions))) stop("NA in post model")
#         m
#     }, error = function(e) NULL)
#     
#     
#     if (is.null(prior_model) || is.null(post_model)) next
#     
#     prior_emissions <- prior_model$totalemissions
#     post_emissions  <- post_model$totalemissions
#     
#     for (y in seq_len(nyears - 1)) {
#         e_Y <- prior_emissions[y]
#         # Look ahead for the first year where post ≤ prior level
#         idx <- which(post_emissions[(y + 1):nyears] <= e_Y)[1]
#         # If none found, record zero; otherwise record the delay
#         delay_matrix[i, y] <- ifelse(is.na(idx), 0, idx)
#             
#         }
#     }
# 
# 
#     
# print(typeof(delay_matrix))
# print(class(delay_matrix))
# print(summary(delay_matrix))
# 
# 
# delay_df <- as.data.frame(delay_matrix)
# cat("Non-NA entries in delay_matrix:", sum(!is.na(delay_matrix)), "\n")
# 
# write.csv(delay_df, "../results/delay_matrix.csv", row.names = FALSE)


# ############################################
# 
# ## SUCCEFUL SANITY CHECK with all 0s
# ############################################
# 
# 
# 
# # 0. (Re-)load your libraries and functions
# library(data.table)
# library(ggplot2)
# library(reshape2)
# library(dplyr)
# source("src/model.R")
# source("src/model_analysis/prior2post.R")
# 
# # 1. Read & prep your prior draws
# priordraws <- fread("../results/priordraws_moore.csv")
# priordraws <- as.data.frame(lapply(priordraws, as.numeric)) %>% na.omit()
# 
# # --- STEP 1: EMPTY‐SURVEY TEST (Beta(1,1)) ---
# param_names <- c(
#     "Homophily","Strong.Force","Weak.Force",
#     "Evidence","Pol.Opinion","Status.Quo.Bias",
#     "Pol.Int.Feedback","Biased.Assimilation","Shifting.Baselines"
# )
# surveyresponse_list <- lapply(param_names, function(x) character(0))
# names(surveyresponse_list) <- param_names
# 
# postdraws <- as.data.frame(
#     prior2post(as.matrix(priordraws), surveyresponse_list)
# )
# # binary-ify that one:
# postdraws$Shifting.Baselines <- as.integer(postdraws$Shifting.Baselines > 0.5)
# 
# # Align sizes just in case
# ndraws     <- min(nrow(priordraws), nrow(postdraws))
# priordraws <- priordraws[1:ndraws, ]
# postdraws  <- postdraws[1:ndraws, ]
# 
# # Check that priors == posteriors under an empty survey
# cat("Empty-survey check (TRUE means no change):\n",
#     all.equal(priordraws, postdraws), "\n\n")
# 
# # 2. Initialize your delay matrix
# years      <- 2020:2100
# nyears     <- length(years)
# delay_matrix <- matrix(
#     0L,
#     nrow = ndraws, ncol = nyears,
#     dimnames = list(NULL, as.character(years))
# )
# 
# # 3. Helper to run Moore's model from one draw
# run_model_from_draw <- function(draw) {
#     model(
#         homophily_param             = draw[[1]],
#         forcestrong                 = draw[[2]],
#         forceweak                   = draw[[3]],
#         evidenceeffect              = draw[[4]],
#         policyopinionfeedback_param = draw[[5]],
#         pol_response                = draw[[6]],
#         pol_feedback                = draw[[7]],
#         biassedassimilation         = draw[[8]],
#         shiftingbaselines           = draw[[9]],
#         time                        = 1:81,
#         year0                       = 2020,
#         frac_opp_0                  = 0.07,
#         frac_neut_0                 = 0.22
#     )
# }
# 
# # 4. Fill in delay_matrix
# for (i in seq_len(ndraws)) {
#     prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
#     post_model  <- tryCatch(run_model_from_draw(postdraws[i,  ]), error = function(e) NULL)
#     if (is.null(prior_model) || is.null(post_model)) next
#     
#     pe <- prior_model$totalemissions
#     me <- post_model$totalemissions
#     
#     # for each base‐year y (except the last)
#     for (y in seq_len(nyears - 1)) {
#         eY  <- pe[y]
#         # look ahead: first year ahead where post ≤ prior
#         idx <- which(me[(y+1):nyears] <= eY)[1]
#         # if never “catches up” → leave 0, else record how many years ahead:
#         if (!is.na(idx)) delay_matrix[i, y] <- idx
#     }
# }
# 
# # 5. Quick sanity checks
# cat("Total zero (never catch up) entries: ", sum(delay_matrix == 0L), "\n")
# cat("Mean delay (yrs, ignoring zeros):  ",
#     mean(delay_matrix[delay_matrix > 0], na.rm = TRUE), "\n")
# print(table(delay_matrix, useNA = "always"))


###################################################
# Now trying the whole model to run with posterior

####################################################


# 0) load packages & helper
library(data.table)

source("src/model.R")
source("src/model_analysis/prior2post.R")

# 1) read in raw draws and subset to the 9 model‐inputs

raw_priors <- fread("../results/parameter_tune.csv")[, 1:9]  # drop sampleweight


param_names <- c(
    "Homophily", "Strong.Force", "Weak.Force",
    "Evidence",  "Pol.Opinion",  "Status.Quo.Bias",
    "Pol.Int.Feedback", "Biased.Assimilation", "Shifting.Baselines"
)
priors9 <- raw_priors[, param_names, with = FALSE]
priors9 <- as.data.frame(lapply(priors9, as.numeric))
priors9 <- na.omit(priors9)
ndraws  <- nrow(priors9)

# 2) generate your survey‐informed posterior draws
surveyresponse_list <- list(
    Homophily           = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Strong.Force        = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Weak.Force          = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Evidence            = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Pol.Opinion         = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Status.Quo.Bias     = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Pol.Int.Feedback    = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Biased.Assimilation = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE),
    Shifting.Baselines  = sample(c("Not a barrier","Small","Moderate","Significant"),30,TRUE)
)

# ——— only β‐update the truly bounded parameters, leave the rest untouched
bounded_params <- c(
    "Homophily",
    "Strong.Force",
    "Weak.Force",
    "Evidence",
    "Pol.Opinion",
    "Biased.Assimilation"
)

# split priors9 into bounded vs fixed
priors_bnd   <- priors9[, bounded_params, drop = FALSE]
priors_fixed <- priors9[, setdiff(param_names, bounded_params), drop = FALSE]

# apply beta‐update only on the bounded ones
post_bnd <- as.data.frame(
    prior2post(as.matrix(priors_bnd), surveyresponse_list[bounded_params])
)
# clamp into [0,1] in case some draws fell outside
post_bnd[] <- lapply(post_bnd, function(x) pmin(pmax(x, 0), 1))

# recombine with the untouched columns
post9 <- cbind(post_bnd, priors_fixed)
# recode your binary Shifting.Baselines back to 0/1
post9$Shifting.Baselines <- as.integer(post9$Shifting.Baselines > 0.5)

# quick sanity
stopifnot(dim(post9) == dim(priors9))

# 3) prepare delay‐matrix
years        <- 2020:2100
nyears       <- length(years)
delay_matrix <- matrix(
    0L, nrow = ndraws, ncol = nyears,
    dimnames = list(NULL, as.character(years))
)

# helper to run one draw
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

# 4) fill in the delays
for (i in seq_len(ndraws)) {
    prior_m <- try(run_model_from_draw(priors9[i,]), silent = TRUE)
    post_m  <- try(run_model_from_draw(post9[i,]),  silent = TRUE)
    if (inherits(prior_m, "try-error") || inherits(post_m, "try-error")) next
    
    pe <- prior_m$totalemissions
    me <- post_m$totalemissions
    
    for (y in seq_len(nyears - 1)) {
        eY  <- pe[y]
        idx <- which(me[(y+1):nyears] <= eY)[1]
        if (!is.na(idx)) delay_matrix[i, y] <- idx
    }
}

# 5) quick sanity checks
cat("Total draws never catch up (delay==0):", sum(delay_matrix==0), "\n")
pos <- delay_matrix[ delay_matrix > 0 ]
if (length(pos) > 0) {
    cat("Mean positive delay:", mean(pos), "\n")
    print(quantile(pos, c(.1, .25, .5, .75, .9)))
} else {
    cat("→ No positive delays at all.\n")
}

# 6) compare mean emissions trajectories
prior_mat <- t(sapply(seq_len(ndraws),
                      function(i) run_model_from_draw(priors9[i,])$totalemissions))
post_mat  <- t(sapply(seq_len(ndraws),
                      function(i) run_model_from_draw(post9[i,])$totalemissions))
mean_prior <- colMeans(prior_mat)
mean_post  <- colMeans(post_mat)

plot(years, mean_prior, type = "l", lwd = 2, col = "blue",
     ylim = range(c(mean_prior, mean_post)), xlab = "Year",
     ylab = "Global Emissions (GtC/yr)",
     main = "Mean Emissions: Prior vs Posterior")
lines(years, mean_post, lwd = 2, col = "red")
legend("topright", c("Prior","Posterior"), col = c("blue","red"), lwd = 2)





# Plot prior vs posterior parameter distributions

# 1) load necessary libraries
library(ggplot2)
library(reshape2)

# 2) melt the priors9 and post9 data.frames into long format
priors_long <- melt(priors9, variable.name = "Parameter", value.name = "Value")
posts_long  <- melt(post9,  variable.name = "Parameter", value.name = "Value")

# 3) add a grouping column
priors_long$Group <- "Prior"
posts_long$Group  <- "Posterior"

# 4) combine into one data.frame
dist_df <- rbind(priors_long, posts_long)

# 5) plot densities, facetted by parameter
ggplot(dist_df, aes(x = Value, fill = Group)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ Parameter, scales = "free") +
    theme_minimal(base_size = 14) +
    labs(
        title = "Parameter Distributions: Prior vs Posterior",
        x     = "Parameter Value",
        y     = "Density"
    ) +
    scale_fill_manual(values = c("Prior" = "#1f78b4", "Posterior" = "#e31a1c")) 
