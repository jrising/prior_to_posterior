# Load libraries and helpers
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

setwd("../..")
source("src/model.R")
source("src/prior_to_post/prior2post.R")

# Read prior draws
raw_priors <- fread("/Users/taky/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/Other computers/My Laptop/UDel/Taky_research/CCAC_taky/Moore_2022/results/MC Runs/parameter_tune.csv")[, 1:9]  # drop sampleweight

names(raw_priors) <- make.names(names(raw_priors))  # Clean column names

param_names <- c(
    "Homophily", "Strong.Force", "Weak.Force",
    "Evidence", "Pol.Opinion", "Status.Quo.Bias",
    "Pol.Int.Feedback", "Biased.Assimilation", "Shifting.Baselines"
)

priors9 <- raw_priors[, param_names, with = FALSE]
priors9 <- as.data.frame(lapply(priors9, as.numeric)) %>% na.omit()
ndraws  <- nrow(priors9)

# Generate survey-informed posterior draws
surveyresponse_list <- list(
    Homophily           = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Strong.Force        = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Weak.Force          = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Evidence            = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Pol.Opinion         = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Status.Quo.Bias     = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Pol.Int.Feedback    = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Biased.Assimilation = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE),
    Shifting.Baselines  = sample(c("Not a barrier","Small","Moderate","Significant"), 30, TRUE)
)
names(surveyresponse_list) <- make.names(names(surveyresponse_list))

# Apply prior2post to all parameters
post9 <- as.data.frame(prior2post(as.matrix(priors9), surveyresponse_list))
post9$Shifting.Baselines <- as.integer(post9$Shifting.Baselines > 0.5)
stopifnot(dim(post9) == dim(priors9))

# Compute delay matrix
years <- 2020:2100
nyears <- length(years)
delay_matrix <- matrix(0L, nrow = ndraws, ncol = nyears, dimnames = list(NULL, as.character(years)))

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

for (i in seq_len(ndraws)) {
    prior_m <- try(run_model_from_draw(priors9[i,]), silent = TRUE)
    post_m  <- try(run_model_from_draw(post9[i,]),  silent = TRUE)
    if (inherits(prior_m, "try-error") || inherits(post_m, "try-error")) next
    
    pe <- prior_m$totalemissions
    me <- post_m$totalemissions
    
    for (y in seq_len(nyears - 1)) {
        eY  <- pe[y]
        idx <- which(me[(y + 1):nyears] <= eY + 1e-10)[1]
        if (!is.na(idx)) delay_matrix[i, y] <- idx
    }
}

cat("Total draws never catch up (delay == 0):", sum(delay_matrix == 0), "\n")
pos <- delay_matrix[delay_matrix > 0]
if (length(pos) > 0) {
    cat("Mean positive delay:", mean(pos), "\n")
    print(quantile(pos, c(.1, .25, .5, .75, .9)))
} else {
    cat("→ No positive delays at all.\n")
}

# Compare mean emissions trajectories
prior_mat <- t(sapply(seq_len(ndraws), function(i) run_model_from_draw(priors9[i,])$totalemissions))
post_mat  <- t(sapply(seq_len(ndraws), function(i) run_model_from_draw(post9[i,])$totalemissions))
mean_prior <- colMeans(prior_mat)
mean_post  <- colMeans(post_mat)

plot(years, mean_prior, type = "l", lwd = 2, col = "blue",
     ylim = range(c(mean_prior, mean_post)), xlab = "Year",
     ylab = "Global Emissions (GtC/yr)", main = "Mean Emissions: Prior vs Posterior")
lines(years, mean_post, lwd = 2, col = "red")
legend("topright", c("Prior", "Posterior"), col = c("blue", "red"), lwd = 2)

# Plot parameter distributions
priors_long <- melt(priors9, variable.name = "Parameter", value.name = "Value")
posts_long  <- melt(post9,   variable.name = "Parameter", value.name = "Value")
priors_long$Group <- "Prior"
posts_long$Group  <- "Posterior"

dist_df <- rbind(priors_long, posts_long)

ggplot(dist_df, aes(x = Value, fill = Group)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ Parameter, scales = "free") +
    theme_minimal(base_size = 14) +
    labs(
        title = "Parameter Distributions: Prior vs Posterior",
        x = "Parameter Value",
        y = "Density"
    ) +
    scale_fill_manual(values = c("Prior" = "#1f78b4", "Posterior" = "#e31a1c"))
