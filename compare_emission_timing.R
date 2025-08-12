# Load libraries and helpers
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/Other computers/My Laptop/UDel/Taky_research/CCAC_taky/prior_to_post")
## setwd("~/research/iamup2/ccac/prior_to_posterior/moore_2022/code")

source("../../prior2post.R")
source("src/model.R")

# 1. Read and prep prior draws
# priordraws <- fread("../../moore_2022/results/MC Runs/parameter_tune.csv")[, 1:9]

## TRY WITH FULL SET OF PARAMS
priordraws <- fread("../../moore_2022/results/MC Runs/MC Runs_TunedParams/params.csv")
priordraws <- priordraws[, -c(2, 3, 11, 13, 14, 15, 21, 22), with = FALSE]
priordraws <- as.data.frame(lapply(priordraws, as.numeric)) %>% na.omit()

# Keep only unidirectional parameters

param_names <- c("Homophily", "Evidence", "Pol.Opinion", "Status.Quo.Bias", "Pol.Int.Feedback", "Biased.Assimilation", "Shifting.Baselines",  "m_max", "ced", "policy_adoption", "etc_total", "normeffect", "adopt_effect", "lbd_param")


priordraws <- priordraws[, param_names]


## CHINA
#surveyresponse_list <- readRDS("../../data/surveyresponse_list_china.rds")

## INDIA
#surveyresponse_list <- readRDS("../../data/surveyresponse_list_india.rds")

## MEXICO
#surveyresponse_list <- readRDS("../../data/surveyresponse_list_mexico.rds")

## NIGERIA
#surveyresponse_list <- readRDS("../../data/surveyresponse_list_nigeria.rds")


# FAKE SURVEY
#surveyresponse_list <- readRDS("../../data/surveyresponse_list_FAKE_biased.rds")
## for (name in names(surveyresponse_list))
##     surveyresponse_list[[name]] <- rep("Not a barrier", length(surveyresponse_list[[name]]))


# Map survey responses
surveyresponse_list_mapped <- list(
    Homophily         = surveyresponse_list$Social,
    ## Evidence          = surveyresponse_list$Social,
    ## Pol.Opinion       = surveyresponse_list$Institutional,
    Status.Quo.Bias   = surveyresponse_list$Institutional,
    ## normeffect        = surveyresponse_list$Social,
    ## policy_adoption   = surveyresponse_list$Institutional,
    ## etc_total         = surveyresponse_list$Institutional,
    m_max             = surveyresponse_list$Economic,
    lbd_param         = surveyresponse_list$Technological
    ## adopt_effect      = surveyresponse_list$Economic
)


flip_flags <- list(
    Homophily         = FALSE,
    Pol.Opinion       = FALSE,
    Evidence          = TRUE,
    ced               = TRUE,
    normeffect        = FALSE,
    policy_adoption   = TRUE,
    etc_total         = TRUE,
    m_max             = TRUE,
    lbd_param         = TRUE,
    adopt_effect      = TRUE,
    Status.Quo.Bias   = FALSE
)



# Generate postdraws
postdraws <- prior2post(priordraws, surveyresponse_list_mapped, flip_flags)

stopifnot(nrow(postdraws) == nrow(priordraws))

#ndraws <- 1000
ndraws <- nrow(priordraws)


# 4. Run Moore model from a draw
run_model_from_draw <- function(draw) {
    model(
        homophily_param             = draw[["Homophily"]],
        ## forcestrong                 = draw[["Strong.Force"]],
        ## forceweak                   = draw[["Weak.Force"]],
        evidenceeffect              = draw[["Evidence"]],
        policyopinionfeedback_param = draw[["Pol.Opinion"]],
        pol_response                = draw[["Status.Quo.Bias"]],
        pol_feedback                = draw[["Pol.Int.Feedback"]],
        biassedassimilation         = draw[["Biased.Assimilation"]],
        shiftingbaselines           = draw[["Shifting.Baselines"]],
        m_max                       = draw[["m_max"]],
        ced                         = draw[["ced"]],
        ## policy_adoption             = draw[["policy_adoption"]],
        etc_total                   = draw[["etc_total"]],
        normeffect                  = draw[["normeffect"]],
        adopt_effect                = draw[["adopt_effect"]],
        lbd_param                   = draw[["lbd_param"]],
        time                        = 1:81,
        year0                       = 2020,
        frac_opp_0                  = 0.07,
        frac_neut_0                 = 0.22
    )
}






# ----------------------------------------------------
# FINAL STEP: All Unidirectional Parameters at Once
# ----------------------------------------------------

# Prepare delay storage
param_names <- colnames(priordraws)
delay_summary <- data.frame(Parameter = param_names, MeanDelay = NA_real_)

get_delay <- function(pe, me) {
    which.max(me) - which.max(pe)
}

for (p in names(surveyresponse_list_mapped)) {
    delays <- numeric(ndraws)
    for (i in seq_len(ndraws)) {
        draw <- priordraws[i, ]
        draw[[p]] <- postdraws[i, p]  # change just one param to its post value

        model_prior <- tryCatch(run_model_from_draw(as.list(priordraws[i, ])), error = function(e) NULL)
        model_post  <- tryCatch(run_model_from_draw(as.list(draw)), error = function(e) NULL)

        if (is.null(model_prior) || is.null(model_post)) {
            delays[i] <- NA
            next
        }

        pe <- model_prior$totalemissions
        me <- model_post$totalemissions
        delays[i] <- get_delay(pe, me)
    }
    delay_summary$MeanDelay[delay_summary$Parameter == p] <- mean(delays, na.rm = TRUE)
    cat("Finished:", p, "| Valid = ", sum(!is.na(delays)), "| Delay =", paste(quantile(delays, na.rm = TRUE), collapse=", "), "\n")
}

## Do all params
delays <- numeric(ndraws)
for (i in seq_len(ndraws)) {
    model_prior <- tryCatch(run_model_from_draw(as.list(priordraws[i, ])), error = function(e) NULL)
    model_post  <- tryCatch(run_model_from_draw(as.list(postdraws[i, ])), error = function(e) NULL)

    if (is.null(model_prior) || is.null(model_post)) {
        delays[i] <- NA
        next
    }

    pe <- model_prior$totalemissions
    me <- model_post$totalemissions
    delays[i] <- get_delay(pe, me)
}
cat("Finished:", p, "| Valid = ", sum(!is.na(delays)), "| Delay =", paste(quantile(delays, na.rm = TRUE), collapse=", "), "\n")

ggplot(data.frame(delays), aes(delays)) +
    geom_histogram() + theme_bw() + xlab("Delay of Peak Emissions in Years")

pdf <- data.frame()
for (name in names(surveyresponse_list)) {
    sf <- sapply(surveyresponse_list[[name]], survey_to_sf)
    aa <- 1 + sum(sf[1, ])
    bb <- 1 + sum(sf[2, ])
    pdf <- rbind(pdf, data.frame(name, xx=seq(0, 1, length.out=100), yy=dbeta(seq(0, 1, length.out=100), aa, bb)))
}

ggplot(pdf, aes(xx, yy)) +
    facet_wrap(~ name) +
    geom_line() + theme_bw() +
    scale_x_continuous(NULL, expand=c(0, 0)) + ylab(NULL)

# # -----------------------------------------------------------------------------------
# # PREVIOUS CODE: One-at-a-time Parameter Delay Test (posterior shift)
# # -----------------------------------------------------------------------------------
#
# ndraws <- 100  # adjust as needed
# param_names <- colnames(priordraws)
# delay_summary <- data.frame(Parameter = param_names, MeanDelay = NA_real_)
#
# get_delay <- function(pe, me) {
#     for (y in seq_along(pe)) {
#         if (me[y] <= pe[y]) return(y)
#     }
#     return(NA)
# }
#
# for (i in seq_along(param_names)) {
#     param <- param_names[i]
#     cat("Running param:", param, "\n")
#
#     # Fix all parameters to their prior medians
#     fixed_vals <- apply(priordraws, 2, median)
#     delays <- numeric(ndraws)
#
#     for (j in seq_len(ndraws)) {
#         draw <- fixed_vals  # start from fixed median prior
#         draw[param] <- postdraws[[param]][j]  # update just one parameter to posterior
#
#         m_post  <- tryCatch(run_model_from_draw(as.list(draw)), error = function(e) NULL)
#         m_prior <- tryCatch(run_model_from_draw(as.list(fixed_vals)), error = function(e) NULL)
#
#         if (is.null(m_post) || is.null(m_prior)) next
#
#         pe <- m_prior$totalemissions
#         me <- m_post$totalemissions
#         delays[j] <- get_delay(pe, me)
#     }
#
#     delay_summary$MeanDelay[i] <- mean(delays, na.rm = TRUE)
#     cat("Finished:", param, "| Mean Delay =", delay_summary$MeanDelay[i], "\n")
# }




















# # Load libraries and helpers
# library(data.table)
# library(ggplot2)
# library(reshape2)
# library(dplyr)
#
# setwd("~/Library/CloudStorage/GoogleDrive-tahmid@udel.edu/Other computers/My Laptop/UDel/Taky_research/CCAC_taky/prior_to_post")
#
# source("prior2post.R")
# source("../../moore_2022/code/src/model.R")
#
# # 1. Read and prep prior draws
# priordraws <- fread("../../moore_2022/results/MC Runs/parameter_tune.csv")[, 1:9]
# priordraws <- as.data.frame(lapply(priordraws, as.numeric)) %>% na.omit()
# ndraws     <- nrow(priordraws)
#
# # # 2. Weighted survey responses
# # weighted_sample <- function(prob) sample(c("Not a barrier", "Small", "Moderate", "Significant"), 30, TRUE, prob = prob)
# # surveyresponse_list <- list(
# #     Homophily           = weighted_sample(c(0.4, 0.3, 0.2, 0.1)),
# #     Strong.Force        = weighted_sample(c(0.1, 0.2, 0.3, 0.4)),
# #     Weak.Force          = weighted_sample(c(0.25, 0.25, 0.25, 0.25)),
# #     Evidence            = weighted_sample(c(0.3, 0.3, 0.2, 0.2)),
# #     Pol.Opinion         = weighted_sample(c(0.2, 0.3, 0.3, 0.2)),
# #     Status.Quo.Bias     = weighted_sample(c(0.3, 0.2, 0.3, 0.2)),
# #     Pol.Int.Feedback    = weighted_sample(c(0.25, 0.25, 0.25, 0.25)),
# #     Biased.Assimilation = weighted_sample(c(0.2, 0.3, 0.3, 0.2)),
# #     Shifting.Baselines  = weighted_sample(c(0.5, 0.2, 0.2, 0.1))
# # )
#
# ## INDIA
# # surveyresponse_list <- readRDS("../../data/surveyresponse_list_india.rds")
#
# ## CHINA
# surveyresponse_list <- readRDS("../../data/surveyresponse_list_china.rds")
#
# # Map to model parameters based on dropbox paper
# # surveyresponse_list_mapped <- list(
# #     Homophily           = surveyresponse_list$Social,
# #     Strong.Force        = surveyresponse_list$Social,
# #     Weak.Force          = surveyresponse_list$Social,
# #     Evidence            = surveyresponse_list$Social,
# #     Pol.Opinion         = surveyresponse_list$Institutional,
# #     Status.Quo.Bias     = surveyresponse_list$Institutional,
# #     Pol.Int.Feedback    = surveyresponse_list$Institutional,
# #     Biased.Assimilation = surveyresponse_list$Social,
# #     Shifting.Baselines  = surveyresponse_list$Social
# # )
#
# surveyresponse_list_mapped <- list(
#     Status.Quo.Bias = surveyresponse_list$Institutional
# )
#
# stopifnot(length(surveyresponse_list_mapped) == ncol(priordraws))
#
# postdraws <- prior2post(as.matrix(priordraws), surveyresponse_list_mapped)
#
# # 3. Ensure dimensions align
# priordraws <- priordraws[1:nrow(postdraws), ]
#
# # 4. Run Moore model from a draw
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
# # 5. Delay matrix
# years <- 2020:2100
# delay_matrix <- matrix(NA, nrow = ndraws, ncol = length(years), dimnames = list(NULL, as.character(years)))
#
# for (i in seq_len(ndraws)) {
#     prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
#     post_model  <- tryCatch(run_model_from_draw(postdraws[i,  ]), error = function(e) NULL)
#     if (is.null(prior_model) || is.null(post_model)) next
#
#     pe <- prior_model$totalemissions
#     me <- post_model$totalemissions
#
#     nonzeros.pe <- pe[pe != 0]
#     nonzeros.me <- me[me != 0]
#     lastchange.pe <- tail(which(diff(sign(diff(nonzeros.pe))) != 0), 1)
#     lastchange.me <- tail(which(diff(sign(diff(nonzeros.me))) != 0), 1)
#
#     if (length(lastchange.pe) == 0 || length(lastchange.me) == 0) next
#
#     for (y in max(lastchange.pe, lastchange.me):max(length(nonzeros.pe), length(nonzeros.me))) {
#         eY <- pe[y]
#         idx <- which(me <= eY + 1e-10)[1] - y
#         delay_matrix[i, y] <- idx
#     }
# }
#
# # 6. Summary stats
# cat("Total zero-delay entries: ", sum(delay_matrix == 0L, na.rm = TRUE), "\n")
# cat("Mean delay (ignoring zeros): ", mean(delay_matrix, na.rm = TRUE), "\n")
# cat("Mean delay in 2050: ", mean(delay_matrix[, "2050"], na.rm = TRUE), "\n")





# ----------------------------------------
# Original delay matrix block (commented out)
# ----------------------------------------

# years <- 2020:2100
# delay_matrix <- matrix(NA, nrow = ndraws, ncol = length(years), dimnames = list(NULL, as.character(years)))
# for (i in seq_len(ndraws)) {
#   prior_model <- tryCatch(run_model_from_draw(priordraws[i, ]), error = function(e) NULL)
#   post_model  <- tryCatch(run_model_from_draw(postdraws[i,  ]), error = function(e) NULL)
#   if (is.null(prior_model) || is.null(post_model)) next
#   pe <- prior_model$totalemissions
#   me <- post_model$totalemissions
#   nonzeros.pe <- pe[pe != 0]
#   nonzeros.me <- me[me != 0]
#   lastchange.pe <- tail(which(diff(sign(diff(nonzeros.pe))) != 0), 1)
#   lastchange.me <- tail(which(diff(sign(diff(nonzeros.me))) != 0), 1)
#   if (length(lastchange.pe) == 0 || length(lastchange.me) == 0) next
#   for (y in max(lastchange.pe, lastchange.me):max(length(nonzeros.pe), length(nonzeros.me))) {
#     eY <- pe[y]
#     idx <- which(me <= eY + 1e-10)[1] - y
#     delay_matrix[i, y] <- idx
#   }
# }

# # 6. Summary stats (unchanged)
# cat("Total zero-delay entries: ", sum(delay_matrix == 0L, na.rm = TRUE), "\n")
# cat("Mean delay (ignoring zeros): ", mean(delay_matrix, na.rm = TRUE), "\n")
# cat("Mean delay in 2050: ", mean(delay_matrix[, "2050"], na.rm = TRUE), "\n")
#
# # 7. Emissions trajectory comparison
# prior_mat <- t(sapply(seq_len(ndraws), function(i) run_model_from_draw(priordraws[i,])$totalemissions))
# post_mat  <- t(sapply(seq_len(ndraws), function(i) run_model_from_draw(postdraws[i,])$totalemissions))
# mean_prior <- colMeans(prior_mat)
# mean_post  <- colMeans(post_mat)
#
# plot(2020:2100, mean_prior, type = "l", lwd = 2, col = "blue",
#      ylim = range(c(mean_prior, mean_post)), xlab = "Year",
#      ylab = "Global Emissions (GtC/yr)", main = "Mean Emissions: Prior vs Posterior")
# lines(2020:2100, mean_post, lwd = 2, col = "red")
# legend("topright", c("Prior", "Posterior"), col = c("blue", "red"), lwd = 2)
#
# # 8. Parameter distribution plots
# priors_long <- melt(priordraws, variable.name = "Parameter", value.name = "Value")
# posts_long  <- melt(postdraws, variable.name = "Parameter", value.name = "Value")
# priors_long$Group <- "Prior"
# posts_long$Group  <- "Posterior"
# dist_df <- rbind(priors_long, posts_long)
#
# ggplot(dist_df, aes(x = Value, fill = Group)) +
#     geom_density(alpha = 0.4) +
#     facet_wrap(~ Parameter, scales = "free") +
#     theme_minimal(base_size = 14) +
#     labs(title = "Parameter Distributions: Prior vs Posterior", x = "Parameter Value", y = "Density") +
#     scale_fill_manual(values = c("Prior" = "#1f78b4", "Posterior" = "#e31a1c"))
