#setwd("../../..")  
setwd("moore_2022/code")

# Convert survey response to fractional success/failure
survey_to_sf <- function(response) {
    switch(response,
           "Not a barrier" = c(s = 0, f = 1),
           "Small"         = c(s = 1/3, f = 2/3),
           "Moderate"      = c(s = 2/3, f = 1/3),
           "Significant"   = c(s = 1, f = 0),
           stop("Unknown response: ", response))
}

# Update a vector of prior values using quantile mapping and Beta posterior
posterior_bounded <- function(prior_vals, responses, flip_direction) {
    sf <- vapply(responses, survey_to_sf, numeric(2))
    a <- 1 + sum(sf[1, ])
    b <- 1 + sum(sf[2, ])
    
    # Dr. Rising's new suggestion
    # Instead of random rbeta() draws, align each prior draw to its quantile
    
    qvals <- (rank(prior_vals) - 0.5) / length(prior_vals)
    if (flip_direction)
        beta_qvals <- qbeta(qvals, b, a)
    else
        beta_qvals <- qbeta(qvals, a, b)
    result <- quantile(prior_vals, beta_qvals, type = 1)
    
    return(result)
}




# NEW: added `flip_flags` argument with default FALSE
prior2post <- function(priordraws, surveyresponse_list, flip_flags) {
    postdraws <- priordraws
    
    for (j in seq_along(surveyresponse_list)) {
        param_name <- names(surveyresponse_list)[j]
        
        if (!(param_name %in% colnames(priordraws))) {
            warning("Parameter not found in priordraws: ", param_name)
            next
        }
        
        responses <- surveyresponse_list[[j]]
        vals <- priordraws[[param_name]]
        
        if (length(responses) == 0) {
            warning("No survey responses for: ", param_name)
            next
        }
        
        postdraws[[param_name]] <- posterior_bounded(vals, responses, flip_flags[[param_name]])
    }
    
    return(postdraws)
}














# # Apply update across all parameters and draws
# prior2post <- function(priordraws, surveyresponse_list) {
#     stopifnot(length(surveyresponse_list) == ncol(priordraws))
#     postdraws <- priordraws
#     
#     for (j in seq_len(ncol(priordraws))) {
#         responses <- surveyresponse_list[[j]]
#         vals <- priordraws[, j]
#         
#         if (length(responses) == 0) {
#             warning("No survey responses for: ", colnames(priordraws)[j])
#             next
#         }
#         
#         postdraws[, j] <- posterior_bounded(vals, responses)
#     }
#     
#     return(postdraws)
# }