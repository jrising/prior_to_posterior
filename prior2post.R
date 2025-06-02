setwd("../../..")  


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
posterior_bounded <- function(prior_vals, responses) {
    sf <- vapply(responses, survey_to_sf, numeric(2))
    a <- 1 + sum(sf[1, ])
    b <- 1 + sum(sf[2, ])
    
    # Convert prior draws to u-space
    prior_q <- ecdf(prior_vals)(prior_vals)
    
    # Sample new quantiles from Beta posterior
    post_q <- qbeta(prior_q, a, b)
    
    # Remap back to original scale using quantile transform
    sorted_prior <- sort(prior_vals)
    result <- quantile(sorted_prior, post_q, type = 1)
    return(result)
}

# Apply update across all parameters and draws
prior2post <- function(priordraws, surveyresponse_list) {
    stopifnot(length(surveyresponse_list) == ncol(priordraws))
    postdraws <- priordraws
    
    for (j in seq_len(ncol(priordraws))) {
        responses <- surveyresponse_list[[j]]
        vals <- priordraws[, j]
        
        if (length(responses) == 0) {
            warning("No survey responses for: ", colnames(priordraws)[j])
            next
        }
        
        postdraws[, j] <- posterior_bounded(vals, responses)
    }
    
    return(postdraws)
}
