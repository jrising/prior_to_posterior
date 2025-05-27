#setwd("../../..")  # sets working directory to Moore_2022/


# Convert survey response to fractional success/failure
survey_to_sf <- function(response) {
    switch(response,
           "Not a barrier" = c(s = 0, f = 1),
           "Small"         = c(s = 1/3, f = 2/3),
           "Moderate"      = c(s = 2/3, f = 1/3),
           "Significant"   = c(s = 1, f = 0),
           stop("Unknown response: ", response))
}

# Update a single prior value with survey evidence

posterior <- function(prior_val, responses) {
    sf <- vapply(responses, survey_to_sf, numeric(2))
    a <- 1 + sum(sf[1, ])
    b <- 1 + sum(sf[2, ])
    
    # Use quantile transformation: q = prior_val since prior is uniform
    q <- prior_val
    
    # Get posterior draw as q-th quantile of the Beta(a, b)
    qbeta(q, a, b)
}


# Apply update across all parameters and draws
prior2post <- function(priordraws, surveyresponse_list) {
    stopifnot(length(surveyresponse_list) == ncol(priordraws))
    postdraws <- priordraws
    
    for (j in seq_len(ncol(priordraws))) {
        resp <- surveyresponse_list[[j]]
        vals <- priordraws[, j]
        
        # if you supplied some survey answers *and* the column really is in [0,1], do the Beta-update
        if (length(resp) > 0 && all(vals >= 0 & vals <= 1, na.rm = TRUE)) {
            for (i in seq_len(nrow(priordraws))) {
                postdraws[i, j] <- posterior(vals[i], resp)
            }
        } else {
            # otherwise leave it as-is
            warning("Skipping Beta-update for “", colnames(priordraws)[j],
                    "”: values not in [0,1].")
        }
    }
    
    postdraws
}


