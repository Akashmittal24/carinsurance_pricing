### clean_names() sanitises the column names of the data
clean_names <- function(colnames) {
    colnames <- gsub(" ",          "_", colnames) %>%
        gsub("/",                  "_",      .) %>%
        gsub("\\.",                "_",      .) %>%
        gsub("\\-",                "_",      .) %>%
        gsub("'",                  "",       .) %>%
        gsub("`",                  "",       .) %>%
        gsub("\\(",                "",       .) %>%
        gsub("\\)",                "",       .) %>%
        gsub("\\?",                "",       .) %>%
        gsub("\\%",                "",       .) %>%
        gsub("\u2019",             "",       .) %>%
        gsub("\u20AC",            'EUR',     .) %>%
        gsub('(.)([A-Z][a-z]+)',  '\\1_\\2', .) %>%
        gsub('([a-z0-9])([A-Z])', '\\1_\\2', .) %>%
        gsub('1_st',              '1st',     .) %>%
        gsub('2_nd',              '2nd',     .) %>%
        tolower %>%
        gsub('__+',               '_',       .)

    return(colnames)
}


### Checks if variable is a date/time
is_date <- function(.x) inherits(.x, c("POSIXt", "POSIXct", "POSIXlt", "Date", "hms"))


### Returns the category of data type passed to it
categorise_datatype <- function (x) {
    if(all(is.na(x))) return("na")

    if(is_date(x))                          "datetime"
    else if (!is.null(attributes(x)) ||
             all(is.character(x)))          "discrete"
    else if (all(is.logical(x)))            "logical"
    else                                    "continuous"
}


### create_coltype_list() splits columns into various types
create_coltype_list <- function(data_tbl) {
    coltypes <- data_tbl %>%
        map_chr(categorise_datatype)

    cat_types <- coltypes %>%
        unique %>%
        sort

    split_lst <- cat_types %>%
        map(function(x) { coltypes[coltypes %in% x] %>% names })

    names(split_lst) <- coltypes %>% unique %>% sort

    coltype_lst <- list(
        split   = split_lst
        ,columns = coltypes
    )

    return(coltype_lst)
}


### Creates a subset of rows and columns of a data frame
create_ggpairs_tbl <- function(data_tbl, sample_cols, sample_rows, verbose = FALSE) {

    ncol_data <- ncol(data_tbl)
    nrow_data <- nrow(data_tbl)

    if(ncol_data > sample_cols) {
        col_index <- sample(ncol(data_tbl), sample_cols, replace = FALSE) %>% sort
    } else {
        col_index <- 1:ncol_data
    }

    row_count <- ifelse(nrow_data > sample_rows, sample_rows, nrow_data)

    if(verbose) {
        cat(paste0(names(data_tbl)[col_index], collapse = ','))
        cat("\n")

        data_tbl %>% select(col_index) %>% glimpse
    }

    sample_tbl <- data_tbl %>%
        select(col_index) %>%
        sample_n(row_count)


    ### Check we are not missing any data
    missing_check <- (sample_tbl %>% filter(complete.cases(.)) %>% nrow) == 0

    ### Check data is not same value down column
    unique_val_count <- sample_tbl %>%
        summarise_all(function(x) x %>% unique %>% length) %>%
        gather %>%
        filter(value == 1) %>%
        nrow
    same_check <- (unique_val_count > 0)

    if(missing_check || same_check) {
        sample_tbl <- create_ggpairs_tbl(data_tbl, sample_cols, sample_rows)
    }


    return(sample_tbl)
}


### Creates outputs for samples of data if row count is above a threshold. Otherwise calculates once
### on the whole dataset
create_sampled_output <- function(fulldata_tbl, summ_func, row_count = 2000, iter_count = 5) {
    output_lst <- list()

    if((fulldata_tbl %>% nrow) > row_count) {
        for(i in 1:iter_count) {
            sample_tbl <- fulldata_tbl %>%
                sample_n(row_count)

            output_lst[[i]] <- summ_func(sample_tbl)
        }
    } else {
        output_lst[[1]] <- summ_func(fulldata_tbl)
    }

    return(output_lst)
}


### Identify outliers
identify_univariate_outliers <- function(x) {
    outlier_vals <- boxplot.stats(x)$out

    outlier_point <- x %in% outlier_vals

    return(outlier_point)
}


### Jitter numeric variable
add_jitter <- function(x) x * rlnorm(length(x), 0, 0.0001)


### Jitter numeric data to assist with uniqueness
jitter_numeric_variables <- function(data_tbl) {
    data_tbl <- data_tbl %>%
        mutate_if(is.numeric, add_jitter)

    return(data_tbl)
}


powerlaw_claimsize_count <- function(claimsize, claimdata_tbl) {
    claimdata_tbl %>%
        filter(claim_amount >= 10^claimsize) %>%
        nrow
}


create_crossval_assessment <- function(train_bs, valid_bs) {
    valid_tbl <- valid_bs %>% as_data_frame

    observed_valid_count <- valid_tbl %>%
        summarise(total_claims = sum(claim_nb)) %>%
        pull(total_claims)

    model_glm <- glm(claim_nb ~ gas + cat_driver_age
                     ,offset = log(exposure)
                     ,data   = train_bs %>% as_data_frame
                     ,family = poisson
    )

    claim_rates <- predict(model_glm
                           ,newdata = valid_bs %>% as_data_frame
                           ,type = 'response')

    predicted_claim_count <- rpois(1000, claim_rates %>% sum)

    cuml_prob <- ecdf(predicted_claim_count)(observed_valid_count)

    assess_lst <- list(
        observed_count  = observed_valid_count
        ,predicted_count = predicted_claim_count
        ,cuml_prob       = cuml_prob
    )

    return(assess_lst)
}


create_claimrate_assessment <- function(train, valid) {
    model_glm <- glm(claim_count ~ gas + cat_driver_age + car_age + density +
                         cat_driver_age:gas
                     ,offset = log(exposure)
                     ,data   = train %>% as_data_frame
                     ,family = poisson
    )

    valid_tbl <- valid %>% as_data_frame

    observed_valid_count <- valid_tbl %>%
        summarise(total_claims = sum(claim_count)) %>%
        pull(total_claims)

    claim_rates <- predict(model_glm
                           ,newdata = valid_tbl
                           ,type = 'response')

    predicted_claim_count <- rpois(1000, claim_rates %>% sum)

    cuml_prob <- ecdf(predicted_claim_count)(observed_valid_count)

    assess_lst <- list(
        observed_count  = observed_valid_count
        ,predicted_count = predicted_claim_count
        ,cuml_prob       = cuml_prob
    )

    return(assess_lst)
}


create_pricing_function <- function(claimrate_model_glm
                                    ,claimsize_model_glm
                                    ,largeloss_charge
                                    ,quote_ratio) {

    price_function <- function(policy_tbl) {
        policy_id <- policy_tbl$policy_id

        claim_rates <- predict(claimrate_model_glm
                               ,newdata = policy_tbl
                               ,type = 'response')

        claim_sizes <- predict(claimsize_model_glm
                               ,newdata = policy_tbl
                               ,type = 'response')

        expect_price <- claim_rates * claim_sizes
        risk_premium <- expect_price + largeloss_charge

        price_tbl <- data_frame(
            policy_id        = policy_id
            ,expect_price     = expect_price
            ,largeloss_charge = largeloss_charge
            ,risk_premium     = risk_premium
            ,quote_price      = risk_premium * (1 + quote_ratio)
        )

        return(price_tbl)
    }

    return(price_function)
}


###
### Quick note on what this function does:
###
### For computational efficiency, I calculate the total number of claims
### required and then use a combination of cumulative sums and indexes to
### properly allocate out the claim amounts across the simulations.
###
calculate_claim_sizes <- function(claim_list, shape, rate) {
    claim_idx <- claim_list %>% cumsum

    total_claim_count <- claim_list %>% sum
    claim_amounts     <- rgamma(total_claim_count, shape = shape, rate = rate)

    claim_amount_cumsum <- c(0, claim_amounts %>% cumsum)
    claim_cumsum        <- claim_amount_cumsum[claim_idx + 1]

    claim_sizes <- c(0, claim_cumsum) %>% diff

    return(claim_sizes)
}


calculate_largeloss_claims <- function(claim_list, scaling) {
    claim_idx <- claim_list %>% cumsum

    total_claim_count <- claim_list %>% sum
    claim_amounts     <- rplcon(total_claim_count, xmin = 25000, alpha = scaling)

    claim_amount_cumsum <- c(0, claim_amounts %>% cumsum)
    claim_cumsum        <- claim_amount_cumsum[claim_idx + 1]

    claim_sizes <- c(0, claim_cumsum) %>% diff

    return(claim_sizes)
}





###
### This function creates a claim simulation function based on the various
### models provided to it (assuming a glm)
###
create_claim_simulator <- function(claimfreq_glm
                                   ,claimsev_glm
                                   ,largeloss_freq    = NULL
                                   ,largeloss_scaling = NULL) {

    generate_claim_simulations <- function(data_tbl
                                           ,n_sim = 1000
                                           ,variable_claim_size = TRUE
                                           ,model_large_losses  = TRUE) {

        ### We first predict the mean for the claim rate and claim size
        ### Then we simulate claim counts for each policy
        simulation_tbl <- data_tbl %>%
            mutate(claim_rate    = predict(claimfreq_glm, newdata = data_tbl, type = 'response')
                   ,claim_size_mu = predict(claimsev_glm,  newdata = data_tbl, type = 'response')
                   ,claim_counts  = map(claim_rate, function(l) rpois(n_sim, l))
            )

        ### If we want variable claim size, we simulate from a gamma distribution
        ### using the parameters as given from the GLM
        if(variable_claim_size) {
            simulation_tbl <- simulation_tbl %>%
                mutate(claim_size_shape = MASS::gamma.shape(claimsev_glm)$alpha
                       ,claim_size_rate  = claim_size_shape / claim_size_mu
                       ,claim_costs      = map(claim_counts
                                               ,calculate_claim_sizes
                                               ,shape = claim_size_shape
                                               ,rate  = claim_size_rate)
                )
        } else {
            simulation_tbl <- simulation_tbl %>%
                mutate(claim_costs = map2(claim_counts, claim_size_mu, `*`))
        }


        ### If we include large losses, we proceed in a similar way for the
        ### the attritional piece - simplified by the fact that all policies
        ### are treated the same.
        if(model_large_losses & !is.null(largeloss_freq) & !is.null(largeloss_scaling)) {
            simulation_tbl <- simulation_tbl %>%
                mutate(largeloss_freq       = largeloss_freq
                       ,largeloss_claimcount = map(largeloss_freq, function(l) rpois(n_sim, l))
                       ,largeloss_claimsize  = map(largeloss_claimcount
                                                   ,calculate_largeloss_claims
                                                   ,largeloss_scaling
                       )
                )
        }

        return(simulation_tbl)
    }

    return(generate_claim_simulations)
}


construct_assessment_plot <- function(sim_vals, obs_val, title, subtitle) {
    assessment_plot <- ggplot() +
        geom_histogram(aes(x = sim_vals), bins = 50) +
        geom_vline(aes(xintercept = obs_val), colour = 'red') +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        xlab("Simulation Value") +
        ylab("Count") +
        ggtitle(title, subtitle = subtitle)

    return(assessment_plot)
}


construct_model_assessment <- function(data_tbl, title_prefix) {
    sim_claim_count <- data_tbl$claim_counts %>% reduce(`+`)
    obs_claim_count <- data_tbl$claim_count %>% sum

    plot_title    <- paste0(title_prefix, " Comparison Plot for Simulated Claim Counts vs Observed")
    plot_subtitle <- paste0((data_tbl %>% nrow %>% comma), " Policies Used")

    claimcount_plot <- construct_assessment_plot(sim_claim_count
                                                 ,obs_claim_count
                                                 ,plot_title
                                                 ,plot_subtitle)



    sim_loss_amount <- data_tbl$claim_costs %>% reduce(`+`)
    obs_loss_amount <- data_tbl$claim_total %>% reduce(`+`)

    plot_title    <- paste0(title_prefix, " Comparison Plot for Simulated Loss Cost vs Observed")
    plot_subtitle <- paste0((data_tbl %>% nrow %>% comma), " Policies Used")

    losscost_plot <- construct_assessment_plot(sim_loss_amount
                                               ,obs_loss_amount
                                               ,plot_title
                                               ,plot_subtitle)

    losscost_plot <- losscost_plot +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


    assess_lst <- list(
        sim_claim_count = sim_claim_count
        ,obs_claim_count = obs_claim_count
        ,claimcount_plot = claimcount_plot
        ,sim_loss_amount = sim_loss_amount
        ,obs_loss_amount = obs_loss_amount
        ,losscost_plot   = losscost_plot
    )

    return(assess_lst)
}


construct_pricing_assessment <- function(data_tbl, title_prefix) {
    sim_loss_amount <- data_tbl$claim_costs  %>% reduce(`+`)
    premium_charged <- data_tbl$expect_price %>% sum

    plot_title    <- paste0(title_prefix, " Comparison Plot for Simulated Loss Cost vs Premium Charged")
    plot_subtitle <- paste0((data_tbl %>% nrow %>% comma), " Policies Used")

    assess_plot <- construct_assessment_plot(sim_loss_amount
                                             ,premium_charged
                                             ,plot_title
                                             ,plot_subtitle
    )


    assess_lst <- list(
        sim_loss_amount = sim_loss_amount
        ,premium_charged = premium_charged
        ,assess_plot     = assess_plot
    )

    return(assess_lst)
}












