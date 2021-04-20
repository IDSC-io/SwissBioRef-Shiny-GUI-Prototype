

###########################
###########################
####    UTILIS FILE    ####
###########################
###########################


# The following file contains external functions, that can be maintained
# ouside of the main source code (ui.R & server.R)

# A) Statistical functions (boxcox functions, kurtosis functions)
# B) Bootstrapping function (for non-parametric RV, and for all Confidence intervals)
# C) Graphing functions


#### LOADING ESSENTIAL FUNCTIONS AND RESOURCES
library(ggplot2)
library(dplyr)
library(shiny)


#########################
## Statistics function ##
#########################

## Box cox transformation
# Backtransformation function (lbd meaning lambda)
boxcox.backtransform <- function(data, lbd) {
  
  if (lbd == 0) { 
    return(exp(data) - 1)
  } else{
    return((data * lbd + 1) ^ (1 / lbd) - 1)
  }
  
}

# Calculate skewness and kurosis from a dataset
symmetrie <- function(data, nrow) {
  x <- data
  n <- nrow
  
  # skewness function
  g1 <- (sum((x - mean(x)) ^ 3) / n) / (sum((x - mean(x)) ^ 2) / n) ^ 1.5
  
  # kurtosis function
  g2 <- n * sum((x - mean(x)) ^ 4) / (sum((x - mean(x)) ^ 2) ^ 2) - 3
  
  # Bind skewness & kurtosis together
  return(c(g1 * sqrt(n * (n - 1)) / (n - 2), ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))))
  
}


#############################
## Bootstrapping functions ##
#############################

# Functions using the standart boot() package

# Written by Harald Witte
# Adjusted to the Shiny environment by Tobias Blatter


# Quantile function
# Simple helper function returning upper and lower quantile from data
utils.stats.bootstrap <- function(data,
                                  indices,
                                  lower_limit = 0.025,
                                  upper_limit = 0.975) {
  
  return(c(quantile(data[indices], lower_limit),
           quantile(data[indices], upper_limit)))
  
}

# Function determining confidence intervals by bootstrapping, using bias-corrected and accelerated (BCa) bootstraping
boot.conf.int <- function(data,
                          stats_function = utils.stats.bootstrap,
                          lower_limit = 0.025,
                          upper_limit = 0.975,
                          num_rep = 100,
                          #core_usage = "multicore", num_cpus = 16,
                          alpha = 0.90,
                          random.seed = NULL) {
  
  # set random seed for bootstrap if applicable
  if (!is.null(random.seed)) {
    set.seed(random.seed)
  }
  
  # create bootstrat replicates
  boot_strap_replicates <- boot::boot(data = data,
                                      statistic = utils.stats.bootstrap,
                                      lower_limit = lower_limit,
                                      upper_limit = upper_limit,
                                      R = num_rep)#,
                                      #parallel = core_usage, ncpus = num_cpus )
  
  
  # # Get the length of t0
  # if(is.logical(length(boot_strap_replicates$t0))){return(c(NA,NA,NA,NA,NA,NA))}
  
  
  length.t0 <- length(boot_strap_replicates$t0)
  
  
  
  # set up vector for BCa bootstrap results (e.g. mean, lower and upper bound) for each statistic
  boostrap_conf_ints <- vector(mode = "numeric", length = (length.t0 + 1) * length.t0)  # +1 for bootstrap value itself
  
  
  # determine confidence intervals for each statistic of interest using BCa
  for (i in 1:length.t0) {
    # confidence interval calculation
    boostrap_ci <- boot::boot.ci(boot.out = boot_strap_replicates,
                                 index = i,
                                 type = "norm",
                                 conf = alpha
      )
    
    # Set start and end
    idx_start <- 1 + (length.t0 + 1) * (i - 1)
    idx_end <- idx_start + length.t0
    
    # get lower bound, bootstrap result and upper bound
    
    # boostrap_ci$bca[4:5] contain the endpoints of the CI-estimation
    boostrap_conf_ints[idx_start:idx_end] <- c(boostrap_ci$bca[4], boostrap_ci$t0, boostrap_ci$bca[5])
    
    # name vector elements
    names(boostrap_conf_ints)[idx_start:idx_end] <- c(paste0( names(boot_strap_replicates$t0[i]), 
                                                              c("_lower_bound", "_bootstrap", "_upper_bound")))
  }
  
  # Will return bootstrap and conf int as
  # [1]   2.5%_lower_bound   [2]   2.5%_bootstrap  [3]   2.5%_upper_bound
  # [4]  97.5%_lower_bound   [5]  97.5%_bootstrap  [6]  97.5%_upper_bound
  
  return(boostrap_conf_ints)
}




##########################
##  Plotting functions  ##
##########################

# Graph for TAB 1 in UI (MAIN Graph)

shinyHistogram <-
  function(dat,
           ana,
           xlimits,
           inputVal,
           refLower,
           refUpper,
           ciTRUE,
           ciLower,
           ciUpper) {
    
    name <- strsplit(ana, split = ' \\| ')
    
    p <- dat %>%
      ggplot(aes(value)) +
      
      # Kernel density estimator
      geom_bar(stat = "density",
               show.legend = FALSE,
               fill = "#b2e0f9") + #428bca
      
      # Setting the title and the xlab
      labs(title = paste0("Histogram for the analyte: ", name[[1]][1])) +
      labs(x = paste0("Value [", name[[1]][2], "]"), y = "Density") +
      
      theme(legend.position = "none")
    
    # Building the plot
    
    #build <- ggplot_build(p)
    #colnames(build) <- make.unique(names(build))
    #d <- build$data[[1]]
    d <- ggplot_build(p)$data[[1]]
    
    if (!is.na(refLower) & !is.na(refUpper)) { 
      
        # Coloring the range of the reference interval differently
        p <- p + #geom_ribbon(data=subset(dat,x>refUpper & x<refLower),aes(p),fill="#428bca") #+
                 geom_area(data = subset(d, x > refUpper),aes(x = x, y = y),fill = "#428bca") + #b2e0f9
                 geom_area(data = subset(d, x < refLower),aes(x = x, y = y),fill = "#428bca") #b2e0f9
    }
    
    
    
    # Building the confidence interval on top
    if (ciTRUE) {
      
      if (ciLower[1] != ciLower[2]){
          p <- p + geom_area(data = subset(d, ciLower[1] < x & x <= ciLower[2]),
                             aes(x = x, y = y), fill = "red", alpha = 0.4)
      }
      
      if (ciUpper[1] != ciUpper[2]){    
          p <- p + geom_area(data = subset(d, ciUpper[1] < x & x <= ciUpper[2]),
                             aes(x = x, y = y), fill = "red", alpha = 0.4)
      }
    }
    
    # Adds a vertical line for the "observed lab value" here
    if (!is.na(inputVal)) {
      
      p <- p + geom_vline(xintercept = inputVal, color = "#000080", linetype = "dashed") #+
               #geom_text(aes(label = 'Observed value', x = inputVal),hjust = 0, vjust = 1, color="#000080")
    }
    
    
    # Call plot --> will render in the UI
    # Adjusts the limits (can be changed by drag and dropping of window)
    p + lims(x = xlimits)
      
    
  }


