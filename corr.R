corr <- function(directory, threshold = 0) {
    # --- Assert 'directory' is a character vector of length 1 indicating the
    # location of the CSV files.  'threshold' is a numeric vector of length 1
    # indicating the number of completely observed observations (on all
    # variables) required to compute the correlation between nitrate and
    # sulfate; the default is 0.  Return a numeric vector of correlations.

    # --- Assert create an empty numeric vector
    corrsNum <- numeric(0)

    # --- Assert get a data frame as ID = 1:332
    nobsDfr <- complete("specdata")

    # --- Assert apply threshold
    nobsDfr <- nobsDfr[nobsDfr$nobs > threshold, ]

    for (cid in nobsDfr$id) {
        # --- Assert get a data frame as ID in $id
        monDfr <- getmonitor(cid, directory)

        # --- Assert calculate correlation between $sulfate and $nitrate
        corrsNum <- c(corrsNum, cor(monDfr$sulfate, monDfr$nitrate, use = "pairwise.complete.obs"))
    }

    # --- Assert return value is a numeric vector of correlations
    return(corrsNum)
}
