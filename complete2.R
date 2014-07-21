complete <- function(directory, id = 1:332) {
    # --- Assert 'directory' is a character vector of length 1 indicating the
    # location of the CSV files.  'id' is an integer vector indicating the
    # monitor ID numbers to be used Return a data frame of the form: id nobs 1
    # 117 2 1041 ...  where 'id' is the monitor ID number and 'nobs' is the
    # number of complete cases

    # --- Assert create an empty vector
    nobsNum <- numeric(0)

    for (cid in id) {
        # --- Assert get data frame as ID
        cDfr <- getmonitor(cid, directory)

        # --- Assert count the number of complete cases and append to numeric
        # vector
        nobsNum <- c(nobsNum, nrow(na.omit(cDfr)))
    }

    # --- Assert return value is a data frame with TWO (2) columns
    data.frame(id = id, nobs = nobsNum)
}