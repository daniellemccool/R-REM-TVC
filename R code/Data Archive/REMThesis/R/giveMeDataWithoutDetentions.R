#' Setup data from empirical example
#'
#' This function just exists for me to more easily get the data from my empirical example into a usable format.
#' @param weather Whether or not to use the year's weather in the calculations. Defaults to FALSE.
#' @keywords data import
#' @export
#' @examples
#' giveMeDataWithoutDetentions()

giveMeDataWithoutDetentions <- function(weather = FALSE) {
    setwd("~/Dropbox/NWO Onderzoekstalent/R code")
    # Combining data and setup
    data <- read.spss("datasets/full.data.SAV", to.data.frame = TRUE)
    data3 <- read.spss("datasets/bestand2009-full.sav", to.data.frame = TRUE)

    new.data <- cbind(data[,c(1, 2, 3)],
                    data3[, 5:12],
                    data[, c(18, 20, 22, 24, 26, 28,
                    30, 32, 34, 35, 36, 37, 38, 39)],
                    data3[, 13:16])


    new.data <- new.data[!new.data$WestEuropa == 1, ]
    new.data <- new.data[, -8]

    new.names <- c("ID", "Sex", "Age", "Turkish", "NorthAfrican",
    "African", "Surinamese", "EasternEuropean","Asian", "American", "obsday.1",
    "obsday.2", "obsday.3", "obsday.4",
    "obsday.5", "obsday.6", "obsday.7",
    "region.1", "region.2", "region.3", "region.4", "region.5",
    "region.6", "region.7", "Amsterdam", "Den Haag",
    "Rotterdam", "Utrecht")

    colnames(new.data) <- new.names

    new.data <- new.data[!is.na(new.data$Age), ]
    new.data[, 25:28] <- 0
    new.data <- data.matrix(new.data)

    N            <- nrow(new.data)
    Time         <- max(new.data[, "obsday.1"])
    cov.names    <- c("intercept", "Sex", "Age", "Turkish", "NorthAfrican", "African", "Surinamese", "EasternEuropean", "Asian", "American", "Amsterdam", "Den Haag", "Rotterdam", "Utrecht")
    num.covs     <- length(cov.names)
    covs         <- new.data[, c(cov.names[-1])]
    obs.day.cols <- new.data[, c("obsday.1", "obsday.2", "obsday.3", "obsday.4", "obsday.5", "obsday.6", "obsday.7")]
    region.cols  <- new.data[, c("region.1", "region.2", "region.3", "region.4", "region.5", "region.6", "region.7")]
    events       <- matrix(0, nrow = N, ncol = Time)
    X.arr        <- array(0, c(N, num.covs, Time), dimnames = list(NULL, cov.names, paste0('d', 1:Time)))
    X.arr[, ,]   <- cbind(1, covs)
    X.arr        <- aperm(X.arr, perm = c(1, 3, 2))
    region       <- 0


    for (i in 1:N) {
        for (j in 1:Time) {
            if(any(obs.day.cols[i, ] == j, na.rm = TRUE)){
                events[i, j] <- 1
                day.col <- (which(obs.day.cols[i, ] == j))
                region  <- region.cols[i, day.col]
                if(is.na(region)){region <- 0}

                if(region == 10){covs[i, "Utrecht"] <- 1; covs[i, c("Amsterdam", "Rotterdam", "Den Haag")] <- 0 }
                if(region == 14){covs[i, "Amsterdam"] <- 1; covs[i, c("Utrecht", "Rotterdam", "Den Haag")] <- 0}
                if(region == 16){covs[i, "Den Haag"] <- 1; covs[i, c("Utrecht", "Amsterdam", "Rotterdam")] <- 0}
                if(region == 18){covs[i, "Rotterdam"] <- 1; covs[i, c("Utrecht", "Amsterdam", "Den Haag")] <- 0}
                X.arr[i, j, -1] <- covs[i, ]
            }
        }
        if (i%%100 == 0){print(i)}

    }


    if(weather == TRUE){
        weather     <- read.table("datasets/2009.weather.txt", header = TRUE, sep = ',')
        weather     <- weather[, 3]
        sc.weather  <- scale(weather, center = TRUE, scale = TRUE)

        weather.mat <- matrix(sc.weather, ncol = 365, nrow = 4234, byrow = TRUE)
        X.arr       <- abind(X.arr, weather.mat)
    }

    return(list(events = events, X.arr = X.arr))
}
