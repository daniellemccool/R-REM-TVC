require(foreign)
setwd("~/Dropbox/NWO Onderzoekstalent/R code")
b2009 <- read.spss("datasets/bestand2009-full.sav", to.data.frame = TRUE)
summary(b2009)
data <- read.spss("datasets/full.data.SAV", to.data.frame = TRUE)
data <- data[, -33] # This removes the unnecessary Regio.1 column (we use Regio.1_tot)
data <- cbind(data, b2009[,5:18]) # Adds the dummy variables for arrest region and nationality
data <- data.matrix(data) # Speeds everything up 10fold.
data <- data[!is.na(data[,"leeftijd"]),] # There were 23 NA values here that were messing everything else up

N            <- nrow(data)
Time         <- max(data[, "obs_dag.1"])
cov.names    <- c("intercept", "geslacht", "leeftijd", "Turks", "NAfrika", "Afrika", "Suriname", "WestEuropa", "OostEuropa", "Azie", "Amerika", "Adam", "Haag", "Rdam", "Utrecht", "R")
num.covs     <- length(cov.names)
covs         <- data[, c("Geslacht", "leeftijd", "Turks", "NAfrika", "Afrika", "Suriname", "WestEuropa", "OostEuropa", "Azie", "Amerika", "Adam", "Haag", "Rdam", "Utrecht", "R")]
obs.day.cols <- data[, c("obs_dag.1", "obs_dag.2", "obs_dag.3", "obs_dag.4", "obs_dag.5", "obs_dag.6", "obs_dag.7")]

events       <- matrix(0, nrow = N, ncol = Time)
X.arr        <- array(1, c(N, Time, num.covs), dimnames = list(NULL, paste0('d', 1:Time), cov.names))
region       <- 0

for (i in 1:N) {
    for (t in 1:Time) {
        if(any(obs.day.cols[i, ] == t, na.rm = TRUE)){
            events[i, t] <- 1
            day.col <- (which(obs.day.cols[i, ] == t))
            region  <- data[i, (day.col + 31)]
            if(is.na(region)){region <- 0}

            if(region == 10){covs[i, "Utrecht"] <- 1; covs[i, c("Adam", "Rdam", "Haag")] <- 0}
            if(region == 14){covs[i, "Adam"] <- 1; covs[i, c("Utrecht", "Rdam", "Haag")] <- 0}
            if(region == 16){covs[i, "Haag"] <- 1; covs[i, c("Utrecht", "Adam", "Rdam")] <- 0}
            if(region == 18){covs[i, "Rdam"] <- 1; covs[i, c("Utrecht", "Adam", "Haag")] <- 0}
            if(day.col == 1){X.arr[i, 1:t, -1] <- rep(covs[i, ], each = t)}
        }
        X.arr[i, t, -1] <- covs[i,]
    }
    print(i)

}

beta <- rep(0, dim(X.arr)[3])
optim(beta, rem.mtvc.lik, x.arr = X.arr, ev.mat = events, method = "BFGS")
