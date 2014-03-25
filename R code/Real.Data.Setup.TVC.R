setwd("~/Dropbox/NWO Onderzoekstalent/R code")
source(Comparing.Data.R)

data <- combined.data
new.names <- c("ID", "Sex", "Age", "Y", "Turkish", "NorthAfrican",
"African", "Surinamese", "EasternEuropean","Asian", "American", "regday.1",
"regday.2", "regday.3", "regday.4",
"regday.5", "regday.6", "regday.7",
"region.1", "region.2", "region.3", "region.4", "region.5",
"region.6", "region.7", "ibsday.1", "ibsday.2", "ibsday.3",
"ibsday.4", "ibsday.5", "ibsday.6", "ibsday.7",
"totaldetentions", "detbegin.1", "detbegin.2",
"detbegin.3", "detbegin.4", "detbegin.5",
"detbegin.6", "detbegin.7", "detend.1",
"detend.2", "detend.3", "detend.4", "detend.5",
"detend.6", "detend.7", "Amsterdam", "DenHaag",
"Rotterdam", "Utrecht", "Reason", "Result")

names(data) <- new.names
data <- data[!is.na(data$regday.1), ]
data <- data[!is.na(data$Age), ]
data <- data.matrix(data)

N            <- nrow(data)
Time         <- max(data[, "regday.1"])
cov.names    <- c("intercept", "Sex", "Age", "Turkish", "NorthAfrican", "African", "Surinamese", "EasternEuropean", "Asian", "American", "Amsterdam", "DenHaag", "Rotterdam", "Utrecht", "Result")
num.covs     <- length(cov.names)
covs         <- data[, c(cov.names[-1])]
obs.day.cols <- data[, c("regday.1", "regday.2", "regday.3", "regday.4", "regday.5", "regday.6", "regday.7")]
region.cols  <- data[, c("region.1", "region.2", "region.3", "region.4", "region.5", "region.6", "region.7")]
detb.cols    <- data[, c("detbegin.1", "detbegin.2", "detbegin.3", "detbegin.4", "detbegin.5", "detbegin.6", "detbegin.7")]
detb.cols    <- detb.cols + 1
detb.cols[(detb.cols > 365) ] <- 365
dete.cols    <- data[, c("detend.1", "detend.2", "detend.3", "detend.4", "detend.5", "detend.6", "detend.7")]
events       <- matrix(0, nrow = N, ncol = Time)
X.arr        <- array(1, c(N, Time, num.covs), dimnames = list(NULL, paste0('d', 1:Time), cov.names))
region       <- 0


for (i in 1:N) {
    t <- 1
    while (t <= Time) {
        if(any(obs.day.cols[i, ] == t, na.rm = TRUE)){
            events[i, t] <- 1
            day.col <- (which(obs.day.cols[i, ] == t))
            region  <- region.cols[i, day.col]
            if(is.na(region)){region <- 0}

            if(region == 10){covs[i, "Utrecht"] <- 1; covs[i, c("Amsterdam", "Rotterdam", "DenHaag")] <- 0 }
            if(region == 14){covs[i, "Amsterdam"] <- 1; covs[i, c("Utrecht", "Rotterdam", "DenHaag")] <- 0}
            if(region == 16){covs[i, "DenHaag"] <- 1; covs[i, c("Utrecht", "Amsterdam", "Rotterdam")] <- 0}
            if(region == 18){covs[i, "Rotterdam"] <- 1; covs[i, c("Utrecht", "Amsterdam", "DenHaag")] <- 0}
            if(day.col == 1){X.arr[i, 1:t, -1] <- rep(covs[i, ], each = t)}
        }
        X.arr[i, t, -1] <- covs[i,]

        if(any(detb.cols[i, ] == t, na.rm = TRUE)){
            which.det <- (which(detb.cols[i, ] == t))
            begin     <- detb.cols[i, which.det]
            end       <- dete.cols[i, which.det]

            events[i, begin:end] <- 3
            t <- end
        }
        t <- t + 1
    }
    if (i%%100 == 0){print(i)}

}

beta <- rep(0, dim(X.arr)[3])
res <- optim(b, rem.mtvc.lik, x.arr = X.arr, ev.mat = events, method = "BFGS")

# b <-  c(-6.74590150, -1.02313410,  0.01254615, -2.31313407, -0.90668807, -0.17576957,
#  -1.47879437,  0.10846263,  0.06261460, -0.22263049,  0.45141534,  0.16560606,
#  -0.01438129, -1.14524373, -0.03453087)
b <- res$par

getNHat(X.arr, b) #[1] 33589.68


