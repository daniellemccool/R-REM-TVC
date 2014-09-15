giveMeData <- function() {
    setwd("~/Dropbox/NWO Onderzoekstalent/R code")
    # Combining data and setup
    require(foreign)
    data <- read.spss("datasets/full.data.SAV", to.data.frame = TRUE)
    data2 <- read.spss("datasets/detentiontimes.sav", to.data.frame = TRUE)
    data3 <- read.spss("datasets/bestand2009-full.sav", to.data.frame = TRUE)

    in.data.1     <- duplicated(c(data$vnummer, data2$vnummer))[-c(1:4330)]
    not.in.data.1 <- !duplicated(c(data$vnummer, data2$vnummer))[-c(1:4330)]

    in.data.2     <- duplicated(c(data2$vnummer, data$vnummer))[-c(1:2913)]
    not.in.data.2 <- !duplicated(c(data2$vnummer, data$vnummer))[-c(1:2913)]

    missing.data <- data[not.in.data.2, ]
    missing.data2 <- data2[not.in.data.1, ]

    data2 <- data2[in.data.1, ] # Four missing from other set
    data2 <- data2[-24, ] # Duplicate row
    data1_r <- data[not.in.data.2, ]
    data3_r <- data3[in.data.2,]

    weather <- read.table("datasets/2009.weather.txt", header = TRUE, sep = ',')
    weather <- weather[, 3]

    all(data2$vnummer == data1_r$vnummer)

    combined.data <- data.frame()
    combined.data <- cbind(data2[,1:3],
                     data2$aantalregistraties_totaal, data3_r$Turks,
                     data3_r$NAfrika, data3_r$Afrika, data3_r$Suriname,
                     data3_r$OostEuropa, data3_r$Azie, data3_r$Amerika,
                     data2$registratiedag.1, data2$registratiedag.2,
                     data2$registratiedag.3, data2$registratiedag.4,
                     data2$registratiedag.5, data2$registratiedag.6,
                     data2$registratiedag.7, data2$Regio.1_tot,
                     data2$REGIO.2, data2$REGIO.3, data2$REGIO.4,
                     data2$REGIO.5, data2$REGIO.6, data2$REGIO.7,
                     data2$IBS_dag.1, data2$IBS_dag.2, data2$IBS_dag.3,
                     data2$IBS_dag.4, data2$IBS_dag.5, data2$IBS_dag.6,
                     data2$IBS_dag.7, data2$aantaldetenties, data2$dagbegindetentie1,
                     data2$dagbegindetentie2, data2$dagbegindetentie3,
                     data2$dagbegindetentie4, data2$dagbegindetentie5,
                     data2$dagbegindetentie6, data2$dagbegindetentie7,
                     data2$daguitstroom1, data2$daguitstroom2, data2$daguitstroom3,
                     data2$daguitstroom4, data2$daguitstroom5, data2$daguitstroom6,
                     data2$daguitstroom7, data3_r[,13:18])


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
    data[data[,"Age"] <= 40, "Age"] <- 0
    data[data[,"Age"] > 40, "Age"] <- 1

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
                if(events[i, begin] == 1){
                    print(c("Conflict", i, t))
                    begin <- begin + 1
                }

                end       <- dete.cols[i, which.det]
                X.arr[i, t:end, -1] <- covs[i,]
                t <- end

                if(!(begin >= 365)){
                    events[i, begin:end] <- 3
                }
            }
            t <- t + 1
        }
        if (i%%100 == 0){print(i)}

    }

    return(list(events = events, X.arr = X.arr))
}
