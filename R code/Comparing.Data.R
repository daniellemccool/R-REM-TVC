# Comparing Data
setwd("~/Dropbox/NWO Onderzoekstalent/R code")
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



