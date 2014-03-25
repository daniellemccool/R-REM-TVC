# Maarten, this isn't finished yet.  It's difficult and it
# just doesn't want to coopeate with me. At this point
# what's missing is that the length of detention doesn't
# always add up perfectly.

setwd("~/Dropbox/NWO Onderzoekstalent/bestand2009/")
b2009 <- read.table("datasets/bestand2009.dat")
b2009 <- data.frame(b2009)
names(b2009) <- c("Y", "YH", "Tact", "Tpot", "Hact", "Hpot", "constant", "man", "plus40", "A", "R", "H", "U", "Turks", "NAfrika", "Afrika", "Suriname", "OostEuropa", "Azie", "Amerika" )

b2009$days_out <- 365-b2009$Tact
b2009$detention_length <- b2009$days_out/b2009$Y
b2009$id <- 1:nrow(b2009)
b2009$workable_time <- 365-b2009$detention_length
num_rows <- (sum(b2009$Y)*2)+nrow(b2009)

long_form_matrix <- matrix(1, nrow=(num_rows), ncol=(length(b2009)+3))
for (i in 1:nrow(b2009)) {
    if(i==1){row_counter <- 1}
    num_events <- b2009[i,"Y"]
    prev_event_date <- 0
    time_stable_covs <- unlist(b2009[i, ])
    for (t in 0:(2*num_events)){
        if(t == (2*num_events)){
            start <- prev_event_date
            stop  <- 366
            new_row <- c(time_stable_covs, start, stop, 0)
            long_form_matrix[row_counter, ] <- new_row
            row_counter <- row_counter + 1
        }else if((t %% 2) == 0){
            start <- prev_event_date + 1
            # This is where the problem is. For the max time
            # available, we need to account for 365 days total
            # and then subtract at least enough time to allow for
            # the detention length to occur each time that a
            # detention is necessary.
            max <- (365  - (num_events-(t/2))*round(b2009[i,"detention_length"]) + 1)
            event_date <- round(runif(1, start, max))
            event_type <- 1
            new_row <- c(time_stable_covs, start, event_date, 1)
            long_form_matrix[row_counter, ] <- new_row
            row_counter <- row_counter + 1
            prev_event_date <- event_date
            }else if((t %% 2) == 1){
                start <- prev_event_date + 1
                if(t == (2*num_events - 1)){
                    extra_days <- b2009[i, "days_out"] - (round(b2009[i, "detention_length"]) * (num_events-1))
                    stop <- event_date + extra_days

                }else{
                    stop <- event_date + round(b2009[i, "detention_length"])
                }
                new_row <- c(time_stable_covs, start, stop, 3)
                long_form_matrix[row_counter, ] <- new_row
                row_counter <- row_counter + 1
                prev_event_date <- stop
            }
    }
}
long_form_matrix <- data.frame(long_form_matrix)
names(long_form_matrix) <- c(names(b2009), "start", "stop", "event_code")
summary(long_form_matrix)

lfm <- long_form_matrix[-which(long_form_matrix$detention_length == 0 & long_form_matrix$event_code == 3), ]

lfm[which(lfm$id == 1 & lfm$event_code ==3,]$stop - lfm[1:9, ]$start

answers <- nrow(b2009)
for (i in unique(lfm$id)) {
    answers[i] <- sum(lfm[which(lfm$id == i & lfm$event_code == 3), "stop"] -
     lfm[which(lfm$id == i & lfm$event_code == 3), "start"] + 1 ) == b2009[i, "days_out"]
}

which(answers==0)

lfm[lfm$id==1509,]