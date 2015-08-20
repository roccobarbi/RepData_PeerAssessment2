# setwd("C:/Users/User/Dropbox/R-Folder/RepRes/RepData_PeerAssessment2")

######################################
# PACKAGES AND ENVIRONMENT VARIABLES #
######################################

invisible(library(dplyr))

###################################
# FUNCTIONS DEFINED FOR LATER USE #
###################################

# do_download_data()
#
# This function checks if the file already exists. If it doesn't, or if the result is ambiguous,
# it downloads the data.
#
do_download_data <- function() {
        file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        dir.create("Data", showWarnings = FALSE)
        storm_data_file <- list.files(path = "Data", pattern = "^storm_data\\.csv.bz2$", full.names = T)
        if (length(storm_data_file) != 1) {
                download.file(file_url,"Data/storm_data.csv.bz2")
        } else {
                writeLines("\nThe storm data file is already in your data folder, no data has been downloaded.\n")
        }
}

# do_collect_garbage()
#
# This function collects garbage efficiently to free up memory
#
do_collect_garbage <- function(x) {
        writeLines("\n\nCLEANING UP MEMORY\n")
        # I noticed that a series of garbage collections works better than a single one
        # The invisible() options suppresses the output from gc()
        temp_mem <- memory.size()
        Sys.sleep(1)
        invisible(gc())
        Sys.sleep(1)
        invisible(gc())
        Sys.sleep(1)
        invisible(gc())
        Sys.sleep(1)
        invisible(gc())
        print(paste(temp_mem - memory.size(), "Mb freed."))
}

#############################
# THE LIVE CODE BEGINS HERE #
#############################

# First I download the data
do_download_data()

# Then I read the data into R and assign it to a data frame
writeLines("\nReading the data into R, please wait. This operation could take a while.\n")
storm_data <- read.csv("Data/storm_data.csv.bz2", header = T, stringsAsFactors = T)

# Freeing up some memory.
do_collect_garbage()

# To make the analyses lighter, I will remove some information that is not relevant to the assignment.
writelines("\nSubsetting the data to free up memory\n")
subset <- select(storm_data, STATE__, BGN_DATE, STATE, EVTYPE, F, MAG, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
rm(storm_data)
do_collect_garbage()

# The harmfulness can be computed in several ways:
# - frequency (probability of the event);
# - damage (mean and median for single events);
# - total damage (sum for event type).

# I clean up the data set by removing irrelevant event types (with zero aggregated damage)
evtypes_dmg <- aggregate(x = (subset[, "PROPDMG"] + subset[, "CROPDMG"]), by = list(subset[, "EVTYPE"]), FUN = "sum")
irrelevant_evtypes <- filter(evtypes_dmg, x == 0) %>% select(Group.1)
subset <- filter(subset, !(EVTYPE %in% unlist(irrelevant_evtypes)))
subset <- droplevels(subset)
do_collect_garbage()

# I tidy up the name of the unknown values.
subset$EVTYPE <- plyr::revalue(subset$EVTYPE, c("?"="UNKNOWN"))

# How many EVTYPES are there with this pattern?
length(grep(".*TSTM.*", levels(subset$EVTYPE)))