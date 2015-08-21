# setwd("C:/Users/User/Dropbox/R-Folder/RepRes/RepData_PeerAssessment2")

######################################
# PACKAGES AND ENVIRONMENT VARIABLES #
######################################

invisible(library(dplyr))
invisible(library(ggplot2))
invisible(library(scales))

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
writeLines("\nSubsetting the data to free up memory\n")
subset <- select(storm_data, BGN_DATE, EVTYPE, MAG, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
rm(storm_data)
do_collect_garbage()

# The harmfulness can be computed in several ways:
# - frequency (probability of the event);
# - damage (mean and median for single events);
# - total damage (sum for event type).

# I clean up the data set by removing irrelevant event types (with zero aggregated damage)
writeLines("\nSubsetting even more to get rid of inconsequential events.\n")
evtypes_dmg <- aggregate(x = (subset[, "PROPDMG"] + subset[, "CROPDMG"] + subset[, "INJURIES"] + subset[, "FATALITIES"]), by = list(subset[, "EVTYPE"]), FUN = "sum")
irrelevant_evtypes <- filter(evtypes_dmg, x == 0) %>% select(Group.1)
subset <- filter(subset, !(EVTYPE %in% unlist(irrelevant_evtypes)))
subset <- droplevels(subset)
do_collect_garbage()

# I copied by hand a list of the correct event type values

correct_event_types <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Freezing Fog", "Frost/Freeze", "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane/Typhoon", "Ice Storm", "Lakeshore Flood", "Lake-Effect Snow", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")

# I tidy up the name of the unknown values.
subset$EVTYPE <- plyr::revalue(subset$EVTYPE, c("?"="UNKNOWN"))

# How many EVTYPES are there with this pattern?
length(grep(".*TSTM.*", levels(subset$EVTYPE)))
grep("tornado", levels(subset$EVTYPE), value = T, ignore.case = T)

# I actually clean up the levels (first set)
levels(subset$EVTYPE)<- gsub(".*waterspout.*", "WATERSPOUT", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*tornado.*", "TORNADO", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*gustnado.*", "TORNADO", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*torndao.*", "TORNADO", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("^thunderstorm hail$", "HAIL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^thunderstorm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*microburst.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*mircoburst.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^lightning.*thunderstorm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^severe.*thunderstorm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^tstm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^tuderstorm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^tunderstorm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^thuderstorm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^ tstm.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*hail.*", "HAIL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("MARINE TSTM WIND", "MARINE THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^thun.*", "THUNDERSTORM WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*avalanche.*", "AVALANCHE", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*blizzard.*", "BLIZZARD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*dust storm.*", "DUST STORM", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*ice storm$", "ICE STORM", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*winter storm.*", "WINTER STORM", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*tropical storm.*", "TROPICAL STORM", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*flash.*flood.*", "FLASH FLOOkD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*flood.*flash.*", "FLASH FLOOkD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*coast.*flood.*", "COASTAL FLOOkD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*cstl.*flood.*", "COASTAL FLOOKD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*lake.*flood.*", "LAKESHORE FLOOkD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*mud.*", "MUDSLIDES", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*flood.*", "FLOOD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("FLASH FLOOKD", "FLASH FLOOD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("COASTAL FLOOKD", "COASTAL FLOOD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("LAKESHORE FLOOKD", "LAKESHORE FLOOD", levels(subset$EVTYPE), ignore.case = T)

grep("hail", levels(subset$EVTYPE), value = T, ignore.case = T)

# And I drop the unused ones.
subset <- droplevels(subset)

# Finally I clean up some memory
do_collect_garbage()

# I actually clean up the levels (second set)
levels(subset$EVTYPE)<- gsub(".*devil.*", "DUST DEVIL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("BLOWING DUST", "DUST STORM", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*drought.*", "DROUGHT", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".+heat", "EXCESSIVE HEAT", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^heat.*", "HEAT", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^(?!.*ext).*chill.*", "COLD/WINKD CHILL", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*ext.*chill.*", "EXTREME COLD/WINKD CHILL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*lake.*snow.*", "LAKE-EFFECT SNOW", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*hea.*snow.*", "HEAVY SNOW", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*sleet.*", "SLEET", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^(?!.*marine).*high.*wind.*", "HIGH WIND", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("^(?!.*marine).*strong.*wind.*", "STRONG WIND", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*marine.*high.*wind.*", "HIGH WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*marine.*strong.*wind.*", "STRONG WIND", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^(?!thunder)(?!marine)(?!strong)(?!high).*wind.*", "WIND", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("COLD/WINKD CHILL", "COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("EXTREME COLD/WINKD CHILL", "EXTREME COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^(?!.*lake).*snow.*", "HEAVY SNOW", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*hurricane.*", "HURRICANE/TYPHOON", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*typhoon.*", "HURRICANE/TYPHOON", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*lightning.*", "LIGHTNING", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*lighting.*", "LIGHTNING", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*fire.*", "WILDFIRE", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*rain.*", "HEAVY RAIN", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*fld.*", "FLOOD", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*warm.*", "HEAT", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*coast.*storm.*", "COASTAL STORM", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*freez.*fog.*", "FREEZING FOG", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^(?!.*freez).*fog.*", "DENSE FOG", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*fre.*", "FROST/FREEZE", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*extreme.*cold.*", "EXTREME COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*record.*cold.*", "EXTREME COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*hyp.*", "EXTREME COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("EXTREME COLD/WIND CHILL", "EXTREME COLKD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*cold.*", "COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("EXTREME COLKD/WIND CHILL", "EXTREME COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*rip.*curr.*", "RIP CURRENT", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*wint.*we.*", "WINTER WEATHER", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*wint.*mix.*", "WINTER WEATHER", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub(".*surf.*", "HIGH SURF", levels(subset$EVTYPE), ignore.case = T)
levels(subset$EVTYPE)<- gsub("^ice.*(?<!storm)$", "ICE", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*glaze.*", "ICE", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("black ice", "ICE", levels(subset$EVTYPE), ignore.case = T, perl = T)

grep("(?!.*lake)snow", levels(subset$EVTYPE), value = T, ignore.case = T, perl = T)

# And I drop the unused ones.
subset <- droplevels(subset)

# Finally I clean up some memory
do_collect_garbage()

# Step 3 of the cleanup: residuals.
levels(subset$EVTYPE)<- gsub(".*land.*", "LANDSLIDE", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("^mixed.*", "MIXED PRECIPITATION", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("HEAVY PRECIPITATION", "HEAVY RAIN", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("HEAVY MIX", "MIXED PRECIPITATION", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*storm surge.*", "STORM SURGE", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("^high$", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*urban.*", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*marine.*(?<!wind)$", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*erosion.*", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*other.*", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*SWELLS.*", "HEAVY SEAS", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*sea.*", "HEAVY SEAS", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*county.*", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*wet.*", "OTHER", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub(".*early frost.*", "FROST", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("low temperature", "COLD/WIND CHILL", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("RAPIDLY RISING WATER", "FLOOD", levels(subset$EVTYPE), ignore.case = T, perl = T)
levels(subset$EVTYPE)<- gsub("ROGUE WAVE", "TSUNAMI", levels(subset$EVTYPE), ignore.case = T, perl = T)

# And I drop the unused ones.
subset <- droplevels(subset)

# Finally I clean up some memory
do_collect_garbage()

# Population health analysis

pop_health <- select(subset, EVTYPE, FATALITIES, INJURIES)
pop_health <- group_by(pop_health, EVTYPE) %>% summarize(fatalities = sum(FATALITIES), injuries = sum(INJURIES))

hist(pop_health$fatalities)
hist(pop_health$injuries)
barplot(pop_health$fatalities)
plot(pop_health$fatalities, pop_health$injuries)
pop_health[pop_health$fatalities == max(pop_health$fatalities),]

inj_fat_plot <- ggplot(data = pop_health, aes(x = fatalities, y = injuries)) 
inj_fat_plot <- inj_fat_plot + geom_point(shape = 1, size = 3, col = "red")
inj_fat_plot <- inj_fat_plot + scale_x_continuous(name = "Fatalities")
inj_fat_plot <- inj_fat_plot + scale_y_continuous(name = "Injuries")
inj_fat_plot <- inj_fat_plot + ggtitle("Injuries and Fatalities across all event types")
inj_fat_plot <- inj_fat_plot + theme(plot.title = element_text(lineheight=1.5, face="bold", size = 24))
inj_fat_plot

head(arrange(pop_health, desc(fatalities)), 10)

# Damage to property

eco_dmg_count <- select(subset, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
eco_dmg_count <- cbind(eco_dmg_count, count = rep(1, nrow(eco_dmg_count)))
eco_dmg_count$PROPDMGEXP <- as.factor(eco_dmg_count$PROPDMGEXP)
propdmgexp_count <- group_by(eco_dmg_count, PROPDMGEXP)
#%>% summarize(number_of_events = sum(count), economic damage = mean(PROPDMG)))