# Source: https://github.com/marimuraki/reproducible_research
# Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) are most harmful with respect to population health?
# Across the United States, which types of events have the greatest economic consequences?

rm(list=ls())

library(car)
library(ggplot2)

setwd("/Users/marimuraki/Dropbox/Mari/courses/Coursera/Reproducible Research/project2")

url    <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zip    <- "./data/repdata-data-StormData.csv.bz2" 
file   <- "./data/repdata-data-StormData.csv"

if (!file.exists(zip)) {
  download.file(url, 
                destfile=zip)
}

if (!file.exists(file)) {
  unzip(zip, 
        exdir="./data")
  file.remove(zip)
}

data <- read.csv(file, 
                 sep=",",
                 na.string = "NA",
                 header=TRUE)

# EVTYPE: Event Type (e.g. tornado, flood, etc.)
# FATALITIES: Number of fatalities
# INJURIES: Number of injuries
# PROPDMG: Property damage estimates, entered as actual dollar amounts
# PROPDMGEXP: Alphabetic Codes to signify magnitude “K” for thousands, “M” for millions, and “B” for billions)
# CROPDMG: Crop damage estimates, entered as actual dollar amounts
# CROPDMGEXP: Alphabetic Codes to signify magnitude “K” for thousands, “M” for millions, and “B” for billions)

# Subset data to only key variables

sublist <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
stormdata <- subset(data, select=sublist)

# Clean data

# Cleaning up main EVTYPE categories e.g., misspellings

stormdata$EVTYPE <- tolower(stormdata$EVTYPE)
stormdata$EVTYPE[grepl("blizzard", stormdata$EVTYPE, ignore.case = TRUE)]        <- "blizzard"  
stormdata$EVTYPE[grepl("cold", stormdata$EVTYPE, ignore.case = TRUE)]            <- "cold"  
stormdata$EVTYPE[grepl("fire", stormdata$EVTYPE, ignore.case = TRUE)]            <- "fire"  
stormdata$EVTYPE[grepl("flood", stormdata$EVTYPE, ignore.case = TRUE)]           <- "flood"  
stormdata$EVTYPE[grepl("hail", stormdata$EVTYPE, ignore.case = TRUE)]            <- "hail"  
stormdata$EVTYPE[grepl("heat", stormdata$EVTYPE, ignore.case = TRUE)]            <- "heat"  
stormdata$EVTYPE[grepl("high surf", stormdata$EVTYPE, ignore.case = TRUE)]       <- "high surf"
stormdata$EVTYPE[grepl("hurricane", stormdata$EVTYPE, ignore.case = TRUE)]       <- "hurricane"  
stormdata$EVTYPE[grepl("lightn", stormdata$EVTYPE, ignore.case = TRUE)]          <- "lightning"  
stormdata$EVTYPE[grepl("mud.*slide", stormdata$EVTYPE, ignore.case = TRUE)]      <- "mudslide"  
stormdata$EVTYPE[grepl("rain", stormdata$EVTYPE, ignore.case = TRUE)]            <- "rain"  
stormdata$EVTYPE[grepl("precip", stormdata$EVTYPE, ignore.case = TRUE)]          <- "rain"  
stormdata$EVTYPE[grepl("rip current", stormdata$EVTYPE, ignore.case = TRUE)]     <- "rip current"
stormdata$EVTYPE[grepl("snow", stormdata$EVTYPE, ignore.case = TRUE)]            <- "snow"  
stormdata$EVTYPE[grepl("storm surge", stormdata$EVTYPE, ignore.case = TRUE)]     <- "storm surge"  
stormdata$EVTYPE[grepl("thun.*orm", stormdata$EVTYPE, ignore.case = TRUE)]       <- "thunderstorm"  
stormdata$EVTYPE[grepl("tstm", stormdata$EVTYPE, ignore.case = TRUE)]            <- "thunderstorm"  
stormdata$EVTYPE[grepl("tornad", stormdata$EVTYPE, ignore.case = TRUE)]          <- "tornado"  
stormdata$EVTYPE[grepl("tropical.*storm", stormdata$EVTYPE, ignore.case = TRUE)] <- "tropical storm"  
stormdata$EVTYPE[grepl("wind", stormdata$EVTYPE, ignore.case = TRUE)]            <- "wind"  
stormdata$EVTYPE[grepl("winter.*mix", stormdata$EVTYPE, ignore.case = TRUE)]     <- "winter mix"
stormdata$EVTYPE[grepl("winter storm", stormdata$EVTYPE, ignore.case = TRUE)]    <- "winter storm"  
stormdata$EVTYPE[grepl("volcanic", stormdata$EVTYPE, ignore.case = TRUE)]        <- "volcanic"  

# Converting CROPDMG & CROPDMGEXP, PROPDMG & PROPDMGEXP >> $

stormdata$PROPDMCROPDMGEXP <- tolower(stormdata$CROPDMGEXP)
stormdata$GEXP <- tolower(stormdata$PROPDMGEXP)
convert_dollars <- "'0'=1;'1'=10;'2'=100;'3'=1000;'4'=10000;'5'=100000;'6'=1000000;'7'=10000000;'8'=100000000;'b'=1000000000;'h'=100;'k'=1000;'m'=1000000;'-'=0;'?'=0;'+'=0;=0"
stormdata$PROPDMG_dollars <- stormdata$PROPDMG * as.numeric(Recode(stormdata$PROPDMGEXP,
                                                                   convert_dollars,
                                                                   as.factor.result = FALSE))
stormdata$CROPDMG_dollars <- stormdata$CROPDMG * as.numeric(Recode(stormdata$CROPDMGEXP,
                                                                   convert_dollars,
                                                                   as.factor.result = FALSE))

# Try agAcross ited States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) are most harmful with respect to population health?

harm_pophealth <- aggregate(list (harm = stormdata$FATALITIES + stormdata$INJURIES),
                            list (EVTYPE = stormdata$EVTYPE),
                            sum)
harm_pophealth <- harm_pophealth[with (harm_pophealth, order (harm, decreasing=TRUE)),]
head(harm_pophealth)

top_harm_pophealth <- harm_pophealth[order(-harm_pophealth$harm),][1:10,]
list(top_harm_pophealth)

png(file="plot1_harm_pophealth.png")
ggplot(top_harm_pophealth, aes(x = reorder(EVTYPE, -harm), y = harm)) + 
  geom_bar(stat = "identity", aes(fill = harm), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Event Type") +
  ylab("Harmful Events") + 
  ggtitle("Harmful events (fatalities + injuries) to population health in USA")
dev.off()

# Across the United States, which types of events have the greatest economic consequences?

harm_econ <- aggregate(list (harm = stormdata$CROPDMG + stCROPDMG_dollars + stormdata$PROPDMG_dollars              list (EVTYPE = stormdata$EVTYPE),
                       sum)
harm_econ <- harm_econ[with (harm_econ, order (harm, decreasing=TRUE)),]
head(harm_econ)

top_harm_econ <- harm_econ[order(-harm_econ$harm),][1:10,]
list(top_harm_econ)

png(file="plot1_harm_econ.png")
ggplot(top_harm_econ, aes(x = reorder(EVTYPE, -harm), y = harm)) + 
  geom_bar(stat = "identity", aes(fill = harm), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Event Type") +
  ylab("Harmful Events") + 
  ggtitle("Harmful economic events (crop + property damage) to US economy")
dev.off()

