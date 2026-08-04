###### Set Working Directory
setwd("C:/R_work_directory/QAQC")

### ============================================================================
###### CODE WILL TAKE CARE OF THE REST
### ============================================================================

###### Load libraries 
library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)
library(tidyr)

###### Read in file from RAW_DATA folder  --------------------------------------
filenames <- list.files(".//raw_data//", pattern="*RAW.csv")
date <- max(substr(filenames, 1, 8))
dat <- read.csv(paste(".//raw_data//", date, "_RAW.csv", sep=""))

###### Make all text columns as.character --------------------------------------
dat$notes <- as.character(dat$notes)
dat$edits <- as.character(dat$edits)
dat$effort_edits <- as.character(dat$effort_edits)
dat$notes[is.na(dat$notes)] <- ""
dat$edits[is.na(dat$edits)] <- ""

###### Clean and convert TrackTime to numeric HHMMSS ---------------------------
dat <- dat %>%
  rename(TrkTime = matches("TrkTime")) %>%
  mutate(TrkTime = as.numeric(gsub(":", "", str_extract(TrkTime, "\\d{2}:\\d{2}:\\d{2}"))))

###### Calculate time gaps between rows ----------------------------------------
## Convert HHMMSS to total seconds, then get gap (sec and min) between consecutive rows
Mysti_outages = dat %>%
  mutate(Time_Seconds = (TrkTime %/% 10000) * 3600 + ((TrkTime %/% 100) %% 100) * 60 + (TrkTime %% 100),
         Seconds_Gap  = Time_Seconds - lag(Time_Seconds),
         Min_Gap      = round(Seconds_Gap / 60, 2),
         Time_prev    = lag(TrkTime)) %>%
  select(Time_prev, Time = TrkTime, Time_Seconds, Seconds_Gap, Min_Gap)

outage_ranges = Mysti_outages %>%
  filter(Seconds_Gap > 5) %>%
  mutate(Min_Gap_clean = ifelse(Min_Gap == round(Min_Gap), sprintf("%dmin", round(Min_Gap)), sprintf("%.1f min", Min_Gap)),
         range = paste0(sprintf("%06d", Time_prev), "-", sprintf("%06d", Time), " (", Min_Gap_clean, ")")) %>%
  pull(range)

if(length(outage_ranges) == 0) outage_ranges = "None!"

##### Adjust Columns -----------------------------------------------------------
dat <- dat %>%
  mutate(
    rectype = pmax(rectype, rectypeEFFORT, na.rm = TRUE),
    edits   = pmax(edits,   effort_edits,  na.rm = TRUE),
    anglel  = ifelse(rectype == "S", NA, angle),
    angler  = ifelse(rectype == "P", NA, angle)
  ) %>%
  select(-any_of(c("gpsspeed", "HeadingPlatMagnetic..T.", "effort_edits", "rectypeEFFORT", "angle"))) %>%
  rename(
    time      = TrkTime,
    lat       = TrkLatitude,
    long      = TrkLongitude,
    alt       = matches("TrkAltitude"),
    heading   = matches("HeadingPlatTrue"),
    gpsspeed  = matches("PlatformSpeed"),
    TrackDist = matches("TrkDist")
  ) %>%
  select(22, 8:11, 1:3, 5, 4, 13:15, 21, 16:20, 23, 65:66, 24:45, 12, 48:51, 6, 52:62, 47, 46, 63:64, 7)

###### Fill in leg and env info & remove out-of-survey rows --------------------
## Will keep off-watch sightings and lunch
dat[dat == ''] <- NA
dat <- dat %>%
  fill(legtype, legstage, legno, glarel, glarer, beaufort, cloud, wx, visiblty, block, .direction = "down") %>%
  mutate(
    legtype  = ifelse(!is.na(rectype) & is.na(legtype),  "0", legtype),
    legstage = ifelse(!is.na(rectype) & is.na(legstage), "-", legstage)
  ) %>%
  filter(!is.na(legtype)) %>%
  mutate(in_survey = row_number() >= min(which(legtype != "0")) &
           row_number() <= max(which(legtype != "0"))) %>%
  filter(!(legtype == "0" & is.na(rectype) & !in_survey)) %>%
  select(-in_survey)

###### Fill-in eventnos --------------------------------------------------------
dat$eventno <- seq.int(nrow(dat))

###### Fill-in setalt and setvel -----------------------------------------------
## setalt is 1500 when block=M2 and 1000 for all other blocks (or if block is blank)
dat <- dat %>%
  mutate(setalt = ifelse(any(block == "M2"), 1500, 1000))
dat$setvel <- as.numeric(100)

###### Format number of decimal places -----------------------------------------
dat[,'heading'] <- round(dat[,'heading'], 0)
dat[,'alt']     <- round(dat[,'alt'],     0)
dat[,'gpsspeed'] <- round(dat[,'gpsspeed'], 1)

###### Rectypes that are not P or S become X -----------------------------------
dat <- dat %>%
  mutate(rectype = ifelse(is.na(rectype) | !rectype %in% c("P", "S"), "X", rectype))

###### Remove duplicate 2/1, 2/3, 2/4, and 2/5 rows ----------------------------
triggers <- c("21", "23", "24", "25")

dat <- dat %>% mutate(type.stage = paste0(legtype, legstage), new.type.stage = type.stage)

## For loop to find repeats and replace with next different type.stage
for (i in 2:nrow(dat)) {
  if (dat$type.stage[i] %in% triggers && dat$type.stage[i] == dat$type.stage[i - 1]) {
    if (i < nrow(dat)) {
      remaining <- dat$type.stage[(i + 1):nrow(dat)]
      next_diff <- remaining[remaining != dat$type.stage[i]][1]
      if (!is.na(next_diff)) {
        dat$new.type.stage[i] <- next_diff
      }
    }
  }
}

dat <- dat %>%
  mutate(legtype  = substr(new.type.stage, 1, 1),
         legstage = substr(new.type.stage, 2, 2)) %>%
  select(-type.stage, -new.type.stage)

###### Clean up glare and legno ------------------------------------------------
dat <- dat %>%
  mutate(
    glarel = ifelse(legtype != "2", "", glarel),
    glarer = ifelse(legtype != "2", "", glarer),
    legno  = ifelse(legtype %in% c("0", "1", "3"), "", legno)
  )

###### Fill-in sightnos --------------------------------------------------------
dat <- dat %>%
  select(-sightno) %>%
  mutate(sightno = ifelse(rectype %in% c("P", "S"),
                          cumsum(rectype %in% c("P", "S")),
                          "")) %>%
  relocate(sightno, .after = wx)

###### Export data file to Outputs folder --------------------------------------
## Make all NAs blank
dat[is.na(dat)] <- ""
write.csv(dat, paste0("output/", date, "_URI.csv"), row.names=FALSE)
message("URI exported to Output folder")

###### Print Mysti time gaps larger than 5 seconds -----------------------------
cat("Mysti outages (or refuel break):", outage_ranges, sep = "\n")
