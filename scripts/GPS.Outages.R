###### Set Working Directory ----------------------------------------------------
setwd("C:/R_work_directory/GPS Outages")

###### Set Outage Thresholds (seconds) -------------------------------------------
### These are the ACCEPTABLE time gaps in SECONDS
Mysti_acc_seconds = 5
GETAC_acc_seconds = 4

##----------------------------------------------------------------------------##
###### CODE WILL TAKE CARE OF THE REST
##----------------------------------------------------------------------------##

###### Libraries ---------------------------------------------------------------
library(tidyverse)
library(xml2)

###### Function to sort printed results by time---------------------------------
sort_ranges = function(ranges) {
  if (length(ranges) == 0) return(ranges)
  start_times = as.POSIXct(sub("-.*", "", ranges), format = "%H:%M:%S")
  ranges[order(start_times)]
}

###### Read in RAW and GETAC data ----------------------------------------------
### RAW with most recent data
filenames = list.files(pattern="*RAW.csv")
m.date = max(substr(filenames, 1, 8))
RAW = read.csv(paste0(getwd(),"/", m.date, "_RAW.csv"))

### GPX file with most recent data
filenames = list.files(pattern="*All.gpx")
g.date = max(substr(filenames, 1, 8))
GPX = read_xml(paste0(getwd(),"/", g.date, "All.gpx"))

### Confirm file dates match
if(m.date != g.date){
  stop(paste("RAW file is from", m.date, "\n", "GPX file is from", g.date))
}

###### Convert GPX file to data frame
#### Extract trackpoints
trkpts = xml_find_all(GPX, ".//trkpt")
GETAC = data.frame(
  Time.EDT = xml_text(xml_find_first(trkpts, "./time")),
  Lat  = as.numeric(xml_attr(trkpts, "lat")),
  Long  = as.numeric(xml_attr(trkpts, "lon"))
)

###### Find Mysti Outages ------------------------------------------------------
Mysti_Outages = RAW %>%
  select(TrkTime..EDT., TrkLatitude, TrkLongitude) %>%
  rename(Time.EDT = TrkTime..EDT., Lat = TrkLatitude, Long = TrkLongitude) %>%
  mutate(Time.EDT = format(as.POSIXct(Time.EDT, format = "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%S")) %>%
  mutate(
    Time_prev = lag(Time.EDT),
    Gap.Sec   = as.numeric(difftime(as.POSIXct(Time.EDT, format = "%H:%M:%S"),
                                    as.POSIXct(Time_prev, format = "%H:%M:%S"),
                                    units = "secs")),
    Gap.Min   = round(Gap.Sec / 60, 2)
  )

#### Flag time gaps
Mysti_time_gaps = Mysti_Outages %>%
  filter(Gap.Sec > Mysti_acc_seconds) %>%
  mutate(
    Gap.Min.Clean = ifelse(Gap.Min == round(Gap.Min),
                           sprintf("%dmin", round(Gap.Min)),
                           sprintf("%.1f min", Gap.Min)),
    range = paste0(Time_prev, "-", Time.EDT, " (", Gap.Min.Clean, ")")
  ) %>%
  pull(range)

#### Flag stretches of frozen lat/long
Mysti_latlong_rle    = rle(paste(Mysti_Outages$Lat, Mysti_Outages$Long))
Mysti_run_ends       = cumsum(Mysti_latlong_rle$lengths)
Mysti_run_starts     = Mysti_run_ends - Mysti_latlong_rle$lengths + 1
Mysti_repeat_runs    = which(Mysti_latlong_rle$lengths >= 3)

Mysti_latlong_ranges = sapply(Mysti_repeat_runs, function(i) {
  start_row  = Mysti_run_starts[i]
  end_row    = Mysti_run_ends[i]
  start_time = Mysti_Outages$Time.EDT[start_row]
  end_time   = Mysti_Outages$Time.EDT[end_row]
  paste0(start_time, "-", end_time,
         " (lat/long repeated for ", Mysti_latlong_rle$lengths[i], " rows)")
})

#### Combine both types of outages
Mysti_outage_ranges = sort_ranges(as.character(c(Mysti_time_gaps, Mysti_latlong_ranges)))
if (length(Mysti_outage_ranges) == 0) Mysti_outage_ranges = "None!"

###### Find GETAC time and GPS outages -----------------------------------------
GETAC_Outages = GETAC %>%
  mutate(Time.EDT = format(as.POSIXct(Time.EDT, format = "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%S")) %>%
  mutate(
    Time_prev = lag(Time.EDT),
    Gap.Sec   = as.numeric(difftime(as.POSIXct(Time.EDT, format = "%H:%M:%S"),
                                    as.POSIXct(Time_prev, format = "%H:%M:%S"),
                                    units = "secs")),
    Gap.Min   = round(Gap.Sec / 60, 2)
  )

#### Flag time gaps
GETAC_time_gaps = GETAC_Outages %>%
  filter(Gap.Sec > GETAC_acc_seconds) %>%
  mutate(
    range = paste0(Time_prev, "-", Time.EDT, " (", Gap.Sec, " sec)")
  ) %>%
  pull(range)

#### Flag stretches of frozen lat/long
GETAC_latlong_rle    = rle(paste(GETAC_Outages$Lat, GETAC_Outages$Long))
GETAC_run_ends       = cumsum(GETAC_latlong_rle$lengths)
GETAC_run_starts     = GETAC_run_ends - GETAC_latlong_rle$lengths + 1
GETAC_repeat_runs    = which(GETAC_latlong_rle$lengths >= 3)

GETAC_latlong_ranges = sapply(GETAC_repeat_runs, function(i) {
  start_row  = GETAC_run_starts[i]
  end_row    = GETAC_run_ends[i]
  start_time = GETAC_Outages$Time.EDT[start_row]
  end_time   = GETAC_Outages$Time.EDT[end_row]
  paste0(start_time, "-", end_time,
         " (lat/long repeated for ", GETAC_latlong_rle$lengths[i], " rows)")
})

#### Combine both types of outages
GETAC_outage_ranges = sort_ranges(as.character(c(GETAC_time_gaps, GETAC_latlong_ranges)))
if (length(GETAC_outage_ranges) == 0) GETAC_outage_ranges = "None!"

###### Print Results -----------------------------------------------------------
message(paste("Outages for", m.date))
cat("Mysti Outages:\n")
cat(Mysti_outage_ranges, sep = "\n")
cat("\nGETAC Outages:\n")
cat(GETAC_outage_ranges, sep = "\n")
