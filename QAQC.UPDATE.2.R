###### Set Working Directory
setwd("C:/R_work_directory/QAQC")

### ============================================================================
###### CODE WILL TAKE CARE OF THE REST
### ============================================================================

###### Load libraries 
library(tidyverse)

###### Read in file from output_DATA folder  -----------------------------------
filenames = list.files(".//output//", pattern="*URI.csv")
date = max(substr(filenames, 1, 8))
dat = read.csv(paste(".//output//", date, "_URI.csv", sep=""))

###### Make all text columns as.character --------------------------------------
dat$notes <- as.character(dat$notes)
dat$edits <- as.character(dat$edits)
dat$notes[is.na(dat$notes)] <- ""
dat$edits[is.na(dat$edits)] <- ""

###### Remove lunch 0- events --------------------------------------------------
dat = dat %>%
  filter(!(legtype == "0" & legstage == "-" & !rectype %in% c("P", "S")))

###### Fill in year, month, and day columns ------------------------------------
dat = dat %>%
  mutate(
    year  = as.numeric(substr(date, 1, 4)),
    month = as.numeric(substr(date, 5, 6)),
    day   = as.numeric(substr(date, 7, 8))
  )

###### Fill in block code ------------------------------------------------------
if (length(unique(dat$block)[unique(dat$block) != ""]) == 1) dat$block = unique(dat$block)[unique(dat$block) != ""]

###### Rectype & Notes for On/Off Watch & events -------------------------------
### Function
rect.not.edit = function(dat, rows, note, remove.edit = character(0)) {
  rows = rows[!is.na(rows)]
  dat$rectype[rows] = "S"
  already.has.note = grepl(note, dat$notes[rows], fixed = TRUE)
  dat$notes[rows] = ifelse(already.has.note, dat$notes[rows],
                           ifelse(dat$notes[rows] == "", note, paste(dat$notes[rows], note)))
  clear.values = c(note, remove.edit)
  dat$edits[rows][dat$edits[rows] %in% clear.values] = ""
  return(dat)
}

### Add legtype/legstage column (deleted later)
dat = dat %>% mutate(type.stage = paste0(legtype, legstage))

### On Watch / Off Watch (first and last "1-" events)
dat = rect.not.edit(dat, min(which(dat$legtype == "1")), "ON WATCH, BEGIN SURVEY",
                    remove.edit = "GOING ON WATCH")
dat = rect.not.edit(dat, max(which(dat$legtype == "1")), "OFF WATCH, END SURVEY",
                    remove.edit = "GOING ON WATCH")

### Lunch: stretches of 0/- sandwiched by 1/-
sandwiched = which(dat$type.stage == "0-" &
                     cumsum(dat$type.stage == "1-") > 0 &
                     rev(cumsum(rev(dat$type.stage == "1-"))) > 0)

if (length(sandwiched) > 0) {
  dat = rect.not.edit(dat, sandwiched[1], "OFF WATCH FOR REFUEL/LUNCH", remove.edit = "GOING OFF WATCH")
  
  resume = max(sandwiched) + 1
  if (resume <= nrow(dat) && dat$type.stage[resume] == "1-") {
    dat = rect.not.edit(dat, resume, "ON WATCH, RESUME SURVEY", remove.edit = "GOING ON WATCH")
  }
}

###### Rectype & Notes for line changes ----------------------------------------
## Other line changes:
line.notes = data.frame(
  stage  = c("1", "5", "3", "4"),
  note   = c("BEGIN LINE", "END LINE", "999 FOR", "RESUME LINE"),
  remove = c("", "", "", "RESUME TRACK"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(line.notes))) {
  rows = which(dat$legtype == "2" & dat$legstage == line.notes$stage[i])
  remove.edit = if (line.notes$remove[i] == "") character(0) else line.notes$remove[i]
  dat = rect.not.edit(dat, rows, line.notes$note[i], remove.edit = remove.edit)
}

### Any 2/5 followed by a 1/- gets ", GOING ON WATCH" appended
end.lines = which(dat$type.stage == "25")
end.lines = end.lines[end.lines + 1 <= nrow(dat) & dat$type.stage[end.lines + 1] == "1-"]
dat = rect.not.edit(dat, end.lines, ", GOING ON WATCH", remove.edit = "GOING ON WATCH")
dat$notes = gsub("END LINE , GOING ON WATCH", "END LINE, GOING ON WATCH", dat$notes) # removes space before ,

##### Remove unneeded "GOING ON WATCH" in edits --------------------------------
for(i in 2:nrow(dat)){
  if(!is.na(dat$edits[i]) & dat$edits[i] == "GOING ON WATCH" & grepl("GOING ON WATCH", dat$notes[i-1])){
    dat$edits[i] = ""
    dat$rectype[i] = "X"
  }
  if(!is.na(dat$edits[i]) & dat$edits[i] == "GOING OFF WATCH" & grepl("OFF WATCH", dat$notes[i-1])){
    dat$edits[i] = ""
    dat$rectype[i] = "X"
  }
}

## Remove added column
dat$type.stage = NULL

###### Clean up glare and legno ------------------------------------------------
dat = dat %>%
  mutate(
    glarel = ifelse(legtype != "2", "", glarel),
    glarer = ifelse(legtype != "2", "", glarer),
    legno  = ifelse(legtype %in% c("0", "1", "3"), "", legno)
  )

###### Flag changes in environmental conditions --------------------------------
env_vars = c(visiblty = "VIS", beaufort = "BSS", cloud = "CLOUD", wx = "WX", glarel = "GLARE", glarer = "GLARE")

for (i in 2:nrow(dat)) {
  changed    = c()
  type.stage = paste0(dat$legtype[i], dat$legstage[i])
  if (type.stage %in% c("21", "25", "23", "24")) next
  for (col in names(env_vars)) {
    if (col %in% c("glarel", "glarer") & dat$legtype[i] != "2") next
    curr = dat[[col]][i]
    prev = dat[[col]][i - 1]
    if (!is.na(curr) & !is.na(prev) & curr != prev) {
      changed = c(changed, env_vars[col])
    }
  }
  if (length(changed) > 0) {
    dat$rectype[i] = "S"
    new_flags = changed[!sapply(changed, function(x) grepl(x, dat$edits[i]))]
    if (length(new_flags) > 0) {
      dat$edits[i] = ifelse(dat$edits[i] == "",
                            paste(new_flags, collapse = " "),
                            paste(dat$edits[i], paste(new_flags, collapse = " ")))
    }
  }
}

###### Fill-in sightnos --------------------------------------------------------
dat = dat %>%
  select(-sightno) %>%
  mutate(sightno = ifelse(rectype %in% c("P", "S"),
                          cumsum(rectype %in% c("P", "S")),
                          "")) %>%
  relocate(sightno, .after = wx)

###### Export data file to Outputs folder --------------------------------------
## Make all NAs blank
dat[is.na(dat)] = ""
write.csv(dat, paste0("output/", date, "_URI.csv"), row.names = FALSE)
