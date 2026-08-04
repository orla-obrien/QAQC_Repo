###### USER SET UP -------------------------------------------------------------
setwd("C:/R_work_directory/Double Check")

################################################################################
#######------------------ CODE WILL TAKE CARE OF THE REST ----------------------
################################################################################

###### Libraries and clear environment -----------------------------------------
library(dplyr)
library(stringr)
rm(list = ls()); cat("\014")

###### Load most recent URI ----------------------------------------------------
filenames = list.files(pattern = "*URI.csv")
date = max(substr(filenames, 1, 8))
dat = read.csv(paste0(date, "_URI.csv"))
dat[is.na(dat)] = ""
rm(filenames)

###### Function to find and return events with errors --------------------------
find_errors = function(error_events, good_value) {
  ifelse(length(error_events) == 0, good_value, paste("ERROR EVENT", collapse_events(error_events)))
}

###### Function to collapse consecutive eventnos into ranges --------------------
collapse_events = function(events) {
  events = sort(unique(as.numeric(events)))
  if (length(events) == 0) return("")
  breaks = c(0, which(diff(events) != 1), length(events))
  groups = lapply(seq_along(breaks[-1]), function(i) events[(breaks[i] + 1):breaks[i + 1]])
  parts = sapply(groups, function(g) if (length(g) == 1) as.character(g) else paste0(g[1], "-", g[length(g)]))
  paste(parts, collapse = ", ")
}

###### A.DATE ------------------------------------------------------------------
dat$date = paste0(dat$year, sprintf("%02d", as.integer(dat$month)), sprintf("%02d", as.integer(dat$day)))
A.DATE = find_errors(dat$eventno[dat$date != date | is.na(dat$date)], date)
rm(date); dat$date = NULL

###### B.BLOCK -----------------------------------------------------------------
B.BLOCK = find_errors(dat$eventno[is.na(dat$block) | trimws(dat$block) == ""],
                      paste(unique(dat$block), collapse = " "))
B.BLOCK = ifelse(length(dat$block)==0, "NO BLOCK COLUMN", B.BLOCK)

###### C.LEGNO -----------------------------------------------------------------
## Check for blanks in legno when legtype is 2 or 4 (and legno filled in when it shouldn't be)
blank.events = dat$eventno[(dat$legtype %in% c("2", "4") & (is.na(dat$legno) | trimws(dat$legno) == "")) |
                             (!(dat$legtype %in% c("2", "4")) & trimws(dat$legno) != "")]

## Check that legno never changes mid-line
midline.events = c()
for (i in 2:nrow(dat)) {
  prev = dat$legno[i - 1]
  curr = dat$legno[i]
  if (!is.na(curr) & trimws(curr) != "" &
      !is.na(prev) & trimws(prev) != "" &
      curr != prev) {midline.events = c(midline.events, dat$eventno[i])}
}

error_events = unique(c(blank.events, midline.events))

C.LEGNO = ifelse(length(error_events) == 0, paste(unique(dat$legno[!is.na(dat$legno) & trimws(dat$legno) != ""]), collapse = " "), 
                 paste("ERROR EVENT", collapse_events(error_events)))
rm(error_events, blank.events, midline.events, i, prev, curr)

###### D/E. ON/OFF WATCH & F/G. LUNCH ------------------------------------------
### Function to check events
check_event = function(row, note.1, note.2) {
  if (dat$rectype[row] != "S") return("ERROR RECTYPE")
  if (is.na(dat$sightno[row]) | trimws(dat$sightno[row]) == "") return("ERROR SIGHTNO")
  if (note.1 != "" & !grepl(note.1, dat$notes[row])) return("ERROR NOTES")
  if (note.2 != "" & !grepl(note.2, dat$notes[row])) return("ERROR NOTES")
  "GOOD"
}

### D/E. ON/OFF WATCH 
D.FIRST.EVENT = check_event(1, "ON WATCH", "BEGIN SURVEY")
E.LAST.EVENT  = check_event(nrow(dat), "OFF WATCH", "END SURVEY")
if (dat$legtype[1] == 0){D.FIRST.EVENT = "OFF WATCH SIGHTING, CHECK MANUALLY"}
if (dat$legtype[nrow(dat)] == 0){E.LAST.EVENT = "OFF WATCH SIGHTING, CHECK MANUALLY"}

### F/G. LUNCH 
lunch.candidates = which(dat$legtype == 0 & seq_len(nrow(dat)) != 1 & seq_len(nrow(dat)) != nrow(dat))
if (length(lunch.candidates) == 0) {
  F.LUNCH = "NO LUNCH"; G.AFTER.LUNCH = "NA"
} else if (length(lunch.candidates) != 1) {
  F.LUNCH = "OFF WATCH SIGHTING, CHECK MANUALLY"; G.AFTER.LUNCH = "OFF WATCH SIGHTING, CHECK MANUALLY"
} else {
  F.LUNCH = check_event(lunch.candidates, "OFF WATCH", "")
  G.AFTER.LUNCH = check_event(lunch.candidates + 1, "ON WATCH", "RESUME SURVEY")
}
rm(lunch.candidates)

###### H-K. LINE CHANGES -------------------------------------------------------
check_line = function(lgtyp, lgstg, nts) {
  lc = dat[dat$legtype == lgtyp & dat$legstage == lgstg, ]
  errors = c()
  if (!all(lc$rectype == "S")) errors = c(errors, paste("ERROR RECTYPE EVENT", collapse_events(lc$eventno[lc$rectype != "S"])))
  if (!all(trimws(lc$sightno) != "")) errors = c(errors, paste("ERROR SIGHTNO EVENT", collapse_events(lc$eventno[trimws(lc$sightno) == ""])))
  if (!all(grepl(nts, lc$notes))) errors = c(errors, paste("ERROR NOTES EVENT", collapse_events(lc$eventno[!grepl(nts, lc$notes)])))
  if (length(errors) == 0) "GOOD" else paste(errors, collapse = " | ")
}

H.BEGIN.LINE  = check_line(2, 1, "BEGIN LINE")
I.END.LINE    = check_line(2, 5, "END LINE")
J.BREAK.LINE  = check_line(2, 3, "999")
K.RESUME.LINE = check_line(2, 4, "RESUME")

# Confirm 999 events don't reference their own eventno
for (i in 1:nrow(dat)) {
  if (dat$legstage[i] == "3" && grepl(paste0("\\b", dat$eventno[i], "\\b"), dat$notes[i])){
    if(J.BREAK.LINE == "GOOD"){J.BREAK.LINE = paste("WRONG EVENT IN NOTES. EVENTNO:", dat$eventno[i])} else {
      J.BREAK.LINE = paste(J.BREAK.LINE,"WRONG EVENT IN NOTES. EVENTNO:", dat$eventno[i])
    }
  }
}
rm(i)

###### L. LEGTYPE/LEGSTAGE PROGRESSION -----------------------------------------
L.PROGRESSION = ""
approved_next = list(
  "0-" = c("1-"),
  "1-" = c("1-","17","21","0-"), "17" = c("1-","17","21","0-"),
  "21" = c("22","27"),
  "22" = c("22","23","25","27"), "27" = c("22","23","25","27"),
  "23" = "4-",
  "4-" = c("4-","47","24"), "47" = c("4-","47","24"),
  "24" = c("22","27"),
  "25" = c("1-","3-"),
  "3-" = c("21","3-","37"), "37" = c("21","3-","37"))

errors = c()

for (i in 1:(nrow(dat) - 1)) {
  ts1 = paste0(dat$legtype[i],   dat$legstage[i])
  ts2 = paste0(dat$legtype[i+1], dat$legstage[i+1])
  if (!ts2 %in% approved_next[[ts1]]) errors = c(errors, dat$eventno[i+1])
}
if (length(errors) == 0) {L.PROGRESSION = "GOOD"} else {
  L.PROGRESSION = paste("ERROR EVENT", collapse_events(errors))
}
rm(i, ts1, ts2, approved_next, errors)

###### M.ENVIRONMENTALS -------------------------------------------------------
M.ENVR = ""
allowed = list(visiblty = as.character(0:5),
               beaufort = as.character(0:6),
               cloud = as.character(1:4),
               wx = c("B","C","D","F","G","H","L","P","R","S","T"))
errors = c()
for (i in 1:nrow(dat)) {
  for (field in names(allowed)) {
    if (!dat[[field]][i] %in% allowed[[field]])
      errors = c(errors, dat$eventno[i])
  }
}
M.ENVR = find_errors(errors, "GOOD")
rm(i, field, allowed, errors)

###### N.GLARE/LEGNO ---------------------------------------------------------
N.GLARE.LEGNO = ""
err.glarel = c(); err.glarer = c(); err.legno = c()
for (i in 1:nrow(dat)) {
  lgtyp = dat$legtype[i]
  if (lgtyp %in% c("0","1","3")) {
    if (dat$glarel[i] != "") err.glarel = c(err.glarel, dat$eventno[i])
    if (dat$glarer[i] != "") err.glarer = c(err.glarer, dat$eventno[i])
    if (dat$legno[i]  != "") err.legno  = c(err.legno,  dat$eventno[i])
  } else if (lgtyp == "2") {
    if (!dat$glarel[i] %in% as.character(0:3)) err.glarel = c(err.glarel, dat$eventno[i])
    if (!dat$glarer[i] %in% as.character(0:3)) err.glarer = c(err.glarer, dat$eventno[i])
    if (dat$legno[i] == "")                    err.legno  = c(err.legno,  dat$eventno[i])
  } else if (lgtyp == "4") {
    if (dat$glarel[i] != "") err.glarel = c(err.glarel, dat$eventno[i])
    if (dat$glarer[i] != "") err.glarer = c(err.glarer, dat$eventno[i])
    if (dat$legno[i]  == "") err.legno  = c(err.legno,  dat$eventno[i])
  }
}
errors = c(
  if (length(err.glarel) > 0) paste("GLAREL EVENT", collapse_events(err.glarel)),
  if (length(err.glarer) > 0) paste("GLARER EVENT", collapse_events(err.glarer)),
  if (length(err.legno)  > 0) paste("LEGNO EVENT",  collapse_events(err.legno))
)
N.GLARE.LEGNO = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
rm(i, lgtyp, err.glarel, err.glarer, err.legno, errors)

###### O-R. ENV CHANGES --------------------------------------------------------
env_func = function(col) {
  errors = c()
  for (i in 2:length(col)) {
    if (col[i] != col[i-1]) {
      if (dat$sightno[i] == "")  errors = c(errors, dat$eventno[i])
      if (dat$rectype[i] != "S") errors = c(errors, dat$eventno[i])
    }
  }
  ifelse(length(errors) == 0, "GOOD", paste("ERROR EVENT", collapse_events(errors)))
}
O.VIS    = env_func(dat$visiblty)
P.BSS    = env_func(dat$beaufort)
Q.CLOUDS = env_func(dat$cloud)
R.WX     = env_func(dat$wx)

###### S/T. GLARE CHANGES ------------------------------------------------------
dat.2 = dat[dat$legtype == 2, ]
err.glarel = c(); err.glarer = c()
if (nrow(dat.2) > 0) {
  for (i in 2:nrow(dat.2)) {
    if (dat.2$glarel[i] != dat.2$glarel[i-1] && dat.2$sightno[i] == "") err.glarel = c(err.glarel, dat.2$eventno[i])
    if (dat.2$glarer[i] != dat.2$glarer[i-1] && dat.2$sightno[i] == "") err.glarer = c(err.glarer, dat.2$eventno[i])
  }
  S.GLAREL.SIGHTNO = find_errors(err.glarel, "GOOD")
  T.GLARER.SIGHTNO = find_errors(err.glarer, "GOOD")
  rm(i)
} else {
  S.GLAREL.SIGHTNO = "GOOD"
  T.GLARER.SIGHTNO = "GOOD"
}
rm(dat.2, err.glarel, err.glarer)

###### U. SIGHTINGS ------------------------------------------------------------
sightings  = dat[dat$speccode != "", ]

if (nrow(sightings) > 0) {
  err.legts  = c(); err.rectype = c(); err.sightno = c(); err.number = c()
  err.photos = c(); err.idrel   = c(); err.confidnc = c()
  
  err.speccode = dat$eventno[dat$speccode == "" & (dat$number != "" | dat$confidnc != "" | dat$idrel != "" | dat$photos != "")]
  
  for (i in 1:nrow(sightings)) {
    ts = paste0(sightings$legtype[i], sightings$legstage[i])
    if (ts %in% c("21","23","24","25"))                                        err.legts    = c(err.legts,    sightings$eventno[i])
    if (!sightings$rectype[i] %in% c("P","S") & sightings$legstage[i] != "7") err.rectype  = c(err.rectype,  sightings$eventno[i])
    if (sightings$sightno[i] == "")                                            err.sightno  = c(err.sightno,  sightings$eventno[i])
    if (sightings$number[i]  == "")                                            err.number   = c(err.number,   sightings$eventno[i])
    if (!sightings$photos[i]   %in% c("1","2"))                                err.photos   = c(err.photos,   sightings$eventno[i])
    if (!sightings$idrel[i]    %in% c("1","2","3","9"))                        err.idrel    = c(err.idrel,    sightings$eventno[i])
    if (!sightings$confidnc[i] %in% as.character(0:11))                        err.confidnc = c(err.confidnc, sightings$eventno[i])
  }
  errors = c(
    if (length(err.speccode) > 0) paste("MISSING SPECCODE EVENT", collapse_events(err.speccode)),
    if (length(err.legts)    > 0) paste("LEGTYPE/STAGE EVENT",    collapse_events(err.legts)),
    if (length(err.rectype)  > 0) paste("RECTYPE EVENT",          collapse_events(err.rectype)),
    if (length(err.sightno)  > 0) paste("SIGHTNO EVENT",          collapse_events(err.sightno)),
    if (length(err.number)   > 0) paste("NUMBER EVENT",           collapse_events(err.number)),
    if (length(err.photos)   > 0) paste("PHOTOS EVENT",           collapse_events(err.photos)),
    if (length(err.idrel)    > 0) paste("IDREL EVENT",            collapse_events(err.idrel)),
    if (length(err.confidnc) > 0) paste("CONFIDNC EVENT",         collapse_events(err.confidnc))
  )
  U.SIGHTINGS = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
  if ("0" %in% sightings$legtype) U.SIGHTINGS = paste0(U.SIGHTINGS, ". CHECK 0/- SIGHTINGS")
  rm(i, ts, err.legts, err.rectype, err.sightno, err.number, err.photos, err.idrel, err.confidnc, err.speccode, errors)
}
rm(sightings)

###### V. ANGLES ---------------------------------------------------------------
V.ANGLE =""
ANIMALS = c("BLWH","BOWH","FIWH","GRWH","HUWH","RIWH","SEWH","SPWH","UNFS","UNLW",
            "BEWH","GOBW", "MIWH","TRBW","UNBW","UNMW","UNWH",
            "KIWH","PIWH","ASDO","BODO","GRAM","HAPO","OBDO","RTDO","SADO","SPDO","STDO","UNDO","WBDO","WSDO","UNCW","UNGD",
            "GRSE","HASE","UNSE",
            "BASH","BLSH","DUSH","GHSH","HHSH","UNSH","WHSH","WTSH",
            "BFTU","CDRA","MAHI","MARA","MOBU","OCSU","SCFI","TUNS","UNFI",
            "GRTU","LETU","LOTU","ORTU","RITU","UNTU",
            "JELL","UNID","ZOOP","UNMM")
err.side= c(); err.nospec= c(); err.on4=c(); err.nonote=c()
for (i in 1:nrow(dat)) {
  if (dat$anglel[i] != "" && dat$rectype[i] == "S")   err.side   = c(err.side,   dat$eventno[i])
  if (dat$angler[i] != "" && dat$rectype[i] == "P")   err.side   = c(err.side,   dat$eventno[i])
  if (dat$anglel[i] != "" && dat$speccode[i] == "")   err.nospec = c(err.nospec, dat$eventno[i])
  if (dat$angler[i] != "" && dat$speccode[i] == "")   err.nospec = c(err.nospec, dat$eventno[i])
  if (dat$anglel[i] != "" && dat$legtype[i] == "4")   err.on4    = c(err.on4,    dat$eventno[i])
  if (dat$angler[i] != "" && dat$legtype[i] == "4")   err.on4    = c(err.on4,    dat$eventno[i])
  if (dat$speccode[i] %in% ANIMALS && dat$legtype[i] == "2" && dat$legstage[i] != "7")
    if (dat$anglel[i] == "" && dat$angler[i] == "" && !grepl("NO ANGLE", dat$notes[i]))
      err.nonote = c(err.nonote, dat$eventno[i])
}

errors = c(
  if (length(err.side)   > 0) paste("ANGLE WRONG SIDE EVENT",        collapse_events(err.side)),
  if (length(err.nospec) > 0) paste("ANGLE NO SPECCODE EVENT",       collapse_events(err.nospec)),
  if (length(err.on4)    > 0) paste("ANGLE ON 4- EVENT",             collapse_events(err.on4)),
  if (length(err.nonote) > 0) paste("MISSING NOTE FOR NO ANGLE EVENT",collapse_events(err.nonote))
)
V.ANGLE = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
rm(i, err.side, err.nospec, err.on4, err.nonote, errors)

###### W. CALVES / BEHAVIOR 40 -------------------------------------------------
b.cols = paste0("b", 1:15)
calves = dat[dat$numcalf != "", ]
has.40 = dat[apply(dat[, b.cols, drop = FALSE], 1, function(x) "40" %in% x), ]

errors.no40   = calves$eventno[!apply(calves[, b.cols, drop = FALSE], 1, function(x) "40" %in% x)]
errors.nocalf = has.40$eventno[has.40$numcalf == "" | is.na(has.40$numcalf)]

if (nrow(calves) == 0 & nrow(has.40) == 0) { W.CALVES.40 = "NO CALVES" } else {
  errors = c(if (length(errors.no40)   > 0) paste("MISSING B40 EVENT",   collapse_events(errors.no40)),
             if (length(errors.nocalf) > 0) paste("MISSING NUMCALF EVENT",collapse_events(errors.nocalf)))
  W.CALVES.40 = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
}
rm(b.cols, calves, has.40, errors.no40, errors.nocalf, errors)

###### X. CONFIDENCE -----------------------------------------------------------
sightings = dat[dat$speccode != "", ]

## Error if blank when speccode filled, or not in 0:11
if (nrow(sightings) == 0) { X.CONF = "GOOD" } else {
  conf.rules = data.frame(
    confidnc = as.character(0:11),
    conf     = c("EXACT","+/- 1","+/- 2","+/- 5","+/- 10","+/- 25","+/- 50","+/- 100","+/- 1000","AT LEAST","NO ESTIMATE","UNKNOWN"),
    min      = c(1,3,4,10,20,50,100,250,NA,NA,NA,NA),
    max      = c(5,7,10,25,50,125,250,500,NA,NA,NA,NA))
  err.confidnc = dat$eventno[(dat$speccode != "" & (is.na(dat$confidnc) | !dat$confidnc %in% as.character(0:11))) |
                               (dat$speccode == "" & !is.na(dat$confidnc) & dat$confidnc != "")]
  
  CONF.CHECK = data.frame(eventno  = sightings$eventno,  legtype  = sightings$legtype,
                          legstage = sightings$legstage, speccode = sightings$speccode,
                          number   = as.numeric(sightings$number),
                          confidnc = as.character(sightings$confidnc), notes = sightings$notes)
  CONF.CHECK = merge(CONF.CHECK, conf.rules, by = "confidnc")
  CONF.CHECK = CONF.CHECK[!((!is.na(CONF.CHECK$min)) & CONF.CHECK$number >= CONF.CHECK$min & CONF.CHECK$number <= CONF.CHECK$max), ]
  CONF.CHECK$confidnc = NULL
  CONF.CHECK = CONF.CHECK[order(CONF.CHECK$eventno), c("eventno","legtype","legstage","speccode","number","conf","notes")]
  rownames(CONF.CHECK) = NULL
  if (nrow(CONF.CHECK) > 0) { X.CONF = "REVIEW CONF.CHECK" } else { X.CONF = "GOOD"; rm(CONF.CHECK) }
  if (length(err.confidnc) > 0) X.CONF = paste("ERROR INVALID CONFIDNC EVENT", collapse_events(err.confidnc))
  rm(conf.rules, sightings, err.confidnc)
}

###### Y. BEHAVIORS ------------------------------------------------------------
behaviors = c(
  `0`="Dead in water", `1`="Dead, stranded", `2`="Dead, in fishing gear",
  `3`="Killed by whalers", `4`="Stranded alive and rescued", `5`="Visible injury",
  `6`="Fast swimming (> 10 knots)", `7`="Moderate swimming", `8`="Slow swimming (< 1 knot)",
  `9`="Obvious speed change", `10`="Apparently influenced by platform", `11`="Porpoising",
  `12`="Riding vessel bow wave", `13`="Breach", `14`="Acrobatics (dolphins)",
  `15`="Swimming upside down", `16`="Swimming on side", `17`="Swimming at surface",
  `18`="Swimming below surface", `19`="Flipper slapping", `20`="Lobtailing",
  `21`="Spyhopping", `22`="Logging", `23`="Dive, flukes not raised", `24`="Dive, flukes raised",
  `25`="Blow, mist visible", `26`="Blow, mist not visible", `27`="Unavailable (resp. int. rec.)",
  `28`="Dive intervals recorded", `29`="Unavailable (sync. dives)", `30`="Swimming in vessel wake",
  `34`="One directional swimming", `35`="Circular movement", `36`="Obvious direction change",
  `37`="Defecation", `38`="Close (< 1/2 mi.) to fishing gear", `40`="Mother with young",
  `41`="Calving", `42`="Nursing", `43`="Penis observed", `44`="Body contact",
  `45`="Riding whale bow wave", `50`="Associated with seaweed", `51`="Associated with other cetaceans",
  `52`="Associated with pinnipeds", `53`="Associated with birds", `54`="Apparent feeding",
  `55`="Feeding on fishery catch", `58`="Bubble observed", `59`="Associated with small fish",
  `60`="Associated with large fish", `61`="Associated with squid", `62`="Associated with jellyfish",
  `63`="Associated with visible zooplankton", `64`="Shark scavenging on carcass",
  `65`="Distinct subgroups", `67`="Belly-to-belly contact", `68`="Motionless below surface",
  `69`="Diving (turtles)", `70`="On beach nesting (turtles)", `71`="Fishing / trawling (fishing vessel)",
  `72`="Hauling / setting gear (fishing vessel)", `73`="Wind farm patrol / security vessel",
  `74`="Wind farm vessel pile driving", `76`="Hauled out on beach", `77`="Hauled out on rocks",
  `78`="Milling", `86`="Change in group heading", `87`="Change in group structure", `89`="Tagged",
  `90`="Surface active group (SAG - right whale)", `91`="Thrashing or violent behavior",
  `92`="Tangled in fishing gear", `93`="Abnormal behavior", `94`="Uncodeable behavior",
  `97`="Mud on animal", `98`="Struck by vessel")

anhead_map = c("0"="N","1"="NNE","2"="NE","3"="ENE","4"="E","5"="ESE","6"="SE","7"="SSE","8"="S",
               "9"="SSW","10"="SW","11"="WSW","12"="W","13"="WNW","14"="NW","15"="NNW","17"="VARIOUS",
               "21"="STATIONARY","22"="ANCHORED/STILTS")

b.cols    = paste0("b", 1:15)
keep.cols = c("eventno","legtype","legstage","speccode","number","numcalf","idrel","confidnc","anhead")
sight.b   = dat[, c(keep.cols, b.cols, "notes")]

if (all(sight.b[, b.cols, drop = FALSE] == "" | is.na(sight.b[, b.cols, drop = FALSE])) &&
    all(sight.b$anhead == "" | is.na(sight.b$anhead))) {
  Y.BEHAV = "NO BEHAVIORS"
} else {
  for (b in b.cols) sight.b[[b]] = behaviors[sight.b[[b]]]
  has.beh = apply(sight.b[, b.cols, drop = FALSE], 1, function(r) any(!(is.na(r) | r == ""))) |
    !(sight.b$anhead == "" | is.na(sight.b$anhead))
  BEHAV.CHECK = sight.b[has.beh, ]
  active.b = b.cols[sapply(BEHAV.CHECK[, b.cols, drop = FALSE], function(x) any(!(is.na(x) | x == "")))]
  BEHAV.CHECK = BEHAV.CHECK[, c(keep.cols, active.b, "notes")]
  BEHAV.CHECK$anhead = ifelse(BEHAV.CHECK$anhead %in% names(anhead_map),
                              unname(anhead_map[BEHAV.CHECK$anhead]),
                              ifelse(BEHAV.CHECK$anhead == "", "", "ERRONEOUS ENTRY"))
  BEHAV.CHECK[is.na(BEHAV.CHECK)] = ""
  rownames(BEHAV.CHECK) = NULL
  Y.BEHAV = "REVIEW BEHAV.CHECK"
  rm(b, has.beh, active.b)
}

###### YA/ZA. BEHAVIOR 34 & ANHEAD ---------------------------------------------
if (!exists("BEHAV.CHECK")) {
  YA.BEHAVIOR.34 = "GOOD"; ZA.ANHEAD = "GOOD"
} else {
  b34.rows = BEHAV.CHECK[apply(BEHAV.CHECK, 1, function(x) any(x == "One directional swimming", na.rm = TRUE)), ]
  missing  = b34.rows$eventno[b34.rows$anhead == "" | is.na(b34.rows$anhead)]
  YA.BEHAVIOR.34 = ifelse(length(missing) == 0, "GOOD",
                          paste("NO ANHEAD LISTED EVENT", collapse_events(missing)))
  rm(b34.rows, missing)
  
  anhead.rows = BEHAV.CHECK[BEHAV.CHECK$anhead != "" & !is.na(BEHAV.CHECK$anhead), ]
  if (nrow(anhead.rows) == 0) { ZA.ANHEAD = "GOOD" } else {
    cols    = intersect(b.cols, names(anhead.rows))
    invalid = anhead.rows$eventno[anhead.rows$anhead == "ERRONEOUS ENTRY"]
    missing = anhead.rows$eventno[!apply(anhead.rows[, cols, drop = FALSE], 1, function(x) any(x == "One directional swimming", na.rm = TRUE))]
    errors  = c(if (length(invalid) > 0) paste("INVALID ANHEAD EVENT", collapse_events(invalid)),
                if (length(missing) > 0) paste("MISSING 34 EVENT",     collapse_events(missing)))
    ZA.ANHEAD = ifelse(length(errors) == 0, "REVIEW BEHAV.CHECK",
                       paste("ERROR", paste(errors, collapse = " | "), "| REVIEW BEHAV.CHECK"))
    rm(cols, invalid, missing, errors)
  }
  rm(anhead.rows)
}
rm(b.cols, keep.cols, anhead_map, sight.b)

###### ZB. ANIMAL IDREL --------------------------------------------------------
animals = dat[dat$speccode %in% ANIMALS, ]
if (nrow(animals) == 0) { ZB.ANIMAL.IDREL = "GOOD" } else {
  errors = animals$eventno[!animals$idrel %in% c("1","2","3")]
  ZB.ANIMAL.IDREL = find_errors(errors, "GOOD")
  rm(errors)
}
rm(animals)

###### ZC. NOTES ---------------------------------------------------------------
notes.df = dat[dat$notes != "", ]
err.rectype = notes.df$eventno[!notes.df$rectype %in% c("P","S")]
err.sightno = notes.df$eventno[notes.df$sightno == ""]
err.long    = notes.df$eventno[nchar(notes.df$notes) > 100]
errors = c(if (length(err.rectype) > 0) paste("RECTYPE EVENT",  collapse_events(err.rectype)),
           if (length(err.sightno) > 0) paste("SIGHTNO EVENT",  collapse_events(err.sightno)),
           if (length(err.long)    > 0) paste("TOO LONG EVENT", collapse_events(err.long)))
ZC.NOTES = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
rm(err.rectype, err.sightno, err.long, errors)
rm(notes.df)

###### ZD. EDITS ---------------------------------------------------------------
edits.df = dat[dat$edits != "", ]
if (nrow(edits.df) == 0) { ZD.EDITS = "GOOD" } else {
  err.rectype = edits.df$eventno[!edits.df$rectype %in% c("P","S")]
  err.sightno = edits.df$eventno[edits.df$sightno == ""]
  err.long    = edits.df$eventno[nchar(edits.df$edits) > 100]
  errors = c(if (length(err.rectype) > 0) paste("RECTYPE EVENT",  collapse_events(err.rectype)),
             if (length(err.sightno) > 0) paste("SIGHTNO EVENT",  collapse_events(err.sightno)),
             if (length(err.long)    > 0) paste("TOO LONG EVENT", collapse_events(err.long)))
  ZD.EDITS = ifelse(length(errors) == 0, "REVIEW EDITS.CHECK", paste("ERROR", paste(errors, collapse = " | "), "| REVIEW EDITS.CHECK"))
  EDITS.CHECK = edits.df[, c("rectype","eventno","legtype","legstage","legno","visiblty","glarel","glarer","beaufort",
                             "cloud","wx","sightno","anglel","angler","speccode","number","numcalf","anhead","photos",
                             "idrel","confidnc","b1","b2","notes","edits")]
  rm(err.rectype, err.sightno, err.long, errors)
}
rm(edits.df)

###### ZE. SIGHTNO -------------------------------------------------------------
sightno.df = dat[dat$sightno != "", ]
sightno.df$sightno = as.numeric(sightno.df$sightno)
errors = c()

## Wrong rectype 
err.rectype = sightno.df$eventno[!sightno.df$rectype %in% c("P","S") & sightno.df$legstage != "7"]
if (length(err.rectype) > 0) errors = c(errors, paste("RECTYPE EVENT", collapse_events(err.rectype)))

## Not chronological 
not.chron = which(sightno.df$sightno[-nrow(sightno.df)] > sightno.df$sightno[-1] & sightno.df$legstage[-nrow(sightno.df)] != "7")
if (length(not.chron) > 0) errors = c(errors, paste("NOT CHRONOLOGICAL EVENT", collapse_events(sightno.df$eventno[not.chron + 1])))

## duplicate sightno 
if (any(duplicated(sightno.df$sightno))) errors = c(errors, "DUPLICATE SIGHTNO")

## No reason for sightno
no.reason = c()
for (i in 2:nrow(sightno.df)) {
  ts = paste0(sightno.df$legtype[i], sightno.df$legstage[i])
  if (!ts %in% c("0-","17","21","23","24","25","27","37","47") &&
      sightno.df$visiblty[i] == sightno.df$visiblty[i-1] &&
      sightno.df$beaufort[i] == sightno.df$beaufort[i-1] &&
      sightno.df$cloud[i]    == sightno.df$cloud[i-1]    &&
      sightno.df$wx[i]       == sightno.df$wx[i-1]       &&
      (sightno.df$legtype[i] != "2" || sightno.df$glarel[i] == sightno.df$glarel[i-1]) &&
      (sightno.df$legtype[i] != "2" || sightno.df$glarer[i] == sightno.df$glarer[i-1]) &&
      sightno.df$speccode[i] == "" && sightno.df$notes[i] == "" && sightno.df$edits[i] == "")
    no.reason = c(no.reason, sightno.df$eventno[i])
}
if (length(no.reason) > 0) errors = c(errors, paste("NO REASON FOR SIGHTNO EVENT", collapse_events(no.reason)))

ZE.SIGHTNO = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
rm(err.rectype, not.chron, no.reason, i, ts, errors, sightno.df)

###### ZF. EVENTNO -------------------------------------------------------------
errors = c()

## Duplicate eventno
dups = dat$eventno[duplicated(dat$eventno)]
if (length(dups) > 0) errors = c(errors, paste("DUPLICATE EVENTNO", collapse_events(dups)))

## Not chronological
not.chron = which(dat$eventno[-nrow(dat)] > dat$eventno[-1])
if (length(not.chron) > 0) errors = c(errors, paste("NOT CHRONOLOGICAL EVENT", collapse_events(dat$eventno[not.chron + 1])))

ZF.EVENTNO = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
rm(dups, not.chron, errors)

###### ZG/ZH. BLANK & UNUSUAL DATA --------------------------------------------
never.blank   = c("rectype","month","day","year","eventno","time","lat","long","heading",
                  "alt","legtype","legstage","visiblty","beaufort","cloud","wx","block",
                  "gpsspeed","setalt","setvel")
usually.blank = c("refno","stratum","utc","radalt","lensfl","ggf","int","gpsq",
                  "gpssats","roll","pitch","yaw","maghead","glarev","ph_qual")

nb.cols = intersect(never.blank,  names(dat))
ub.cols = intersect(usually.blank, names(dat))

blanks  = nb.cols[sapply(dat[nb.cols], function(x) any(is.na(x) | trimws(x) == ""))]
unusual = ub.cols[sapply(dat[ub.cols], function(x) any(!is.na(x) & trimws(x) != ""))]

ZG.BLANK.DATA   = ifelse(length(blanks)  == 0, "GOOD", paste("BLANKS IN COLUMNS:", paste(blanks,  collapse = ", ")))
ZH.UNUSUAL.DATA = ifelse(length(unusual) == 0, "GOOD", paste("DATA IN COLUMNS:",   paste(unusual, collapse = ", ")))
rm(never.blank, usually.blank, nb.cols, ub.cols, blanks, unusual)

###### ZI. SET ALT/VEL ---------------------------------------------------------
expected_alt = ifelse(grepl("M2", dat$block), 1500, 1000)
err.alt = dat$eventno[dat$setalt != expected_alt]
err.vel = dat$eventno[dat$setvel != 100]
errors  = c(if (length(err.alt) > 0) ifelse(length(err.alt) == nrow(dat), "WRONG SETALT ALL EVENTS", paste("WRONG SETALT EVENT", collapse_events(err.alt))),
            if (length(err.vel) > 0) ifelse(length(err.vel) == nrow(dat), "WRONG SETVEL ALL EVENTS", paste("WRONG SETVEL EVENT", collapse_events(err.vel))))
ZI.SET.ALT.VEL = ifelse(length(errors) == 0, "GOOD", paste("ERROR", paste(errors, collapse = " | ")))
rm(expected_alt, err.alt, err.vel, errors)

###### ZJ. SPELL SIGHTINGS -----------------------------------------------------
Non.Animal = if (as.numeric(dat$year[1]) < 2026) {
  c("CV-O","CV-C","CREW","MV-C","MV-L","MV-O","MV-T","MV-B","RV-L","FE-S","FE-H","FV-H",
    "FV-U","FV-C","FV-T","FG-U","SPFV","SV-S","SV-L","RECV","UNVE","MY-S","MY-L","RV-S")
} else { c() }

COMMON.ANIMALS = c(Non.Animal, setdiff(ANIMALS, c("BOWH","RTDO","SPDO","UNCW","UNGD","BEWH","UNMW","UNMM",
                                                  "UNWH","WHSH","WTSH","GRTU","LOTU","ORTU","RITU")))
Rare.Specs = dat[dat$speccode != "" & !dat$speccode %in% COMMON.ANIMALS, ]
ZJ.SPELL.SIGHTINGS = ifelse(nrow(Rare.Specs) == 0, "GOOD", paste(unique(Rare.Specs$speccode), collapse = ", "))
rm(Non.Animal, COMMON.ANIMALS, Rare.Specs)

###### ZK. SUGGESTED BEHAVS ----------------------------------------------------
behav.keywords = list(
  dead        = list(keywords = c("dead","carcass","strand","died","body"),                    behaviors = c("0","1","2","4","64","92")),
  injury      = list(keywords = c("injury","injured","scar","wound"),                          behaviors = c("5","92")),
  breach      = list(keywords = c("breach","jump"),                                            behaviors = c("13")),
  flipper     = list(keywords = c("flipper","pec","slapping","slap","lobtailing","kickfeeding"),behaviors = c("19","20")),
  logging     = list(keywords = c("log","sleep","rest"),                                       behaviors = c("22")),
  travel      = list(keywords = c("travel","fast","swimming","north","south","east","west","traveling"), behaviors = c("34","36")),
  poop        = list(keywords = c("poop"),                                                     behaviors = c("37")),
  gear        = list(keywords = c("gear","fishing","net"),                                     behaviors = c("38","92")),
  mom         = list(keywords = c("mom","calf","baby","nursing","nurse"),                      behaviors = c("40","42")),
  birds       = list(keywords = c("bird"),                                                     behaviors = c("53")),
  feeding     = list(keywords = c("feed","bubble","bubblenet","lob","kick","lunge","eat"),     behaviors = c("54","53")),
  bubble      = list(keywords = c("bubble"),                                                   behaviors = c("58")),
  sag         = list(keywords = c("sag"),                                                      behaviors = c("67","90")),
  entanglement= list(keywords = c("entanglement","net","gear","tied"),                         behaviors = c("92")),
  mating      = list(keywords = c("penis","sag","mate","mating","breed","breeding"),           behaviors = c("43","44","67","90")),
  bow         = list(keywords = c("bow","ride","riding"),                                      behaviors = c("45","12")),
  mud         = list(keywords = c("mud"),                                                      behaviors = c("97")),
  belly       = list(keywords = c("belly"),                                                    behaviors = c("67","90","15")),
  upside      = list(keywords = c("upside","upside-down","upsidedown"),                        behaviors = c("15"))
  )

check_row = function(notes, behaviors_row, speccode, number) {
  notes         = tolower(as.character(notes))
  behaviors_row = as.character(behaviors_row)
  speccode      = as.character(speccode)
  if (is.na(speccode) || speccode == "") return(list(flag = NA, suggest = NA, suggest.labels = NA))
  
  found_flags = c(); suggested_behaviors = c()
  
  for (rule_name in names(behav.keywords)) {
    rule = behav.keywords[[rule_name]]
    if (grepl(paste0("\\b(", paste(rule$keywords, collapse = "|"), ")\\b"), notes) &&
        !any(behaviors_row %in% rule$behaviors, na.rm = TRUE)) {
      found_flags          = c(found_flags, rule_name)
      suggested_behaviors  = c(suggested_behaviors, rule$behaviors)
    }
  }
  if (speccode %in% c("HUWH","FIWH","MIWH","RIWH","SEWH") && number > 5 && !("65" %in% behaviors_row)) {
    found_flags = c(found_flags, "subgroups");             suggested_behaviors = c(suggested_behaviors, "65")
  }
  if (speccode %in% c("UNSE","GRSE","HASE") && number > 2 && !any(behaviors_row %in% c("76","77"))) {
    found_flags = c(found_flags, "hauled_out");            suggested_behaviors = c(suggested_behaviors, c("76","77"))
  }
  if (any(behaviors_row %in% c("58","20"), na.rm = TRUE) && !("54" %in% behaviors_row)) {
    found_flags = c(found_flags, "feeding_from_behavior"); suggested_behaviors = c(suggested_behaviors, "54")
  }
  if (length(found_flags) == 0) return(list(flag = NA, suggest = NA, suggest.labels = NA))
  
  unique.behavs = sort(unique(suggested_behaviors))
  list(flag           = paste(unique(found_flags), collapse = "; "),
       suggest        = paste(unique.behavs, collapse = ", "),
       suggest.labels = paste(sapply(unique.behavs, function(x) paste0(x, " - ", behaviors[x])), collapse = ",\n"))
}

if (nrow(dat[dat$speccode != "", ]) == 0) { 
  ZK.SUGGESTED.BEHAVS = "GOOD" 
  rm(behav.keywords, sightings)} else {
    
  b.cols  = paste0("b", 1:15)
  results = lapply(1:nrow(dat), function(i)
    check_row(dat$notes[i], unlist(dat[i, b.cols]), dat$speccode[i], dat$number[i]))
  dat.keywords                     = dat
  dat.keywords$keyword_flag        = sapply(results, function(x) x$flag)
  dat.keywords$suggested_behaviors = sapply(results, function(x) x$suggest.labels)
  
  BEHAV.SUGGEST = dat.keywords[!is.na(dat.keywords$keyword_flag),
                               c("eventno","speccode","number","numcalf","anhead","photos","idrel","confidnc",
                                 b.cols, "notes","edits","suggested_behaviors")]
  empty.b       = b.cols[sapply(BEHAV.SUGGEST[, b.cols, drop = FALSE], function(x) all(is.na(x) | x == ""))]
  BEHAV.SUGGEST = BEHAV.SUGGEST[, !names(BEHAV.SUGGEST) %in% empty.b]
  rownames(BEHAV.SUGGEST) = NULL
  
  ZK.SUGGESTED.BEHAVS = ifelse(nrow(BEHAV.SUGGEST) == 0, "GOOD", "REVIEW BEHAV.SUGGEST")
  rm(results, dat.keywords, empty.b, behav.keywords, b.cols)
  if(nrow(BEHAV.SUGGEST) == 0){rm(BEHAV.SUGGEST)}
}
rm(behaviors, ANIMALS)

##### Altitude, Speed, & Heading -----------------------------------------------
### Read in Survey_Headings.csv
Headings.Ref = read.csv("Survey_Headings.csv")

### Blocks with no lat/long reference table - their two candidate windows are fixed
Fixed.Heading.Blocks = list(
  M2 = list(c(170, 190), c(350, 10)),
  CM = list(c(144, 164), c(324, 344)))

## Flag blocks with unknown headings
heading.known = (B.BLOCK %in% Headings.Ref$block) || (B.BLOCK %in% names(Fixed.Heading.Blocks))
if (!heading.known) {heading.text = paste0("No saved headings for block '", B.BLOCK, "' - check manually")}

### Subset on-line events, make lat/long numeric
on.line = subset(dat, dat$legtype == "2")
on.line$lat = as.numeric(on.line$lat)
on.line$long = as.numeric(on.line$long)

### For survey with no lines:
if (nrow(on.line) == 0) {
  alt.text = "NO ON LINE EVENTS"
  speed.text = "NO ON LINE EVENTS"
  heading.text = "NO ON LINE EVENTS"
  
} else { ### For surveys with lines:
  
  ## Function to collapse consecutive numbers into ranges
  collapse_ranges = function(x) {
    x = sort(unique(x))
    if (length(x) == 0) return("")
    r = split(x, cumsum(c(1, diff(x) != 1)))
    sapply(r, function(g) if (length(g) == 1) as.character(g) else paste0(min(g), "-", max(g))) |> paste(collapse = ", ")
  }
  
  ## Function that creates text for both the altitude and speed checks below
  range_check_text = function(in_range) {
    if (all(in_range)) return("all in range")
    bad = on.line[!in_range, ]
    tab = table(bad$legno)
    paste(
      sapply(names(tab), function(ln) {
        ev = bad$eventno[bad$legno == ln]
        paste0("Line ", ln, ": ", length(ev), " events (", collapse_ranges(ev), ") due to")
      }),
      collapse = "\n"
    )
  }
  
  ### Altitude check: valid range depends on block
  alt.numeric = as.numeric(on.line$alt)
  in_range = ifelse(on.line$block == "M2", alt.numeric >= 396 & alt.numeric <= 518, alt.numeric >= 244 & alt.numeric <= 365)
  alt.text = range_check_text(in_range)
  
  ### Speed check
  speed.numeric = as.numeric(on.line$gpsspeed)
  in_range = speed.numeric >= 80.0 & speed.numeric <= 120.0
  speed.text = range_check_text(in_range)
  
  ##### Heading check
  ## Only runs if heading.known is TRUE
  if (heading.known) {
    
    ## Function to handle ranges that wrap past 360/0 (e.g., low=354, high=14)
    in_heading_range = function(heading, low, high) {
      if (low <= high) heading >= low & heading <= high else heading >= low | heading <= high
    }
    
    ## Function to calculate initial compass bearing (0-360) from line start to line end
    bearing = function(lat1, long1, lat2, long2) {
      d_long = (long2 - long1) * pi / 180
      lat1r = lat1 * pi / 180; lat2r = lat2 * pi / 180
      y = sin(d_long) * cos(lat2r)
      x = cos(lat1r) * sin(lat2r) - sin(lat1r) * cos(lat2r) * cos(d_long)
      (atan2(y, x) * 180 / pi) %% 360
    }
    
    ## Function to calculate circular distance between two compass headings (handles wraparound)
    circ_dist = function(a, b) {
      d = abs(a - b) %% 360
      pmin(d, 360 - d)
    }
    
    ## Function to find midpoint heading of a window, handling wraparound (e.g., 350-10 -> 0)
    window_center = function(low, high) if (low <= high) (low + high) / 2 else ((low + high + 360) / 2) %% 360
    
    ## Check headings
    heading.errors = c()
    line.combos = unique(on.line[, c("block", "legno")])
    for (i in 1:nrow(line.combos)) {
      block.id = line.combos$block[i]
      ln = line.combos$legno[i]
      line.rows = on.line[on.line$block == block.id & on.line$legno == ln, ]
      
      ## Get line's two candidate windows, whichever source applies
      if (block.id %in% names(Fixed.Heading.Blocks)) {
        windows = Fixed.Heading.Blocks[[block.id]]
      } else {
        ref = Headings.Ref[Headings.Ref$block == block.id & Headings.Ref$line == as.numeric(ln), ]
        if (nrow(ref) == 0) {
          heading.errors = c(heading.errors,
                             paste0("Line ", ln, ": no saved heading - check manually"))
          next  # no reference found for this block/line - skip
        }
        windows = list(c(ref$range_fwd_low, ref$range_fwd_high), c(ref$range_rev_low, ref$range_rev_high))
      }
      
      ## Calculate bearing from the first to last recorded point on this line,
      ## then lock onto whichever window's center is closest to that actual bearing
      first.row = line.rows[1, ]
      last.row  = line.rows[nrow(line.rows), ]
      actual.bearing = bearing(first.row$lat, first.row$long, last.row$lat, last.row$long)
      centers = sapply(windows, function(w) window_center(w[1], w[2]))
      chosen = windows[[which.min(circ_dist(actual.bearing, centers))]]
      
      bad = line.rows[!in_heading_range(as.numeric(line.rows$heading), chosen[1], chosen[2]), ]
      
      if (nrow(bad) > 0) {
        heading.errors = c(heading.errors,
                           paste0("Line ", ln, ": ", nrow(bad), " events (", collapse_ranges(bad$eventno), ") due to"))
      }
    }
    heading.text = if (length(heading.errors) == 0) "all in range" else paste(heading.errors, collapse = "\n")
    
    rm(in_heading_range, bearing, circ_dist, window_center,
       heading.errors, line.combos, i, block.id, ln, line.rows, first.row, last.row,
       actual.bearing, centers, chosen, bad)
  }
  
  rm(collapse_ranges, range_check_text, alt.numeric, in_range, speed.numeric, heading.known, Headings.Ref,
     Fixed.Heading.Blocks)
}

###### Species Summary Table ----------------------------------------------------
Species.Lookup = data.frame(
  TYPE = c(
    "BLWH","BOWH","FIWH","GRWH","HUWH","RIWH","SEWH","SPWH","UNFS","UNLW", "MIWH", "UNMW","UNWH",
    "TRBW","UNBW", "BEWH", "GOBW",
    "KIWH", "PIWH","ASDO","BODO","GRAM","HAPO","OBDO","RTDO","SADO","SPDO","STDO","UNDO","WBDO","WSDO","UNCW","UNGD",
    "GRSE","HASE","UNSE",
    "BASH","BLSH","DUSH","GHSH","HHSH","UNSH","WHSH","WTSH",
    "BFTU","CDRA","MAHI","MARA","MOBU","OCSU","SCFI","TUNS","UNFI",
    "GRTU","LETU","LOTU","ORTU","RITU","UNTU",
    "JELL","UNID","ZOOP","UNMM"
  ),
  Species = c(
    "Blue whale","Bowhead whale","Fin whale","Gray whale","Humpback whale", "Right whale","Sei whale","Sperm whale",
    "Unidentified Fin or Sei Whale","Unidentified large whale", "Minke whale", "Unidentified medium whale","Unidentified whale",
    "True's beaked whale","Unidentified beaked whale", "Beaked whale", "Cuvier's beaked whale",
    "Killer whale","Pilot whale", "Atlantic spotted dolphin","Bottlenose dolphin","Risso's dolphin","Harbor porpoise",
    "Offshore/common bottlenose dolphin","Rough-toothed dolphin","Common dolphin",
    "Spotted dolphin","Striped dolphin","Unidentified dolphin","White-beaked dolphin",
    "Atlantic white-sided dolphin","Unidentified Common or White-sided Dolphin","Unidentified gray dolphin",
    "Gray seal","Harbor seal","Unidentified seal",
    "Basking shark","Blue shark","Dusky shark","Great hammerhead shark",
    "Hammerhead shark","Unidentified shark","Whale shark","White shark",
    "Bluefin tuna","Chilean Devil Ray","Mahi mahi","Manta ray","Mobulid ray sp",
    "Ocean sunfish","School of fish","Unidentified tuna","Unidentified fish",
    "Green turtle","Leatherback turtle","Loggerhead turtle","Olive Ridley sea turtle",
    "Kemp's Ridley turtle","Unidentified turtle",
    "Jellyfish","Unidentified animal","Zooplankton patches","Unidentified marine mammal"
  ),
  Group = c(
    rep("Whale", 13), rep("Beaked whale", 4), rep("Dolphin/\nporpoise", 16),
    rep("Seal", 3), rep("Shark", 8), rep("Fish", 9), rep("Turtle", 6), rep("Other", 4)
  ),
  stringsAsFactors = FALSE
)

Non.PA.Sightings = dat %>% filter(legstage != "7") %>% filter(speccode != "")

if (nrow(Non.PA.Sightings) == 0) {
  species.summary.text = "No Animal Sightings"
} else {
  Non.PA.Sightings$number = as.numeric(Non.PA.Sightings$number)
  species.seen = unique(Non.PA.Sightings$speccode)
  leg_cols = unique(trimws(as.character(dat$legno)))
  leg_cols = leg_cols[leg_cols != ""]
  
  Species.Summary = data.frame(matrix(ncol = length(c("TYPE", "TOTAL", leg_cols, "Off.Effort")), nrow = length(species.seen)))
  colnames(Species.Summary) = c("TYPE", "TOTAL", leg_cols, "Off.Effort")
  Species.Summary$TYPE = species.seen
  
  for (s in 1:length(species.seen)) {
    species.sub = subset(Non.PA.Sightings, Non.PA.Sightings$speccode == species.seen[s])
    Species.Summary$TOTAL[s] = sum(species.sub$number)
    Species.Summary$Off.Effort[s] = sum(subset(species.sub, species.sub$legno == "")$number)
    if (length(leg_cols) > 0) {
      for (i in seq_along(leg_cols)) {
        Species.Summary[s, (i + 2)] = sum(subset(species.sub, species.sub$legno == leg_cols[i])$number)
      }
    }
  }
  
  ### Join in Species + Group, replace speccode with full species name, reorder columns
  Species.Summary = merge(Species.Summary, Species.Lookup, by = "TYPE", all.x = TRUE)
  Species.Summary$TYPE = ifelse(!is.na(Species.Summary$Species), Species.Summary$Species, Species.Summary$TYPE)
  Species.Summary = Species.Summary[, c("Group", setdiff(names(Species.Summary), "Group"))]
  Species.Summary$Species = NULL
  
  ### Sort by Group (matching Species.Lookup order), then by species name
  Species.Summary$Group = factor(Species.Summary$Group, levels = unique(Species.Lookup$Group))
  Species.Summary = Species.Summary[order(Species.Summary$Group, Species.Summary$TYPE), ]
  Species.Summary[Species.Summary == 0] = ""
  
  ### Export
  survey.date = paste0(dat$year[1], sprintf("%02d", as.integer(dat$month[1])), sprintf("%02d", as.integer(dat$day[1])))
  write.csv(Species.Summary, paste0("Species_Summary_", survey.date, ".csv"), row.names = FALSE)
  species.summary.text = "Species Summary Table has been exported"
  rm(survey.date)
}


###### END OF CODE -------------------------------------------------------------
message("Done! The code ran all the way through! Review below text for alt,speed,heading & species summary")
message("Altitude:\n", alt.text, "\n\n", "Speed:\n", speed.text, "\n\n", "Heading:\n", heading.text)
message(species.summary.text)
rm(list = c(intersect(c("on.line", "alt.text", "heading.text", "speed.text", "windows", "ref", "species.summary.text"), ls()), intersect(c("Non.PA.Sightings", "Species.Lookup", "species.seen", "leg_cols", "s", "i", "species.sub"), ls())))
