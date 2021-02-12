######################################################
#                                                    #
# WORKING WITH DATETIME DATA IN R                    #
#                                                    #
######################################################
# Biodiversity Exploratories Synthesis R helpdesk
# Noëlle Schenk 12.02.2021
#
# this file contains examples and exercises for
#TODO
# RESOURCES
# lubridate cheatsheet https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf
# time formats : https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

library(lubridate)

# your function : as.POSIXct(), NOT POSIXlt (lt uses much more storage.)


##
# EASY EXAMPLES
#
curr_time <- Sys.time()
class(curr_time)
unclass(curr_time)                    # a large integer
floor(unclass(curr_time)/86400)       # the number of days since 1970-01-01 (UTC)
now <- as.POSIXlt(Sys.time())         # the current datetime, as class "POSIXlt"
# using R base : tricky
now$year
now$year + 1900
months(now)
weekdays(now)

# easier : using lubridate
year(now)
today()
now()
# easier : use POSICct
curr_time <- as.POSIXct(curr_time)
year(curr_time)
wday(curr_time)
hour(curr_time)



###
# CONVERT STRING TO TIME
#
a <- c("2017-01-31")
as.POSIXct(a, format = "%Y-%m-%d") # describe the format of the string to be converted

a <- c("19.07.2021")
as.POSIXct(a, format = "%d.%m.%Y")

# cheatsheet to look up : https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html 


###
# TIME ZONES
# 
# germany : TODO
# switzerland : tz = "Europe/Zurich"

# check in system time, which time zone is assigned by default :
Sys.time()
# CET : central european time
Sys.timezone() # Europe/Zurich




###
# READ IN DATA AND WORK WITH IT
#
# recommendation : use relative time as soon as possible 

# strptime() usage detected and wrapped with as.POSIXct(). 
# This is to minimize the chance of assigning POSIXlt columns, 
# which use 40+ bytes to store one date (versus 8 for POSIXct). 
# Use as.POSIXct() (which will call strptime() as needed internally) to avoid this warning.


## dates to date format
# DO YOU HAVE DIFFERENT FORMATS?
# 4 and 5 contain "no" values which are converted to NA (no other NA values)
dys[gastric_tube_start_x == "no", gastric_tube_start_x := NA]
dys[gastric_tube_end_x == "no", gastric_tube_end_x := NA]
# 4, 5, 13 and 14 are in different time format. 
dys[, gastric_tube_start_x := as.POSIXct(dys[, gastric_tube_start_x], format = "%d.%m.%Y %H:%M")]
dys[, gastric_tube_end_x := as.POSIXct(dys[, gastric_tube_end_x], format = "%d.%m.%Y %H:%M")]
dys[, intermittend_rrt_end := as.POSIXct(dys[, intermittend_rrt_end], format = "%d.%m.%Y %H:%M")]
dys[, intermittend_rrt_start := as.POSIXct(dys[, intermittend_rrt_start], format = "%d.%m.%Y %H:%M")]
# IF ALL TIME FORMATS ARE THE SAME
# all other date colums are in standard format
dys[,(dates) := lapply(.SD, as.POSIXct), .SDcols=dates]


test[, DateTime := lapply(.SD, function(x) as.POSIXct(x, format = '%Y-%m-%d  %H:%M:%S', tz = "Europe/Zurich")), .SDcols = "DateTime"]



###
# CALCULATE DIFFERENCES IN TIME
#
# calculate diff of 2 columns (posixct)
extr[, timetest := as.numeric(difftime(ValueEnterTime, ValueTime, units = "hours"))]

# Calculate a difference in time on multiple columns simultaneously
dates <- c("Insel Adm.",  "Study Start",  "Activ Study End", "Start Vasoactiva OR", "ICU Discharge","Insel Discharge", "Intubation Date", "Extubation Date", "IMP Start", "IMP Stop", "D30", "D90", "date of IMC discharge")
backup <- data.table::copy(heracles_baseline)
# heracles_baseline <- data.table::copy(backup)
for(d in dates){
  heracles_baseline[, (d) := lapply(.SD, function(x) difftime(x, `ICU Adm.`, units = "days")), .SDcols = d]
}