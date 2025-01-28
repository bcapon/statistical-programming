## create date-time data (format is actually default here)
d <- as.POSIXct(c("2016-06-28 21:30:00","2016-06-29 22:00:00",
                 "2016-06-30 22:30:00"),format="%Y-%m-%d %H:%M:%S")
julian(d,origin=as.Date("2016-01-01")) ## get day since Jan 1 2016

format(d,"%T") ## extract times as text

as.Date(d) ## get dates as class ’Date’
