# TODO:
#.	Check smoothing splines
#.	Check rm.spikes parameters -- create adjustment weighting curve based on Z scores?

library('dplyr')
library('forecast')
library('xts')
library('lubridate')


departmentsToForecast <- c('Chat', 'Customer Care', 'Retentions', 'Sales', 'Commercial Support', 'Tech Support')

# Takes inContact report template call data, and returns forecasts for each department
# Always returns forecast in 15 minute intervals
# TODO : try dshw as replacement to stlf
createForecasts <- function(d, interval=30, period=48*7, rm.holidays=TRUE, rm.spikes=TRUE, dpt.plots=TRUE, tz='MST') {
    # Check that data is in 15 or 30 minute intervals
    if (!(interval %in% c(15,30))) error('Interval must be in 30 or 15 minute intervals, ya dangus')
    
    # Add Department column to categorize data
    if (is.null(d$Department)) {
        d <- departmentLookup(d)
    }
    
    # Data frame to store all the forecasts
    results <- data.frame(matrix(nrow=48*7*2, ncol=2))
    colnames(results) <- c('DOW', 'Interval')

    # Add column to show intervals and day of week, using first department's start/stop dates
    # - Always 15 minute intervals, since that is how fcast will be exported
    first.interval <- tail(index(nco.aht.interval(d[d$Department==departmentsToForecast[1], ], interval=interval)), 1) + minutes(interval)
    results$Interval <- seq.POSIXt(from=first.interval, by=paste(15,'mins'), length.out=48*7*2, tz=tz)
    results$DOW <- factor(weekdays(results$Interval, abbreviate=FALSE),
                          c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

    # since the intervals above are always 15-minute, create interval list that is 30 minutes (if interval arg=30)
    ints.matching.interval <- seq.POSIXt(from=first.interval, by=paste(interval,'mins'), length.out=period, tz=tz)
    
    # Iterate through each department
    for (dpt in departmentsToForecast) {
        # Filter for this department
        print(dpt)
        dpt.data <- d[d$Department == dpt,]
        
        # Aggregate data based on department & interval (combining skills and such)
        nco <- nco.aht.interval(dpt.data, interval=interval)
        
        if (rm.holidays) nco <- rm.junk.days(nco, dpt, period=period) # Remove holidays/outage days listed in "Ingore" CSV file
        if (rm.spikes) nco$Calls <- rm.spikes(nco$Calls, returnZscores=FALSE)
        
        # Create forecast and print some info about it 
        fcast.calls <- stlf(ts(as.integer(nco$Calls), f=period), h=period)
        print(paste(dpt, 'forecast', fcast.calls$model$method, 'parameters:'), quote=FALSE)
        print(coefficients(fcast.calls$model))
        if (dpt.plots) plot(1:(period), fcast.calls$mean, type='l', main=dpt)
        
        # Forecast AHT
        nco$AHT[is.na(nco$AHT) | is.infinite(nco$AHT)] <- 0   # Remove NA's and Infinities 
        fcast.AHT <- stlf(ts(as.integer(nco$AHT), f=period), h=period)
        
        #Smooth the forecasts to avoid extreme spikiness
        # *** To make forecasts more smooth, increase the "spar" parameter's value (between 0 and 1) and/or decrease nknots. ***
        # *** To make forecasts more spiky, decrease spar and/or increase nknots. ***
        fcast.calls <- as.double(smooth.within.HOOPs(xts(fcast.calls$mean, order.by=ints.matching.interval), dpt, nknots=15, spar=0.28))
        if (dpt.plots) lines(fcast.calls, col='blue')
        fcast.AHT <- as.double(smooth.within.HOOPs(xts(fcast.AHT$mean, order.by=ints.matching.interval), dpt, nknots=15, spar=0.45))

        # Remove negatives
        fcast.calls[fcast.calls < 0] <- 0
        fcast.AHT[fcast.AHT < 0] <- 0
        
        # If forecasting w/ 30 minute interval data, convert to 15 minute intervals then set intervals outside HOOPs to 0
        if (interval==30) {
            fcast.calls <- as.double(smooth.within.HOOPs(xts(convert.to.15.min(fcast.calls, divisor=2), order.by=results$Interval), dpt, smooth=FALSE))
            fcast.AHT <- as.double(smooth.within.HOOPs(xts(convert.to.15.min(fcast.AHT, divisor=1), order.by=results$Interval), dpt, smooth=FALSE))
        }
        
        # Assign forecast values to the department's columns
        results[,paste(dpt,'Calls')] <- fcast.calls
        results[,paste(dpt,'AHT')] <- fcast.AHT
    }

    # Sort by day of week, starting with Sunday
    print(results)
    results <- results[order(results$DOW), ]
    print(results)

    # Plot "Calls" columns and return results
    col.filter <- seq(3, ncol(results), by=2)
    matplot(results[ , col.filter], type='l', main='Call Forecast', lwd=1.5, ylim=c(0,60), sub=paste('rm.spikes=',deparse(substitute(rm.spikes))))
    #legend(x='topright', legend=colnames(results[ , col.filter]), col=1:length(col.filter), lty=1, lwd=1.5, pch=1, cex=0.7, ncol=2)
    
    return(results)
}


# Convert from 30 min to 15 min intervals
# d: array of data points on 30-minute scale
# divisor: number to divide data values by when converting -- should be 2 for calls, 1 for AHT (to avoid halving AHT)
convert.to.15.min <- function(d, divisor=2) {
    res <- 1:(length(d)*2) * 0
    res[1] <- d[1] / divisor
    for (i in 2:(length(d)-1)) {
        res[i*2] <- (d[i]) / divisor
        res[i*2+1] <- (d[i] + d[i+1]) / (2 * divisor)
    }
    res[(i+1)*2] <- d[i+1] / divisor
    return(res)
}

# Takes data set from inContact custom reporting and returns the same data set, with a column added to show "Department",
#   based on inContact CampaIgn Names
departmentLookup <- function(dataSet,  campaignLookup=NULL) {
    if (is.null(campaignLookup)) {
        campaignLookup <- read.csv('inContact Campaign to Department Map.csv', header=T)
    }
    merge(dataSet, campaignLookup, by='Campaign.Name')
}


# Remove holidays and other unwanted days specified in the "Forecast Dates to Ignore.csv" file.
# Unwanted days have their intervals replaced by the average of other intervals with the same weekday and time-of-day.
# nco: xts object with call data, indexed with dates and times
# department: string name of department
# period: number of intervals per seasonal period; defaults to 48*7*2 for weekly 15-minute intervals
rm.junk.days <- function(nco, department, period=48*7*2) {
    rm.data <- read.csv('Forecast Dates to Ignore.csv', header=T)
    
    # Convert to uppercase so "chat" is the same as "Chat", etc.
    department <- toupper(department)
    rm.data$Departments <- toupper(rm.data$Departments)
    rm.data$Date <- as.POSIXct(rm.data$Date, tz=tz(nco), format='%m/%d/%Y')
    
    # Create a list of dates which need to be removed
    rm.dates <- .POSIXct('')
    for (i in 1:nrow(rm.data)) {
        # Check if this department is included in this row
        if ((rm.data$Departments[i] == 'ALL') || (grepl(department, rm.data$Departments[i]))) {
            d <- as.POSIXct(rm.data$Date[i], tz=tz(nco))
            rm.dates <- append(rm.dates, d)
        }
    }
    
    # Indices where nco matches the date to exclude
    indices <- which(floor_date(index(nco), 'day') %in% rm.dates)
    for (i in indices) {
        # Determine intervals at same day-of-week and time, but not on a holiday/junk day
        matching.intervals <- seq.int(i %% period, nrow(nco), period)
        matching.intervals <- matching.intervals[!(matching.intervals %in% indices)]
        
        # Calls are average of calls in other intervals
        nco$Calls[i] <- mean(nco$Calls[matching.intervals])
        
        # AHT is weighted average of AHT with calls 
        nco$AHT[i] <- sum(nco$Calls[matching.intervals] * nco$AHT[matching.intervals])  /  sum(nco$Calls[matching.intervals])
    }
    
    return(nco)
}


# Removes outlier intervals based on Z scores
# Description of Z scores:  http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h.htm
rm.spikes <- function(nco, period=48*7, returnZscores=FALSE) {
    zs <- xts(1:nrow(nco) * 0, order.by=index(nco))
    
    for (i in 1:period) {
        matching.intervals <- seq.int(from=i %% period, to=nrow(nco), by=period)
        int.calls <- nco[matching.intervals]$Calls
        
        # Calculate Z scores: measure of how far each data point is away from the mean 
        int.calls$zscores <- (int.calls - mean(int.calls)) / sd(int.calls)
        
        # At points where Z score is high or low, set that point to the average for the interval (mean of points at that time/DOW)
        int.calls$Calls[(int.calls$zscores > 2.5) | (int.calls$zscores < -2.0)]  <-  mean(int.calls$Calls)
        
        nco[matching.intervals]$Calls <- int.calls$Calls
        zs[matching.intervals] <- int.calls$zscores
        
        #  break
    }
    if (returnZscores) {
        return(cbind(nco, zs))
    } else {
        return(nco)
    }
}

#  Smooths object, first filtering to hours of operation so that the 0-call intervals outside hours don't affect the smoothing
# d: xts object with interval-level data
# department: title of department to look up HOOPs (from list object called "HOOPs")
# ...: arguments to pass to smooth.spline()
# smooth: if smooth=FALSE, data will not be smoothed, but intervals outside HOOPs will be set to 0
smooth.within.HOOPs <- function(d, department, smooth=TRUE, ...) {
    tzone(d) <- HOOPs$timezone
    dpt.HOOPs <- HOOPs[[department]]
    
    # Blank xts to store results
    d.smoothed <- d
    d.smoothed[] <- 0
    
    for (DOW in c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')) {
        # Format to filter xts object: e.g., for Care on weekdays, 'T0600/T0959'
        filter.string <- paste('T', dpt.HOOPs[[DOW]][1], '/T', dpt.HOOPs[[DOW]][2], sep='')
        d.filtered <- d[weekdays(index(d))==DOW][filter.string]

        if (smooth) {
            d.smoothed[weekdays(index(d))==DOW][filter.string]  <-  smooth.spline(d.filtered, ...)$y
        } else {
            d.smoothed[weekdays(index(d))==DOW][filter.string]  <-  d.filtered
        }
    }
    
    return(d.smoothed)
}


# Hours Of OPerations
# Times should be in military format, up to the last minute that is open (e.g., 11:59 AM isntead of 12:00 AM for Tech)
HOOPs <-
    list(timezone='MST',
         'Customer Care'=list(Sunday=c('0700','1659'),
                              Monday=c('0600', '1959'),
                              Tuesday=c('0600', '1959'),
                              Wednesday=c('0600', '1959'),
                              Thursday=c('0600', '1959'),
                              Friday=c('0600', '1959'),
                              Saturday=c('0700', '1959')),
         'Retentions'=list(Sunday=c('0700','1659'),
                           Monday=c('0600', '1959'),
                           Tuesday=c('0600', '1959'),
                           Wednesday=c('0600', '1959'),
                           Thursday=c('0600', '1959'),
                           Friday=c('0600', '1959'),
                           Saturday=c('0700', '1659')),
         'Sales'=list(Sunday=c('0800','1659'),
                      Monday=c('0600', '1959'),
                      Tuesday=c('0600', '1959'),
                      Wednesday=c('0600', '1959'),
                      Thursday=c('0600', '1959'),
                      Friday=c('0600', '1959'),
                      Saturday=c('0700', '1559')),
         'Tech Support'=list(Sunday=c('0700','1959'),
                             Monday=c('0500','2359'),
                             Tuesday=c('0500','2359'),
                             Wednesday=c('0500','2359'),
                             Thursday=c('0500','2359'),
                             Friday=c('0500','2359'),
                             Saturday=c('0700','1959')),
         'Commercial Support'=list(Sunday=c('0700','1959'),
                                   Monday=c('0500','2359'),
                                   Tuesday=c('0500','2359'),
                                   Wednesday=c('0500','2359'),
                                   Thursday=c('0500','2359'),
                                   Friday=c('0500','2359'),
                                   Saturday=c('0700','1959')),
         'Chat'=list(Sunday=c('0700','1959'),
                     Monday=c('0700','2159'),
                     Tuesday=c('0700','2159'),
                     Wednesday=c('0700','2159'),
                     Thursday=c('0700','2159'),
                     Friday=c('0700','2159'),
                     Saturday=c('0700','1959')))

###########################################################################
# Utility functions to deal with data (mostly from inContact reporting)   #
###########################################################################

# Turn clipboard Excel data into data.frame
# To use, just copy a selection in Excel, then type "xcopy()" in the console
xcopy <- function(header=TRUE, sep='\t') {
    return( read.table(file='clipboard', sep=sep, header=header) )
}

# Copy vector to Excel-pastable clipboard
# For example, to copy a variable called "rdata", enter "xpaste(rdata)" in the console, then CTRL-V into Excel
xpaste <- function(x) {
    writeClipboard(as.character(x))
}


xts.to.dataframe <- function(x, by='week', period=48*7*2) {
    intervals <- seq.POSIXt(from=floor_date(head(index(x),1), unit=by),
                            to=floor_date(tail(index(x),1), unit=by),
                            by=paste(by,'s',sep=''))
    df <- data.frame(matrix(nrow=period, ncol=1+length(intervals)))
    colnames(df) <- c('Interval', as.character(intervals))
    
    for (i in intervals) {
        this.interval <- x[floor_date(index(x), unit=by) == i]
        
        df[1:nrow(this.interval), as.character(head(index(this.interval),1))] <- as.integer(this.interval[,1])
    }
    
    return(df)
}

review.weekly.volume <- function(d, date.range, period=48*7*2) {
    for (dpt in departmentsToForecast) {
        x <- nco.aht.int.15(d[d$Department==dpt,], dpt)
        plot(x$Calls[date.range], type='l', main=dpt)
    }
}



# Import inContact CSV data from multiple files, return as xts object 
# Assumes file names in format "[file.name then whatever].csv"
import <- function(file.name) {
    all <- 0
    for (f in dir(pattern=paste(file.name, '.*', sep=''))) {
        print(f)
        d <- read.csv(f, header=TRUE)
        
        # Data starts on 3rd row
        d <- d[2:nrow(d),]
        
        # Set up "all" data.frame during initial iteration
        if (class(all) != 'data.frame') {
            all <- d
        } else {
            all <- rbind(all, d)
        }
    }
    return(all)
}


# Takes a data.frame extracted from inContact data
# Returns an xts ordered by day, containing Inbound with all campaigns consolidated
nco.daily <- function(dataf) {
    dataf$Date <- as.POSIXct(dataf$Date, format='%m/%d/%Y')
    daily <- aggregate(Inbound ~ Date, dataf, FUN=sum)
    return(xts(x=daily$Inbound, order.by=daily$Date))
}

# Takes a data.frame extracted from inContact data
# Returns an xts ordered by month, containing Inbound with all campaigns consolidated
nco.monthly <- function(dataf) {
    dataf$Date <- as.POSIXct(dataf$Date, format='%m/%d/%Y')
    
    dataf$months <- strftime(dataf$Date, "%m")
    dataf$years  <- strftime(dataf$Date, "%Y")
    
    monthly  <- aggregate(Inbound ~ months + years, dataf, FUN=sum)
    return(monthly)
}




# Takes a data.frame extracted from inContact data
# Returns an xts ordered by 15 minute interval, containing Inbound and AHT with all campaigns consolidated
nco.aht.interval <- function(dataf, interval=15, tz='MST') {
    # Set interval column's name to Interval, instead of inContact's Interval.15.Minutes and Interval.30.Minutes
    colnames(dataf)[grep('Interval',colnames(dataf))] <- 'Interval'
    
    # Transform times from "HH:MM-HH:MM" strings to POSIX start times
    int.starts <- substr(dataf$Interval, 0, regexpr('-', dataf$Interval) - 1)
    dataf$Intervals <- as.POSIXct(paste(dataf$Date, dataf$Interval), format='%m/%d/%Y %H:%M', tz=tz)
    
    # Sort data oldest to newest
    dataf <- dataf[order(dataf$Intervals),]
    
    # Create range of intervals to ensure no gaps in final product
    # First, set intervals to extend from midnight to midnight
    first.interval <- floor_date(dataf$Intervals[1], 'day')
    last.interval  <- as.POSIXct(paste(format(tail(dataf$Intervals, 1), '%Y-%m-%d'), paste(23, 60-interval, sep=':')))
    ints <- seq.POSIXt(from=first.interval, to=last.interval, by=paste(interval,'mins'), tz=tz)
    int.df <- data.frame(Intervals=ints)
    
    # Re-create data.frame with all intervals (and put 0s for rows that were missing)
    dataf.with.all.intervals <- full_join(int.df, dataf, by='Intervals')
    dataf.with.all.intervals$Inbound[is.na(dataf.with.all.intervals$Inbound)] <- 0
    
    # Sum up each interval's inbound numbers, regardless of campaign, etc.
    int.data <- aggregate(. ~ Intervals, dataf.with.all.intervals, FUN=sum, na.action=na.pass, na.rm=TRUE)

    # Create result as xts object, then aggregate on 30 or 15 minute intervals
    res <- xts(cbind(Calls=int.data$Inbound, Handled=int.data$Inbound.Handled, Handle.Time=int.data$Inbound.Handle.Time),
               order.by=int.data$Intervals)
    res <- align.time.down(res, n=interval*60)
    res <- aggregate(. ~ index(res), res, FUN=sum)

    # The aggregate function turns "res" back into a data.frame, so it must be transformed into xts again while calculating AHT
    res <- xts(cbind(Calls=res$Calls, AHT=res$Handle.Time / res$Handled), order.by=res$`index(res)`)
    
    return(res)
}

# Aggregate xts into intervals
# x: xts object
# n: number of minutes per interval (default 30 minutes)
align.time.down <- function(x, n=60*30) {
    index(x) <- index(x) - n
    return(align.time(x,n))
}

# Scale x to fit within min and max of range (nice for plotting multiple axes w/o having to make multiple axes)
scale.range <- function(x, range) {
    return(x / (max(x,na.rm=T) / max(range,na.rm=T)))
}

# 
# # Takes a data.frame extracted from inContact data
# # Returns an xts ordered by 15 minute interval, containing Inbound and AHT with all campaigns consolidated
# nco.aht.int.15 <- function(dataf, tz='MST') {
#     # Transform times from "HH:MM-HH:MM" strings to POSIX start times
#     int.starts <- substr(dataf$Interval.15.Minutes, 0, regexpr('-', dataf$Interval.15.Minutes) - 1)
#     dataf$Intervals <- as.POSIXct(paste(dataf$Date, dataf$Interval.15.Minutes), format='%m/%d/%Y %H:%M', tz=tz)
#     
#     # Sort data oldest to newest
#     dataf <- dataf[order(dataf$Intervals),]
#     
#     # Create range of intervals to ensure no gaps in final product
#     # First, set intervals to extend from midnight to midnight
#     first.interval <- floor_date(dataf$Intervals[1], 'day')
#     last.interval  <- as.POSIXct(paste(format(tail(dataf$Intervals, 1), '%Y-%m-%d'), '23:45'))
#     ints <- seq.POSIXt(from=first.interval, to=last.interval, by='15 mins', tz=tz)
#     int.df <- data.frame(Intervals=ints)
#     
#     # Re-create data.frame with all intervals (and put 0s for rows that were missing)
#     dataf.with.all.intervals <- full_join(int.df, dataf, by='Intervals')
#     dataf.with.all.intervals$Inbound[is.na(dataf.with.all.intervals$Inbound)] <- 0
#     
#     # Sum up each interval's inbound numbers, regardless of campaign, etc.
#     int.data <- aggregate(. ~ Intervals, dataf.with.all.intervals, FUN=sum, na.action=na.pass, na.rm=TRUE)
#     int.data$AHT <- int.data$Inbound.Handle.Time / int.data$Inbound.Handled
#     
#     # Create result as xts object
#     res <- xts(x=int.data$Inbound, order.by=int.data$Intervals)
#     colnames(res) <- c('Calls')
#     
#     res$AHT <- int.data$AHT
#     return(res)
# }


# 
# # Takes a data.frame extracted from inContact data
# # Returns an xts ordered by 15 minute interval, containing Inbound with all campaigns consolidated
# nco.int.15 <- function(dataf, tz='MST') {
#     # Transform times from "HH:MM-HH:MM" strings to POSIX start times
#     int.starts <- substr(dataf$Interval.15.Minutes, 0, regexpr('-', dataf$Interval.15.Minutes) - 1)
#     dataf$Intervals <- as.POSIXct(paste(dataf$Date, dataf$Interval.15.Minutes), format='%m/%d/%Y %H:%M', tz=tz)
#     
#     # Sort data oldest to newest
#     dataf <- dataf[order(dataf$Intervals),]
#     
#     # Create range of intervals to ensure no gaps in final product
#     # First set intervals to extend from midnight to midnight
#     first.interval <- floor_date(dataf$Intervals[1], 'day')
#     last.interval  <- as.POSIXct(paste(format(tail(dataf$Intervals, 1), '%Y-%m-%d'), '23:45'))
#     ints <- seq.POSIXt(from=first.interval, to=last.interval, by='15 mins', tz=tz)
#     int.df <- data.frame(Intervals=ints)
#     
#     # Re-create data.frame with all intervals, then put 0s for rows that were missing
#     dataf.with.all.intervals <- full_join(int.df, dataf, by='Intervals')
#     dataf.with.all.intervals$Inbound[is.na(dataf.with.all.intervals$Inbound)] <- 0
#     
#     # Sum up each interval's inbound numbers, regardless of campaign, etc.
#     int.data <- aggregate(Inbound ~ Intervals, dataf.with.all.intervals, FUN=sum)
#     return(xts(x=int.data$Inbound, order.by=int.data$Intervals))
# }
# 
# 
# # Takes a data.frame extracted from inContact data
# # Returns an xts ordered by 30 minute interval, containing Inbound with all campaigns consolidated
# nco.int.30 <- function(dataf, tz='MST') {
#     # Transform times from "HH:MM-HH:MM" strings to POSIX start times
#     int.starts <- substr(dataf$Interval.30.Minutes, 0, regexpr('-', dataf$Interval.30.Minutes) - 1)
#     dataf$Intervals <- as.POSIXct(paste(dataf$Date, dataf$Interval.30.Minutes), format='%m/%d/%Y %H:%M', tz=tz)
#     
#     # Sort data oldest to newest
#     dataf <- dataf[order(dataf$Intervals),]
#     
#     # Create range of intervals to ensure no gaps in final product
#     # First set intervals to extend from midnight to midnight
#     first.interval <- floor_date(dataf$Intervals[1], 'day')
#     last.interval  <- as.POSIXct(paste(format(tail(dataf$Intervals, 1), '%Y-%m-%d'), '23:30'))
#     ints <- seq.POSIXt(from=first.interval, to=last.interval, by='30 mins', tz=tz)
#     int.df <- data.frame(Intervals=ints)
#     
#     # Re-create data.frame with all intervals (and put 0s for rows that were missing)
#     dataf.with.all.intervals <- full_join(int.df, dataf, by='Intervals')
#     dataf.with.all.intervals$Inbound[is.na(dataf.with.all.intervals$Inbound)] <- 0
#     
#     # Sum up each interval's inbound numbers, regardless of campaign, etc.
#     int.data <- aggregate(Inbound ~ Intervals, dataf.with.all.intervals, FUN=sum)
#     return(xts(x=int.data$Inbound, order.by=int.data$Intervals))
# }


