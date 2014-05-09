makePlot4 <- function()
{
  dat <- read.table("household_power_consumption.txt",
                    stringsAsFactors = FALSE,
                    colClasses = c("character", "character", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric"),
                    sep = ";",
                    header = TRUE,
                    na.strings = "?")
  
  dat <- subset( dat, Date %in% c("1/2/2007", "2/2/2007") )
  my_datetime <- strptime(sprintf( "%s %s", dat$Date, dat$Time), format = "%d/%m/%Y %H:%M:%S")
  
  plot_h <- max(c(max(dat$Sub_metering_1), max(dat$Sub_metering_2), max(dat$Sub_metering_3)))
  min_dt <- min(my_datetime)
  days_ct <- as.POSIXct(c(min_dt, min_dt + 86400, min_dt + 2*86400))
  days_lbl <- format( days_ct, "%a")
  
  png( "plot4.png", width = 480, height = 480 )
  
  par( bg = "transparent", mfcol = c(2,2) )
  
  ##plot1
  plot( my_datetime, dat$Global_active_power,
        col = "black", fg = "black",
        type = "l",
        ylab = "Global Active Power",
        xlab = "")
  
  ##plot2
  plot.new()
  plot.window( ylim = c( 0, plot_h), xlim = as.POSIXct(c( min_dt, max(my_datetime))))
  axis(1, at = days_ct, labels = days_lbl)
  axis(2)
  box( col = "black" )
  title( ylab = "Energy sub metering" )
  legend( "topright",
          legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
          col = c("black", "red", "blue"), lty = "solid", text.col = "black" )
  lines( my_datetime, dat$Sub_metering_1, col = "black" )
  lines( my_datetime, dat$Sub_metering_2, col = "red" )
  lines( my_datetime, dat$Sub_metering_3, col = "blue" )
  
  ##plot3
  plot( my_datetime, dat$Voltage,
        col = "black", fg = "black",
        type = "l",
        ylab = "Voltage",
        xlab = "datetime")
  
  ##plot4
  plot( my_datetime, dat$Global_reactive_power,
        col = "black", fg = "black",
        type = "l",
        ylab = "Global_reactive_power",
        xlab = "datetime")
  
  dev.off()
}