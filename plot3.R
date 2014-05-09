makePlot3 <- function()
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
  
  png( "plot3.png", width = 480, height = 480 )
  par( bg = "transparent" )
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
  dev.off()
}