makePlot2 <- function()
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
  
  png( "plot2.png", width = 480, height = 480 )
  par( bg = "transparent" )
  plot( my_datetime, dat$Global_active_power,
        col = "black", fg = "black",
        type = "l",
        ylab = "Global Active Power (kilowatts)",
        xlab = "")
  dev.off()
}