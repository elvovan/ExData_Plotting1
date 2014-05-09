makePlot1 <- function()
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
  
  png( "plot1.png", width = 480, height = 480 )
  par(bg = "transparent")
  hist(dat$Global_active_power, col = "red",
       main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  dev.off()
}