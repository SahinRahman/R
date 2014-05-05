drawEODVol <- function(){
  
  # Required package
  suppressPackageStartupMessages(library(rgl))
  
  # Load the data
  filename <- file.choose()
  load(filename)
  
  # Get all the globex products present in the data
  allGlobex <- unique(eod$globex)
  
  # Since 'day' is always blank in the data set (not daily option), 
  # we are assuming day=30 for the sake of maturity calculation
  day = 30;
  
  # Create plot for each product
  for(globexName in allGlobex){
    
    # Get only options by fo='O' and filter absent data
    impVolData <- subset(eod, select=(c(globex, pc, date, strike, yr, mon, impvol)), 
                         subset=(globex == globexName & fo == 'O'))
    impVolData <- na.omit(impVolData)
    
    # Should have some data at this point
    if(nrow(impVolData) > 0){
      
      # Strike
      x <- data.matrix(impVolData$strike) 
      
      # Maturity = End date - trade date
      endDates <- as.Date(ISOdate(impVolData$yr, impVolData$mon, day))
      y <- data.matrix(endDates - impVolData$date)
      
      # Implied volatility
      z <- data.matrix(impVolData$impvol)
      
      # Open a new device and plot
      # Planning to use surface3d
      open3d()
      plot3d(x, y, z, main=paste('Implied vol for', globexName, sep=' '), xlab="Strike", 
             ylab="Maturity", zlab="Volatility", type="p", size=5, col=rainbow(1000))
      
    }
  }
}
