loadData <- function(){
  
  # Open a connection to KDB
  conn <- dbQ('localhost', 5000)
  
  # Load the data to R
  eod <- execute(conn, 'select globex, descrip, fo, 
                  date, yr, mon, day, lasttrade, pc, 
                  strike, settle, delta, impvol, 
                  openinterest, totvlm, globvlm, flrvlm, 
                  GlobexOpen, GlobexHigh, GlobexLow from eod1')
  
  # Save the data as RData format
  save(eod, file = '/Users/sahin/R/data/eod.RData')
}

