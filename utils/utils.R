####################################################################################################
#  dbQ() opens Q connection to a single Q server or a list
#    Host and port may be given as separate lists, or host may be strings  host:port
#  dbClose()  closes a list of database handles
#
#  Examples:  dbQ('atlas',c(5012,6012,7012))
#             dbQ(c('ecqb3:5011','fra1:5011','fra1:5041'))
#             dbQ(10020)  host is "localhost"
#
#  For hosts and ports for which we cannot open the connection, returned handle is NA

dbQ <- function(
  host,                 #  host name or vector of names, possibly including port numbers
  port=NULL,            #  port number or vector of numbers, if not part of host
  stopOnFailure=FALSE,  #  whether to stop (fatal error) if we cannot open a connection
  verbose=FALSE)        #  whether to display helpful console output for debugging
{
	#  Special case of  dbQ(portnum)  where host is localhost
	if ((missing(port) || is.null(port)) && is.numeric(host))  { port <- host;  host <- 'localhost'; }

	#  Expand each of host,port to a vector of common maximum length
	n <- max( length(host), length(port) )
	host <- rep(host,length.out=n)
	if (!is.null(port)) port <- rep(port,length.out=n)

	if (verbose)
	{	print(paste('host =',paste(host,collapse=', ')),quote=FALSE)
		if (!is.null(port)) print(paste('port =',paste(port,collapse=', ')),quote=FALSE) }

	#  Decide whether we were given  host:port  or separate lists
	if (all(grepl('^[[:alnum:]]+:[[:digit:]]+$',host)))
	{	L <- strsplit(host,':')
		host <- unlist( lapply( L, function(x) { return(x[[1]]) } ))
		port <- as.numeric(unlist( lapply( L, function(x) { return(x[[2]]) } ))) }
	else
	{	if (is.null(port))
			stop('dbQ called without port but host argument is not identified as host:port') }

	if (verbose) print(data.frame(host=host,port=port,stringsAsFactors=FALSE))

	#  Open all the connections
	db <- rep(NA,n)
	for (i in seq_len(n))
	{	try( db[i] <- open_connection(host=host[i],port=port[i]), silent=TRUE )
		if (is.na(db[i]))
		{ 	msg <- paste0('Cannot open connection to ',host[i],':',port[i])
			if (stopOnFailure) stop(msg)
			else               print(msg,quote=FALSE) } }

	if (verbose) print(data.frame(host=host,port=port,db=db,stringsAsFactors=FALSE))

	db
}

dbClose <- function(db) { for (d1 in db[!is.na(db)]) close_connection(d1) }

####################################################################################################
#  dupl() returns a new vector in which each element is repeated 2 times
#   example:  dupl(c(1,2,3))  returns   1 1 2 2 3 3
#  It is easy to modify this function to allow other values of n,
#   but dangerous because it is easy to accidently call with large values of n
#
#  Main application is to generate step plots without using  plot(...,mode='s')
#  For example, given vectors  x and y of length n (x increasing and y nonnegative)
#   and a right-side value  xR >= x[n],
#   one can generate a nice plot with a filled area underneath by
#
#      polygon( dupl(c(x,xR)), c(0,dupl(y),0), col='light gray', border=NA )
#      lines( c(x[1],dupl(x[-1]),xR), dupl(y) )
#      points( x, y, pch=20 )

dupl <- cmpfun( function(x) rep(x,each=2) )

####################################################################################################
#  Qsym()  convert arbitrary string or list of strings into a form acceptable to Q
#
#           'GEH2'               -->       `GEH2
#         'GEH2-GEZ2'            -->   `$"GEH2-GEZ2"
#       c('GEH2','GEZ2')         -->     `GEH2`GEZ2
#  c('GEH2','GEZ2','GEH2-GEZ2')  -->  (`GEH2;`GEZ2;`$"GEH2-GEZ2")

Qsym <- cmpfun( function(sym)
{
	if (!inherits(sym,'character')) stop('sym must be a string or vector of strings')

	if (length(sym)==0)  return('')

	isatom <- grepl('^[A-Za-z0-9]*$',sym)          #  whether Q will accept as atom like "`GEH2"
	sym[!isatom] <- paste0('$"',sym[!isatom],'"')  #  add quotes to ones that need it
	sym <- paste0('`',sym)                         #  add back-tick to all

	#  Concatenate if more than one element
	if (length(sym)>1)
	{	if (any(!isatom)) sym <- paste0('(',paste(sym,collapse=';'),')')
		else              sym <- paste(sym,collapse='') }

	sym
})

####################################################################################################
#  Add a clause or clauses to a query or to an existing clause
#  Examples:  qappend('select x from t','a=b','p=q')     -->  "select x from t where a=b,p=q"
#             qappend('select x from t where p=q','r>0') -->  "select x from t where p=q,r>0"
#             qappend('a=b','not null x','r>0')          -->  "a=b,not null x,r>0"

qappend <- cmpfun( function( originalClause, ... )
{
	qapp <- function(orig,newcls)
	{
		if (newcls=='') return(orig)
		if (orig=='')   return(newcls)

		#  remove leading and trailing whitespace, and comma or 'where' from newcls
		newcls <- sub('^(,[ ]*|where[ ]+)','',str_trim(newcls),ignore.case=TRUE)

		if (grepl('(select|exec)',orig))   # original query is a complete select statement
		{
			if (grepl('where[ ]*$',orig))  #  query ends with "where":  add our clause with one space
				return(paste(str_trim(orig,side='right'),newcls))
			else if (grepl('where',orig))  #  query has a "where" plus some other clause:  add with comma
				return(paste(orig,newcls,sep=','))
			else                           #  query has no "where":  add our query with a "where"
				return(paste(orig,'where',newcls))
		}
		else                        #  original query is only a list of where clauses
		{
			if (orig=='')                  #  list of clauses is empty:  new list is simply our clause
				return(newcls)
			else if (grepl('[ ]*where[ ]*$',orig))  #  original is simply the word "where"
				return(paste(str_trim(orig,side='right'),newcls))
			else                           #  list is non-empty:  append our clause with commma
				return(paste(orig,newcls,sep=','))
		}
	}

	for (newClause in list(...)) originalClause <- qapp(originalClause,newClause)
	originalClause
} )

####################################################################################################
#  qtfmt() formats a POSIXct time, or vector of POSIXct times, to be part of a Q query
#   It uses the shortest of three forms:
#         2013-11-04 15:56:47     if time is a round number of seconds
#         2013-11-04 15:56:47.123 if time is a round number of milliseconds
#         float with 15 digits    if time is not a round number of milliseconds or seconds

#  An R POSIXct object is the double-precision number of seconds since 1970-01-01 00:00:00 UTC.
#  We are currently between   2^30 = 2004-01-10 13:37:04 UTC   and   2^31 = 2038-01-19 03:14:08 UTC
#  Therefore the spacing between successive discrete times is
#    dtminR  =  2 * eps * 2^30 sec  =  2^(-22) sec  =  2.38e-07 sec,
#    where eps = machine precision = 2^-53 = 1.1e^-16
#  Times that are integer numbers of seconds are represented exactly.
#
#  Verification:
#
#   > eps <- 2^(-53)           1.11022302463e-16
#
#   > t0 <- Sys.time()         "2013-10-02 13:54:35.818 EDT"
#
#   > dtmin <- as.difftime( eps * 2^31, units='secs' )
#   Time difference of 2.38418579102e-07 secs
#
#   > ( t0 + 0.5*dtmin ) - t0     #  Adding 0.5 * dtmin  rounds up to  t0 + dtmin
#   Time difference of 2.38418579102e-07 secs
#
#   > ( t0 + 0.49*dtmin ) - t0    #  Adding anything less rounds back down to t0
#   Time difference of 0 secs
#
#  R has the strangeness (bug) that on output using the time formats, values are rounded down
#  to the appropriate discretization. Thus to print a time to the nearest millisecond, we must
#  add an offset equal to 1/2 of a millisecond to get proper rounding.
#
#  A Q datetime object is the double-precision number of days since 2000-01-01 00:00:00 UTC.
#  We are currently between 2^12 = 2011-03-20 and 2^13 = 2022-06-06.
#  Therefore the spacing between successive discrete times is
#   dtminQ  =  2 * eps * 2^12 day  =  2^-40 day  =  7.85e-08 sec
#  This is smaller than dtminR by a factor 3, which is due to the different origin:  from
#    2011-03-20, the origin 2000-10-01 is 4096 days in the past; 1970-01-01 is 15053 days.
#  Times that are integer numbers of days are represented exactly, but not integer seconds.
#
#  Verification:
#
#   q)t0: .z.z
#   q)t0
#   2013.11.04T20:10:43.096
#
#   q)dtmin:2 xexp neg 40
#   q)dtmin
#   9.0949470177293e-13
#
#   q)( t0 + 0.51*dtmin ) - t0    #  adding a bit more than 0.5 dtmin rounds up to t0 + dtmin
#   9.0949470177293e-13
#   q)( t0 + 0.5*dtmin ) - t0     #  adding a bit less than 0.5 dtmin rounds back down to t0
#   0f
#
#  Q has the strangeness that on conversion of a string to a datetime, digits in the string
#   beyond millisecond precision are ignored. Thus for times which are not integer values
#   of milliseconds, we must represent the value as a float string, and convert that to datetime.

#  Convert an R POSIXct object into Q numerical value
time2qfloat <- cmpfun( function(x)
{
	if (!inherits(x,'POSIXct')) stop('time2qfloat() input must be a POSIXct object')
	torigin <- as.POSIXct('2000-01-01 00:00:00',tz='UTC')  #  origin for Q times
	attr(torigin,'tzone') <- NULL  #  remove time zones to prevent warnings
	attr(x,'tzone') <- NULL
	as.numeric(x-torigin,units='days')
})

qtfmt <- cmpfun( function(x)
{
	if (!inherits(x,'POSIXct'))    stop('qtfmt() input must be a POSIXct scalar or vector')

	#  As described above, dtmin is the interval between successive discrete R times
	dtmin <- 2^(-22)

	#  dtR is an offset necessary to handle the R bug that printing times with decimal
	#   seconds rounds down rather than to nearest
	dtR <- as.difftime( 0.0005, units='secs' )

	#  xn is the numeric value of x, in seconds since the epoch
	xn <- as.numeric(x)

	#  issec is TRUE where x is an exact number of seconds.
	#  We do not need a tolerance on the comparison since integers are represented exactly
	#  These values are printed with no decimal digits
	issec <- ( xn == round(xn) )

	#  ismsec is TRUE where x is an exact number of milliseconds (and x is not exact in seconds)
	#  The comparison tolerance is 1/2 of the interval between successive R times
	#  These values are printed with three decimal digits, the max number that Q notices
	ismsec <- ( !issec & ( abs( xn - round(xn,3) ) < 0.5 * dtmin ) )

	#  isnsec is TRUE where x is neither an exact number of seconds nor milliseconds
	#  These values are printed using full 15-digit float numbers, converted in Q to datetime
	isnsec <- !( issec | ismsec )

	y <- rep('',length(x))
	y[issec] <- format( x[issec], '%Y.%m.%dT%H:%M:%S', tz='UTC' )
	y[ismsec] <- format( x[ismsec] + dtR, '%Y.%m.%dT%H:%M:%OS3', tz='UTC' )
	y[isnsec] <- paste0('15h$',format(time2qfloat(x[isnsec]),digits=15))

	y
})

qdfmt <- cmpfun( function(x) format(as.Date(x),'%Y.%m.%d') )

#  qtcmp(): Return a Q predicate expressing   tname op tval, op is one of "<", ">", "<=", or ">="
#
#  For example,
#      qtcmp( 'time', '<=', as.POSIXct('2013-04-03 10:11:12.123',tz='America/Chicago') )
#  will return a construction equivalant to
#
#        "time<=2013.04.03T15:11:12.123"   (Q timestamps are always UTC)
#
#  We do not produce this string for two reasons:
#   (1)  Q parsing of datetime strings is accurate only to millisecond level.
#          Solution:  express constants as 15-digit float and do comparison on float
#   (2)  Q comparison applies a floating-point tolerance unless comparison is against zero.
#          Solution:  rearrange expressions to compare against zero:  "x<y"  -->  "0<y-x"
#  Thus the actual result from the above example will be
#
#       "0f<=4841.63277920139-9h$time"
#
#  Note that this relies on Q right-to-left expression grouping.
#  The equality case will likely never happen because of float roundoff error.
#  This function may work if tname is not just a variable name but an expression,
#    for example tname="(t+deltat)" -- the parentheses are likely necessary depending
#    on whether or not Q cast operator $ obeys right-to-left grouping or not (I don't know).

qtcmp <- cmpfun( function(tname,op,tval)
{
	if (!inherits(tname,'character')) stop('tname must be a variable name')
	if (!inherits(tval,'POSIXct'))    stop('tval must be a POSIXct object')
	if ( !inherits(op,'character') || !( op %in% c('<','>','<=','>=') ) )
		stop('op must be a string representing an operator "<", ">", "<=", or ">="')

	paste0('0f',op,format(time2qfloat(tval),digits=15),'-9h$',tname)
} )

####################################################################################################
#  R frequently loses Date and POSIXct attributes
#    For example,  t3 <- ifelse(condition,t1,t2)  will make t3 be a pure numeric
#       even if both t1 and t2 are POSIXct.
#  These functions toT() and toD() convert numeric back to POSIXct and Date respectively.
#  They require explicit definition of the origin point, which is lost on numeric conversion.

torigin <- ISOdatetime(1970,1,1,0,0,0,tz='UTC')
stopifnot(as.numeric(torigin)==0)   #  make sure this value is correct
toT <- cmpfun( function(x) as.POSIXct(x,origin=torigin) )

dorigin <- as.Date('1970-01-01')
stopifnot(as.numeric(dorigin)==0)   #  make sure this value is correct
toD <- cmpfun( function(x) as.Date(x,origin=dorigin) )

####################################################################################################
#  Trade date identification changes over at 17:30 NY (6.5 hours before midnight)
#   This function is also used in oplot() and elsewhere.
UTC2tradedate <- cmpfun( function(x)
{	if (!inherits(x,'POSIXct'))  stop('UTC2tradedate() must be called with a POSIXct object')
	as.Date( x + as.difftime(6.5,units='hours'), tz='America/New_York' ) } )

####################################################################################################
#  Parse a datetime string, or a pure time with optional date
tparse <- cmpfun( function(
   x,   #  Single time string, either as '2014-01-10 12:34:55' or '12:34:55' (not negative)
   d,   #  Date (or string) to be used if time string does not have a date (required)
   tz='UTC' )  #  time zone for parsing
{
	if (inherits(x,'POSIXct'))
	{	y <- x }
	else
	{
		y <- NULL
		try( y <- as.POSIXct(x,tz=tz), silent=TRUE )

		if (is.null(y))
		{	if (missing(d))
				stop(paste('d must be given in tparse() if x does not have a date,',
				    'since we make no calls to Sys.Date() or Sys.time()'))
			y <- as.POSIXct( paste(format(as.Date(d),'%Y-%m-%d'),x), tz=tz ) }
	}

	#  Remove the time zone attribute so comparisons never complain about mismatched zones
	attr(y,'tzone') <- NULL
	y
} )

####################################################################################################
#  Console display of POSIXct times, with optional date and/or timezone
tfmt <- cmpfun( function(
   x,             #  time to be displayed. May be scalar or vector, but must be POSIXct
   sec=TRUE,      #  whether to show seconds columns
   msec=TRUE,     #  whether to show milliseconds (also need sec=TRUE)
   d=FALSE,       #  whether to show date
   z=TRUE,        #  whether to show time zone
   na.print='NA', #  what to show for NA values
   tz='UTC'       #  time zone
)
{
	if (!( inherits(x,'POSIXct') || ( inherits(x,'logical') && all(is.na(x)) ) ))
		stop(paste('tfmt() called with object of class',paste(class(x),collapse=',')))
	fmt <- '%H:%M'
	toffset <- as.difftime(0,units='secs')
	if (sec)
	{	if (msec)
		{	fmt <- paste(fmt,'%OS3',sep=':');
			toffset <- as.difftime(0.0005,units='secs') }
		else
		{	fmt <- paste(fmt,'%S',sep=':') } }
	if (d) fmt <- paste('%Y-%m-%d',fmt)
	if (z) fmt <- paste(fmt,'%Z')

	#  Handle scalars and vectors differently since can find no way to unify
	y <- rep(na.print,length(x))
	iok <- !is.na(x)
	if (length(x)>1) { y[iok] <- format(x[iok]+toffset,fmt,tz=tz) }
	else if (iok)    { y <- format(x+toffset,fmt,tz=tz) }
	y
} )

####################################################################################################
#  tprint()  displays a data frame, showing only the head and tail if it is too large
tprint <- function(
   X,           #  data frame or matrix
   nm=10,       #  max number of rows to print as full table
   nr=3,        #  if we do split into head and tail, how many rows in each
   digits=15,   #  number of significant digits to show for numeric columns
   decdigits,   #  number of decimal digits to show after the decimal point for numeric columns
   sec=TRUE,    #  whether to show seconds in POSIXct columns
   msec=TRUE,   #  whether to show milliseconds in POSIXct columns (also need sec=TRUE)
   d=FALSE,     #  whether to show date on POSIXct columns
   z=TRUE,      #  whether to show time zone on POSIXct columns
   na.print='', # what to show for NA values
   tz='UTC',     #  time zone to use for display of POSIXct columns
   epsfmt=1e-14  #  fractional amount to add to float values to compensate for roundoff error
)
{
	if (missing(decdigits))
		ffmt <- function(x) format(x,digits=digits)
	else
	{	stopifnot( is.numeric(decdigits) && decdigits>=0 && decdigits==round(decdigits) )
		ffmt <- function(x) sprintf('%.*f',decdigits,x*( 1 + epsfmt )) }

	#  Convert a data.frame to character representation
	tochar <- function(x)
	{
		if (nrow(x)==0) return(x)
		y <- x
		for (i in seq_len(ncol(y)))
		{
			if (inherits(x[,i],'POSIXct'))
			{	y[,i] <- tfmt(x[,i],sec=sec,msec=msec,d=d,tz=tz,z=z,na.print=na.print) }
			else
			{	y[,i] <- ffmt(x[,i])
				if (all(is.numeric(x[,i])))
					y[ is.na(x[,i]) | is.null(x[,i]) | is.nan(x[,i]), i] <- na.print }
		}
		y
	}

	if ( !inherits(X,'data.frame') && !inherits(X,'matrix') )
		stop(paste('tprint called with object of class',paste(class(X),collapse=',')))

	if ( nm<=0 || nrow(X)<=nm )   #  Print the entire array
	{
		print(tochar(X),na.print=na.print,quote=FALSE)
	}
	else                   #  Print just the head and tail
	{
		X1 <- tochar(head(X,nr))
		X2 <- tochar(tail(X,nr))

		#  Make a 1-line data.frame for the middle
		if (class(X)=='data.frame')
		{	if (ncol(X)>1)
			{	Xm <- X1[1,]
				for (i in seq_len(ncol(Xm)))  Xm[,i] <- '...' }
			else
			{	Xm <- data.frame(x='...',stringsAsFactors=FALSE) } }
		else if (class(X)=='matrix') { Xm <- matrix('...',nrow=1,ncol=ncol(X)) }
		if (!is.null(colnames(X))) colnames(Xm) <- colnames(X)
		else                       colnames(Xm) <- NULL
		rownames(Xm) <- '...'

		print( rbind(X1,Xm,X2),na.print=na.print,quote=FALSE )
	}
}

####################################################################################################
#  setPDF()  sets up to plot a PDF file using the Cairo driver
#   The default width and height are appropriate for a full-page orderplot
#    but can be adjusted as necessary for other purposes

setPDF <- function(
   filename,     #  Name of output PDF file.  Cairo will add the ".pdf" suffix if not included
   width=7,      #  Width and height of output file, in inches
   height=10,    #  NB: good sizes for inclusion in Keynote are  width=13, height=9, pointsize=18
   pointsize=9,  #  Size of text on plot.
   fontname='Arial',   #  Values to use for all four text types:  upright/italic and normal/bold
   fontstyle='Normal', #  These must match something shown by   "CairoFontMatch(':',sort=TRUE)"
   verbose=FALSE)
{
	if (verbose)
		print(paste0('setPDF( filename=',filename,', width=',width,', height=',height,
		   ', pointsize=',pointsize,', fontname=',fontname,', fontstyle=',fontstyle,' )'),quote=FALSE)
	#  We are switching to using the "Cairo" graphics package from the default pdf() driver
	#  Cairo stores fonts as UTF-8 rather than 8-bit ASCII with a code page.
	#   This allows specification of a wide range of symbols such as fractions and Euro symbols
	Cairo(type='pdf',file=filename, width=width,height=height,units='in', pointsize=pointsize )
	if (verbose) print(paste('PDF output to',filename),quote=FALSE)

	#  Set font name and style. We set all four fontspecs to the "Regular" or "Normal" style,
	#  because Cairo seems to makes all text display in italic, ignoring what is specified.
	#  Linux installation lists an "Arial:Regular", while Mac has "Arial:Normal" but either
	#   "Normal" or "Regular" seems to work on both.
	fontspec <- paste0(fontname,':style=',fontstyle)
	if (verbose) print(paste('Setting all font types to',fontspec),quote=FALSE)
	CairoFonts(
 	      regular=fontspec,
	         bold=fontspec,
	       italic=fontspec,
	   bolditalic=fontspec,
	       symbol="Symbol")
}
