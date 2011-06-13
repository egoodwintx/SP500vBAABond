# This is a simple graph of BAA Bond historic bond yields
# and S&P 500 earnings yield
# vs. the S&P 500 earnings yields
#
# author: Ed Goodwin
# date  : 08.10.2010
#
library(ggplot2)
library(xts)
# NOTE: assumes that options(stringsAsFactors = FALSE) has been set in .Rprofile
# pull bond yields from Fed website and convert to std format data frame with Dates and numerics
baa <- read.csv("http://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=b2763fc01e872c23bc1aa1ef99a9a8de&lastObs=&from=&to=&filetype=csv&label=include&layout=seriescolumn", header=TRUE, skip=5)
baa <- data.frame("date"=as.Date(baa$Time.Period, format="%Y-%m-%d"), "baayield"=(as.numeric(baa$RIMLPBAAR_N.B)/100))
# change ND (no data) to NA
newbaa <- na.omit(baa)

# pull P/E yields from multpl.com and calc earnings yield
sp500pe <- read.csv("http://www.multpl.com/s-p-500-pe10.csv", header=TRUE)
sp500pe.colnames <- c("Date", "S.P.500.PE.Ratio")
sp500pe <- data.frame("date"=as.Date(sp500pe$Date, format="%Y-%m-%d"), "PE"=sp500pe$S.P.500.PE.Ratio, "spyield"=1/sp500pe$S.P.500.PE.Ratio)

# pull dividend yields for S&P
sp500div <- read.csv("http://www.multpl.com/s-p-500-dividend-yield/s-p-500-dividend-yield.csv", header=TRUE)
sp500div <- data.frame("date"=as.Date(sp500div$Date, format="%Y-%m-%d"), "div"=sp500div$S.P.500.Dividend.Yield/100)

# DATA KLUDGE - April 2011 left out div yield for some reason; add in placeholder
sp500div.missing <- data.frame("date"=as.Date("2011-04-01", format="%Y-%m-%d"), div=.018) # hardcoded div yield
sp500div <- rbind(sp500div, sp500div.missing)

# merge the two S&P dataframes to put div and earnings yield into same dataframe
# then calc total yield (earnings yield + div yield)
sp500tot <- merge(sp500pe, sp500div)
sp500tot$totyield <- sp500tot$spyield + sp500tot$div
sp500plot <- subset(sp500tot, as.Date(date) > as.Date("1985-12-01", format="%Y-%m-%d"))

# TODO: convert BAA yield data to monthly using xts and then run simple regression
baa.xts <- xts(newbaa$baayield, as.Date(newbaa$date))
sp500.xts <- xts(sp500plot$totyield, as.Date(sp500plot$date))

baa.xts <- to.monthly(baa.xts)
sp500.xts <- to.monthly(sp500.xts)

# create data frame from xts objects to use in ggplot
sp500.yield.plot.df <- as.data.frame(sp500.xts)
sp500.yield.plot.df$Date <- as.Date(index(sp500.xts))

baa.yield.plot.df <- as.data.frame(baa.xts)
baa.yield.plot.df$Date <- as.Date(index(baa.xts))

yield.plot.df <- merge(sp500.yield.plot.df, baa.yield.plot.df, by="Date")
yield.plot.df <- data.frame(
                            "Date"  = yield.plot.df$Date,
                            "SP500" = yield.plot.df$sp500.xts.Close,
                            "BAA"   = yield.plot.df$baa.xts.Close
                            )

# melt data to reshape it to allow index factors to graph in different colors
yield.plot.df <- melt(yield.plot.df, id=c("Date"))

# change column names to be more intuitive
names(yield.plot.df) <- c("Date", "Index", "Yield")

# Create plot of timeseries
p <- ggplot(yield.plot.df, aes(Date, Yield, colour=Index)) + geom_point() +
  opts(title="S&P 500 Total Yield vs. BAA Corp Bond Yield")
print(p)
