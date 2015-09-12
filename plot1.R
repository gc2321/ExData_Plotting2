plot1 <- function() {
  
  # read data
  nei <- readRDS("summarySCC_PM25.rds")
  #scc <- readRDS("Source_Classification_Code.rds")
  
  d<- transform(nei, year=factor(year) )
  
  total<-tapply(d$Emission, d$year, sum)
  total2<-as.data.frame(total)
  
  # convert to png
  png("plot1.png", width=480, height=480)
  
  barplot(total2$total, xlab="Year", ylab="Emission", main = "Total emission from PM2.5 in US from 1999 - 2008")
  
  dev.off()
  
} 
  
  
  
  