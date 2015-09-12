plot2 <- function() {
  
  # read data
  nei <- readRDS("summarySCC_PM25.rds")
  #scc <- readRDS("Source_Classification_Code.rds")
  
  d<- transform(nei, year=factor(year) )
  d2<-d[d$fips=="24510",]
  
  total<-tapply(d2$Emission, d2$year, sum)
  total2<-as.data.frame(total)
  
  # convert to png
  png("plot2.png", width=480, height=480)
  
  barplot(total2$total, xlab="Year", ylab="Emission", main = "Total emission from Baltimore City from 1999 - 2008")
  
  dev.off()
  
} 
  
  
  
  