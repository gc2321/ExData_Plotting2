plot4 <- function() {
  
  # read data
  nei <- readRDS("summarySCC_PM25.rds")
  scc <- readRDS("Source_Classification_Code.rds")
  
  d<- transform(nei, year=factor(year), SCC=factor(SCC) )
  
  # subset scc where EI.sector contains "Fuel Comb..Coal"
  coal<-grep("^Fuel Comb -(.*)- Coal", scc$EI.Sector, ignore.case=T)
  scc2<- scc[coal,]
  
  # obtain a list of SCC where EI.secotr contains "Fuel Comb..Coal"
  scc_coal<-unique(scc2$SCC)
  
  # subset d based on scc_coal
  d2<-d[d$SCC %in% scc_coal,]
  
  # compute total emission of d2 for each year
  total<-tapply(d2$Emission, d2$year, sum)
  total2<-as.data.frame(total)
  
  # convert to png
  png("plot4.png", width=480, height=480)
  
  barplot(total2$total, xlab="Year", ylab="Emission", main = "Total emission from coal combustion-related sources\n in the US from 1999 - 2008")
  
  
  dev.off()
  
} 
  
  
  
  