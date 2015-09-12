plot5 <- function() {
  
  # read data
  nei <- readRDS("summarySCC_PM25.rds")
  scc <- readRDS("Source_Classification_Code.rds")
  
  d<- transform(nei, year=factor(year), SCC=factor(SCC) )
  
  # subset scc where EI.sector contains "Mobile"
  mobile<-grep("^Mobile -", scc$EI.Sector, ignore.case=T)
  scc2<- scc[mobile,]
  
  # obtain a list of SCC where EI.secotr contains "Mobile"
  scc_mobile<-unique(scc2$SCC)
  
  # subset d based on scc_mobile
  d2<-d[d$SCC %in% scc_mobile,]
  
  # subset d2 on Baltimore City
  d3<-d2[d2$fips=="24510",]
  
  # compute total emission of d3 for each year
  total<-tapply(d3$Emission, d3$year, sum)
  total2<-as.data.frame(total)
  
  # convert to png
  png("plot5.png", width=480, height=480)
  
  barplot(total2$total, xlab="Year", ylab="Emission", main = "Emission from motor vechicle sources\n in the Baltimore City from 1999 - 2008")
  
  dev.off()
  
  
} 
  
  
  
  