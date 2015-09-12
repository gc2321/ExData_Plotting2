plot6 <- function() {
  
  # read data
  nei <- readRDS("summarySCC_PM25.rds")
  scc <- readRDS("Source_Classification_Code.rds")
  
  d<- transform(nei, year=factor(year), SCC=factor(SCC), fips=factor(fips) )
  d<- transform(nei, year=factor(year), SCC=factor(SCC) )
  
  # subset scc where EI.sector contains "Mobile"
  mobile<-grep("^Mobile -", scc$EI.Sector, ignore.case=T)
  scc2<- scc[mobile,]
  
  # obtain a list of SCC where EI.secotr contains "Mobile"
  scc_mobile<-unique(scc2$SCC)
  
  # subset d based on scc_mobile
  d2<-d[d$SCC %in% scc_mobile,]
  
  # subset d2 on Baltimore City and Los Angeles County
  d3<-d2[d2$fips %in% c("24510","06037"),] 
  
  library(dplyr) 	
  d4<- tbl_df(d3) 
  d4<-select(d4, Emissions, fips, year) 	
  
  d5<- transform(d4, fips=factor(fips))
  
  d6<-xtabs(Emissions ~., data=d5)
  d7<-as.data.frame(d6)
  
  library(ggplot2)
  
  levels(d7$fips)[levels(d7$fips)=="06037"]<-"Los Angeles, CA"
  levels(d7$fips)[levels(d7$fips)=="24510"]<-"Baltimore, MD"
  
  a<-xlab("Year")
  b<-ylab("Emissions")
  c<-ggtitle("Emissions from motor vehicle sources \n in Baltimore City and Los Angeles County from 1999 - 2008")	
  
  p<-ggplot(d7, aes(x=year, y=Freq))+geom_bar(stat="identity")+facet_grid(. ~fips)+a+b+c
  
  # convert to png
  png("plot6.png", width=480, height=480)
  print(p)
  dev.off()
  
  
  
} 
  
  
  
  