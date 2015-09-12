plot3 <- function() {
  
  # read data
  nei <- readRDS("summarySCC_PM25.rds")
  #scc <- readRDS("Source_Classification_Code.rds")
  
  d<- transform(nei, year=factor(year), type=factor(type) )
  d2<-d[d$fips=="24510",]
  
  library(dplyr) 	
  
  d3<- tbl_df(d2) 
  d3<-select(d3, Emissions, type, year) 	
  
  d4<-xtabs(Emissions ~., data=d3)
  d5<-as.data.frame(d4)
  
  library(ggplot2)
  
  a<-xlab("Year")
  b<-ylab("Emissions")
  c<-ggtitle("Type of emissions in Baltimore City from 1999 - 2008")	
  
  p<-ggplot(d5, aes(x=year, y=Freq))+geom_bar(stat="identity")+facet_grid(. ~type)+a+b+c
  
  # convert to png
  png("plot3.png", width=580, height=480)
  print(p)
  dev.off()
  
} 
  
  
  
  