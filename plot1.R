load_data<-function(){
      
      NEI <- readRDS("./data/summarySCC_PM25.rds")
      SCC <- readRDS("./data/Source_Classification_Code.rds")
      
      return(NEI)
      
}

total_for_year<-function(yr,data){
      
      v<-sum(subset(data, year==yr, select = Emissions))     
      s<-as.numeric(as.character(v))
      sk<-s/1000

      return(sk)
}

plot1<-function(){
      
      df<-load_data()
      
      years<- c("1999", "2002", "2005", "2008")
      
      totals <- sapply(years, total_for_year, data=df)
      
      sum.df <- data.frame(years, totals)
      
      png(filename = "plot1.png",
          width = 480, height = 480, units = "px", pointsize = 12,
          bg = "white",  res = NA,
          type = "quartz")
      
      barplot(sum.df$totals, 
              names.arg=sum.df$years,
              xlab="Year", 
              ylab="Total PM2.5 emitted (1000 tons)",
              main="US: Total emissions down over 10 years",
              col="chocolate3",
              border = NA)
      
      dev.off()
      
}