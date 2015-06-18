load_data<-function(){
      
      NEI <- readRDS("./data/summarySCC_PM25.rds")
      SCC <- readRDS("./data/Source_Classification_Code.rds")
      
      return(NEI)
      
}


total_for_year<-function(yr,data){
      
      v<-sum(subset(data, year==yr, select = Emissions))  ## Sum of all emissions for year 
      s<-as.numeric(as.character(v))
      sk<-s/1000

      return(sk)
}

plot2<-function(){
      
      df<-load_data()
      
      sub.df<-subset(df, fips=="24510")               # only Baltimore City Maryland
      
      years<- c("1999", "2002", "2005", "2008")
      
      totals <- sapply(years, total_for_year, data=sub.df)
      
      sum.df <- data.frame(years, totals)
      rownames(sum.df) <- NULL
      
      #################### PNG
      
      png(filename = "plot2.png",
          width = 480, height = 480, units = "px", pointsize = 12,
          bg = "white",  res = NA,
          type = "quartz")
      
      #################### Plot
      
      barplot(sum.df$totals, 
              names.arg=sum.df$years,
              xlab="Year", 
              ylab="Total PM2.5 emitted (1000 tons)",
              main="Baltimore: Total emissions down over 10 years",
              col="darkolivegreen",
              border = NA)
      
      #################### End 
      
      dev.off()
     
      
}