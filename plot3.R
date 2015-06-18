load_data<-function(){
      
      NEI <- readRDS("./data/summarySCC_PM25.rds")
      SCC <- readRDS("./data/Source_Classification_Code.rds")
      
      return(NEI)
      
}

baltimore<-function(){
      
      # If Baltimore dataset not created already, create baltimore.csv
      if (!file.exists("./baltimore1.csv")) {
            
            print("Loading Data...")
            
            df<-load_data()                           ## Load fullfat datafile
            
            df2<-subset(df, fips=="24510")            ## Subset = only Baltimore City
            
            df3 <- df2[, c("type","year","Emissions")]
            rownames(df3) <- NULL
            
            write.csv(df3, file = "baltimore1.csv")    ## Write a small file for plotting
            
      }
      
      print("File created")
      
      df <- read.csv("./baltimore1.csv")
      
      return(df)
    
}

plot3<-function(){    
      
      this.df <- baltimore()                          ## Returns Baltimore only data.frame
            
      library(reshape)
      
      df.agg <- cast(this.df, year ~ type, sum)       ## Aggregates totals (sum) by year/type

      mdf<-melt(df.agg)                               ## reshapes aggregated data in long form
      rownames(mdf) <- NULL 
      
      
      library(ggplot2)
  
                                                      ## Create Emissions/year plot 
      g <- ggplot(mdf, aes(x=year, y=value))
      g + geom_point(alpha = 1/3) +
            facet_grid(. ~ type) +                    ## Spit data by "type"
            geom_smooth(method="lm", col="steelblue",aes(group = 1), se=FALSE) +
            xlab("Year") + ylab("Total PM2.5 Emissions (tons)") +
            ggtitle('Baltimore: Emissions down for all types except "POINT"') +
            theme(axis.text.x = element_text(colour="grey40",size=8,angle=45),
                  axis.text.y = element_text(colour="grey40",size=8,),
                  title = element_text(colour="grey40",size=9,))
                                                      
      ## PNG
 
      ggsave("plot3.png", width=6, height=4, dpi=100)

}