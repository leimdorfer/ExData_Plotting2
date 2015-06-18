load_data<-function(x){
      
      if(x=="SCC"){
            
            SCC <- readRDS("./data/Source_Classification_Code.rds")           
            return(SCC)
            
      } else {
            
            NEI <- readRDS("./data/summarySCC_PM25.rds")          
            return(NEI)            
      }
}

sector_scc<-function(sector_name){        #### Get SCC codes for a specific string
      
      scc<-load_data("SCC")               ## Load all the SCC Data
      
      ## grep on sector string (i.e "coal"  in "EI.Sector")
      
      finds <- grep(sector_name, scc$EI.Sector, ignore.case = TRUE) 
      
      sector_df<-scc[finds, ]             ## subset on rows that contain that string
      
      return(sector_df$SCC)               ## return the SCC codes for matched rows
}

data_from_sector<-function(sector_name){  ## return df of NEI data on a specific sector
      
      filename <- paste0(sector_name,".csv")          ## filename for csv of subset
      f<-paste0("./",filename)
      
      if (!file.exists(f)) {                          ## does subset csv exist           
            print("Loading Data...")
            
            sector <- sector_scc(sector_name)         ## SCC codes related to sector
      
            df<-load_data("NEI")                      ## Load NEI data
            
            sector_rows <- df$SCC %in% sector         ## matches in NEI for secotr SCCs
            
            sectordf<-df[sector_rows, ]               ## only sector-related NEI data
            
            df2 <- sectordf[, c("fips","Emissions","type","year")]      
            rownames(df2) <- NULL
            
            write.csv(df2, file = filename)           ## Write csv for plotting
      
      }
      
      print("File created")                           
      
      df <- read.csv(f)
      
      return(df)                                      ## Return df of subset
}

total_for_year<-function(yr,data){
      
      v<-sum(subset(data, year==yr, select = Emissions))      
      #v<-colMeans(subset(data, year==yr, select = Emissions))      
      s<-as.numeric(as.character(v))
      sk<-s/1000
      
      return(sk)
}

plot5<-function(){
      
      sector.df <- data_from_sector("vehicle")        ## get data that matches this string
      
      baltimore <- "24510"
      
      this.df <- subset(sector.df, fips==baltimore)   ## subset sector data Baltimore only
      
      year<- c("1999", "2002", "2005", "2008")
      
      totals <- sapply(year, total_for_year, data=this.df) ## Return totals for each year
      
      sum.df <- data.frame(year, totals)        ## Create data frame for totals

      library(ggplot2)
      
      g <- ggplot(sum.df, aes(x=year, y=totals))      ##year ~ totals line
      g + geom_point(alpha = 1/3) +
            geom_smooth(method="lm", col="steelblue",aes(group = 1),se=FALSE) +
            xlab("Year") + ylab("Total PM2.5 Emissions (1000 tons)") +
            ggtitle("Downward trend in emissions for motor vehicle sources (Baltimore)")+
            theme(axis.text.x = element_text(colour="grey40",size=8,angle=45),
                  axis.text.y = element_text(colour="grey40",size=8,),
                  title = element_text(colour="grey40",size=11,))
      
      ## PNG
      ggsave("plot5.png", width=6, height=4, dpi=100)
      
}