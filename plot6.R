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

place_data<-function(){
      
      filef <- "./plot5.csv"
      baltimore <- "24510"
      LA <- "06037"
      
      if (!file.exists(filef)) {                          ## does subset csv exist           
            print("Loading Data...")
            
            df<-load_data("NEI")                      ## Load NEI data
            
            areas<-subset(sector.df,fips==baltimore | fips==LA) 
            
            df2 <- sectordf[, c("fips","Emissions","type","year")]      
            rownames(df2) <- NULL
            
            write.csv(df2, file = filename)           ## Write csv for plotting
            
      }
      
      print("File created")                           
      
      df <- read.csv(f)
      
      return(df)                                      ## Return df of subset
      
      
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
      
      print("NEI subset for Sector exists")                           
      
      df <- read.csv(f)
      
      return(df)                                      ## Return df of subset
}

totals_for_states<-function(){
      
      f2<-"./state_totals.csv"
      
      if (!file.exists(f2)) {                          ## does subset csv exist           
            print("Making state data csv")
            
            sector.df <- data_from_sector("vehicles")        ## get data that matches this string
            
            baltimore <- "24510"
            LA <- "6037"            ###  Check number of rows: print(sum(sector.df$fips == LA))
            
            balt.df <- subset(sector.df,  fips==baltimore)   ## subset sector data Baltimore only
            LA.df <- subset(sector.df, fips==LA)            ## subset sector data LA County only
            
            year<- c("1999", "2002", "2005", "2008")
            
            balt.tot <- sapply(year, total_for_year, data=balt.df) ##  totals for each year
            LA.tot <- sapply(year, total_for_year, data=LA.df) 
            
            
            sum.df <- data.frame(year, balt.tot, LA.tot)        ## Create data frame for totals
            
            write.csv(sum.df, file = "state_totals.csv")           ## Write csv for plotting
      }
      
      print("state totals exists")                           
      
      f2.df <- read.csv(f2)
      
      return(f2.df)                                      ## Return df of data
      
}

total_for_year<-function(yr,data){
      
      v<-sum(subset(data, year==yr, select = Emissions))          
      s<-as.numeric(as.character(v))
      
      return(s)
}

differences<-function(x){
      
      dif<-c(0)
      
      for (i in 1:length(x)) {
            
            if(i>1){
                  this.dif <- x[1] - x[i]                   ## difference from 1999
                  this.p_dif <- (0-(this.dif/x[1]))*100     ## % difference from 1999
                  dif[i]<-this.p_dif                        ## add to array
            }
      }
      return(dif)
}

plot6<-function(){
      
      df <- totals_for_states() # df total emissions LA/Baltimore for "vehicles"
      
      balt.dif <- differences(df$balt.tot) ##  % difference for each year
      LA.dif <- differences(df$LA.tot) ##  % difference for each year
      year<- c("1999", "2002", "2005", "2008")
            
      dif.df <- data.frame(year, balt.dif, LA.dif)        ## df for % differences
      names(dif.df)<-c("year","Baltimore", "LA County")
      
      mdf<-melt(dif.df)
      names(mdf)<-c("year","location", "change")
      colors<-c("#0575c9", "#ed1e0e")
      
      
      #################### Plot
            
      library(ggplot2)
      
      p <- ggplot(mdf, aes(x=year, y=change, group=location))
      p + geom_line(aes(colour = location), size = 1.2) + 
            geom_point(alpha = 1/4, size=4) +
            xlab("10 years 1999 - 2008") + 
            ylab("Change in PM2.5 Emissions (%)") +
            ggtitle("Baltimore: Significant drop, LA County: Slight rise")+
            theme(plot.title = element_text(colour="grey40",size=12,vjust = 2),
                  axis.text.x = element_text(colour="grey40",size=10),
                  axis.text.y = element_text(colour="grey40",size=10),
                  legend.title = element_text(size = 11))
      
      
      ## PNG
      ggsave("plot6.png", width=6, height=4, dpi=100)
      
}