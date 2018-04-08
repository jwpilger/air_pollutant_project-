
plot4 <- function(df1,project_folder,script_n) {
    plot_file <- file.path(project_folder,paste(script_n,".png",sep=""))
    png(file=plot_file,width=480, height=480,units="px")
    
    q <- qplot(year,data=df1, geom="bar",weight=Emissions/1000,fill=year,
               main="US PM2.5 Emissions by Year Coal Sites Only", 
               xlab="Year", ylab = "Emissions (thousand tons)")
    print(q)
    dev.off()   # close graphics device
    
    return(plot_file)
}



# define required libs and source files
library(RCurl)
library(dplyr)
library(ggplot2)
source("downloadfile.R")

Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
script_r <-  tools::file_path_sans_ext(basename(r_source_name()))
project_folder <- "Pollution"    # set to "" if script name to be used
filter_coal <- "coal"


log(script_r,"#Coursera Exploratory Data Analysis course project 2")
log(script_r,"#Explore the National Emissions Inventory database regarding fine particulate matter pollution in the US")
log(script_r,"#R script: ",script_r,".R"," Author: John Pilger Date: ",date())

log(script_r,"##[1]Retrieve National Emissions Inventory (NEI) and SCC files using downloadfile function ")
    rc <- downloadfile(Url,script_r,project_folder)    # download file
    file_name <- rc[1,4]    #  path name + file name of downloaded file
    rc_down <- rc[1,1]      #  return code 0 = successful
    project_folder_name <- rc[1,2]  # project folder where was it placed?
    folder_cnt <- rc[1,3]   # num of ds from zipfile

if (rc_down == 0) {  # file was retrived
    
log(script_r,"##[#2] PM2.5 Emissions file ",rc[1,5]) 
    NEI <- readRDS(rc[1,5])
log(script_r,"##PM2.5 Emissions data rows x cols : ", nrow(NEI)," x ", ncol(NEI)) 
#    write.table(NEI, file = file.path(project_folder_name,"NEI.csv"),row.name=FALSE, sep = ',',na="NA",col.name=TRUE)
    
log(script_r,"##[#3] Source Classification Code Table file ",rc[1,4])
     SCC <- readRDS(rc[1,4])
log(script_r,"##Source Classification Code Table data rows x cols : ", nrow(SCC)," x ",ncol(SCC))  
#    write.table(SCC, file = file.path(project_folder_name,"SCC.csv"),row.name=FALSE, sep = ',',na="NA",col.name=TRUE)     
    NEI_scc <- merge(NEI, SCC, by="SCC") 
log(script_r,"##Merge NEI and SCC data frames rows x cols : ", nrow(NEI_scc)," x ",ncol(NEI_scc))    
  
  NEI_coal <- NEI_scc[grepl(filter_coal,NEI_scc$Short.Name,ignore.case=TRUE),] 
log(script_r,"##PM2.5 Emissions filter data rows x cols : ",filter_coal,"  ", nrow(NEI_coal)," x ", ncol(NEI_coal))  

log(script_r,"##Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?")
log(script_r,"##The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.")
log(script_r,"##Using the ggplot2 plotting package, make a plot showing US PM2.5 emission from coal.")
    

    NEI_coal$year <- as.character(NEI_coal$year) # char for plotting 
    df <- aggregate(Emissions~year, data=NEI_coal, sum)
log(script_r,"##[3]Plot aggregate by year -  data rows x cols : ", nrow(df)," x ", ncol(df)) 
    
log(script_r,"##[4]Create a barchart showing emissions for coal points")
    print(df)
    plot_ds <- plot4(df,project_folder_name,script_r)
log(script_r,"## ",plot_ds," Complete and written to project folder")
    
} else {
    log(script_r,"Error: Download failed rc= ", rc_down)    
}
