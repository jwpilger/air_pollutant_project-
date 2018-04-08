
plot3 <- function(df1,project_folder,script_n) {
    plot_file <- file.path(project_folder,paste(script_n,".png",sep=""))
    png(file=plot_file,width=480, height=480,units="px")
   
    q <- qplot(year,data=df1, geom="bar",facets=.~type,weight=Emissions,fill=year,
          main="Baltimore PM2.5 Emissions by Year and Type", 
          xlab="Year", ylab = "Emissions (tons)")
    print(q)
    dev.off()   # close graphics device
    
    return(plot_file)
}



# define required libs and source files
library(RCurl)
library(dplyr)
library(ggplot2)
source("downloadfile.R")

#define variables

Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
script_r <-  tools::file_path_sans_ext(basename(r_source_name()))
project_folder <- "Pollution"    # set to  "" if script name to be used
baltimore_fips <- "24510"

log(script_r,"#Coursera Exploratory Data Analysis course project 2")
log(script_r,"#Explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United States")
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
    

log(script_r,"##Have PM2.5 emissions types decreased in Baltimore from 1999 to 2008?")
log(script_r,"##The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.")
log(script_r,"##Using the ggplot2 plotting package, make a plot showing Baltimore PM2.5 emission.")

    NEI_baltimore <- NEI[NEI$fips == baltimore_fips,]
log(script_r,"##PM2.5 Baltimore Emissions (subset) data rows x cols : ", nrow(NEI_baltimore)," x ", ncol(NEI_baltimore)) 

log(script_r,"##Sum Emissions by year and type for Baltimore - fips = ",baltimore_fips)

    NEI_baltimore$year <- as.character(NEI_baltimore$year) # char for plotting 
    df <- aggregate(Emissions~year+type, data=NEI_baltimore, sum)
log(script_r,"##[3]Plot aggregate by type and show four years -  data rows x cols : ", nrow(df)," x ", ncol(df)) 

log(script_r,"##[4]Create a barchart showing emissions by year and type for Baltimore")
    print(df)
    plot_ds <- plot3(df,project_folder_name,script_r)
log(script_r,"## ",plot_ds," Complete and written to project folder")

} else {
log(script_r,"Error: Download failed rc= ", rc_down)    
}