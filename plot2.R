plot2 <- function(df1,project_folder,script_n) {
    plot_file <- file.path(project_folder,paste(script_n,".png",sep=""))
    png(file=plot_file,width=480, height=480,units="px")
    barplot(df1$Emissions,
            xlab = "Years",
            ylab = "Total Emissions Tons",
            main = "Baltimore PM2.5 Emissions by Year",
            names.arg = df1$year,
            col = "mistyrose")
    dev.off()   # close graphics device
    return(plot_file)
}
# Use the base plotting package to draw a bar plot of our
# aggregate calculation.


# define required libs and source files
library(RCurl)
library(dplyr)
library(data.table)
source("downloadfile.R")

#rm(list = ls())

#define variables

Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
script_r <-  tools::file_path_sans_ext(basename(r_source_name()))
project_folder <- "Pollution"    # set to  "" if script name to be used
baltimore_fips <- "24510"

log(script_r,"#Coursera Exploratory Data Analysis course project 2")
log(script_r,"#Explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United States")
log(script_r,"#R script: ",script_r,".R"," Author: John Pilger Date: ",date())

log(script_r,"##Retrieve National Emissions Inventory (NEI) and SCC files using downloadfile function ")
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
    
log(script_r,"##Have total emissions from PM2.5 decreased in Baltimore from 1999 to 2008?")
log(script_r,"##The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.")
log(script_r,"##Using the base plotting system, make a plot showing the total PM2.5 emission.")


NEI_baltimore <- NEI[NEI$fips == baltimore_fips,]
log(script_r,"##PM2.5 Baltimore Emissions data rows x cols : ", nrow(NEI_baltimore)," x ", ncol(NEI_baltimore)) 

log(script_r,"##Sum Emissions by year for Baltimore - fips = ",baltimore_fips)
df <- aggregate(Emissions~year, data=NEI_baltimore, sum)

log(script_r,"##Create a barchart showing emissions by year for Baltimore")
print(df)
plot_ds <- plot2(df,project_folder_name,script_r)
log(script_r,"## ",plot_ds," Complete and written to project folder")

} else {
    log(script_r,"Error: Download failed rc= ", rc_down)    
}
