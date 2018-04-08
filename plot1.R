
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for 
#each of the years 1999, 2002, 2005, and 2008.


plot1 <- function(df1,project_folder,script_n) {
    plot_file <- file.path(project_folder,paste(script_n,".png",sep=""))
    png(file=plot_file,width=480, height=480,units="px")
    barplot(df1$Emissions/1000,
            xlab = 'Years',
            ylab = 'Total Emissions Tons (thousands)',
            main = 'US PM2.5 Emissions by Year',
            names.arg = df1$year,
            col = 'mistyrose')
    dev.off()   # close graphics device
    return(plot_file)
}




# define required libs and source files
library(RCurl)
library(dplyr)
library(data.table)
source("downloadfile.R")


Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
script_r <-  tools::file_path_sans_ext(basename(r_source_name()))
project_folder <- "Pollution"    # set to  "" if script name to be used


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
    

log(script_r,"##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?")
log(script_r,"##The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.")
log(script_r,"##Using the base plotting system, make a plot showing the total PM2.5 emission.")

log(script_r,"##Sum Emissions by year")
df <- aggregate(Emissions~year, data=NEI, sum)

log(script_r,"##Create a barchart showing emissions by year delta")
    print(df)
    plot_ds <- plot1(df,project_folder_name,script_r)

log(script_r,"## ",plot_ds," Complete and written to project folder")


} else {
    log(script_r,"Error: Download failed rc= ", rc_down)    
}
