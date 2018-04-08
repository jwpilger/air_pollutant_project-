downloadfile <- function(Url_download,script_name,proj_folder) 
{
    rc_df <- data.frame()
    rc_df[1,1] <- 99 # error return code
    rc_df[1,2] <- "" # project folder
    rc_df[1,3] <- 0 # downloaded, unzipped file count
    rc_df[1,4] <- "" # first file
    colnames(rc_df) <- c("RC","Project Folder","FileCnt","File#1")
    
    zip_file <- paste(proj_folder,"-download",sep="")
    folder_path <- paste("./",proj_folder,sep="")
    unzip_file <- file.path(folder_path,paste(proj_folder,"-data",sep=""))
    download_zipfile <- file.path(folder_path,paste(zip_file,".zip",sep=""))
    
    log(script_name,"##Using working directory path: ",getwd())
    # Create folder for project, downloads, zipfiles, codebook, etc.
    if(!file.exists(folder_path))
    {
        if (!dir.create(folder_path)) {
            log(script_name,"could not create project file folder: ",folder_path)  
            return(rc_df)
        }
        log(script_name,"##Project file folder created: ",folder_path)
      
    }
    rc_df[1,2] <- folder_path
    log(script_name,"##Using project file folder: ",folder_path)
    
    if(!file.exists(download_zipfile)){   # download .zip file if not exists
        log(script_name,"downloading file: ",Url_download)
        start.time = Sys.time()
        rc <- download.file(Url_download,destfile=download_zipfile,mode = "curl")
        if (rc == 0) {
            log(script_name,"##Download Complete Duration: ",(Sys.time() - start.time)," secs ") 
            log(script_name,"##File size (MB): ",file.size(download_zipfile)/(1024*1024))
        } else { 
            log(script_name,"Download failed return code: ",rc) 
            rc_df[1,1] <- rc
            return(rc_df)   
        }
        log(script_name,"writing downloaded file: ",download_zipfile) 
    } else {
        log(script_name,"file already downloaded: ",download_zipfile)
    }
    ext <- tools::file_ext(basename(download_zipfile))
    if (ext == "zip") {
        # unzip the file 
        if(!file.exists(unzip_file)){   # has the file be unzipped and exists?
            unzip(download_zipfile, exdir=unzip_file, overwrite=TRUE)
            log(script_name,"##Zip file extracted to folder: ",unzip_file)
        } else {
            log(script_name,"##Zip file already extracted to folder: ",unzip_file)
        }
        file_list<-list.files(unzip_file, recursive=TRUE) # get the files from the zip folder
        folder_cnt <- length(file_list)
        if (folder_cnt == 0) { 
            message("Folder ",folder_path," file count = 0 no files to process"); 
            return(rc_df)  
        }
        
        if (folder_cnt >= 1) {
            sort(file_list) # sort filenames and store in return DF
            rc_df[1,4] <- file.path(unzip_file,file_list[1])
            rc_df[1,5] <- file.path(unzip_file,file_list[2])
            rc_df[1,6] <- file.path(unzip_file,file_list[3])
            log(script_name,"##Processing files in zip folder: ",folder_path)
            rc_df[1] <- 0
            rc_df[3] <- folder_cnt
            log(script_name,"##Zip file folder count: ",folder_cnt)
        }
    } else { # not a zip file 
        rc_df[1,1] <- 0
        rc_df[3] <- 1
        rc_df[1,4] <- download_zipfile
    }        
    # ready to process file_name
 
    return(rc_df) 
}




# find the running R script name
r_source_name <- function () 
{
    frame_files <- lapply(sys.frames(), function(x) x$ofile)
    frame_files <- Filter(Negate(is.null), frame_files)
    frame_files[[length(frame_files)]]
}
log <- function(script_name,...) 
{
    cat("[",script_name,"] ", ..., "\n", sep="")
#   cat("[",file=paste(script_name,"_Codebook.md",sept=""),...,"\n", sep="",append=TRUE)
#   final run goes to codebook file for documentation
}

