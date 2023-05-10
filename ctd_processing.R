library(raster)
library(stringr)
library(readtext)
library(openxlsx)
library(ggplot2)
library(DescTools)
library(r2r)

############################################
# Helper functions
############################################
getColumnName <- function(text,colNum){
  exp <- paste0("name ",colNum,"...")
  name_partial <- str_split(text,regex(exp,dotall = TRUE))[[1]][2]
  name_partial <- str_split(name_partial, regex(": ",dotall=TRUE))[[1]][2]
  name <- str_split(name_partial, regex("\n",dotall=TRUE))[[1]][1]
  name
}

getColumnNameDataFrame <- function(text,colNum){
  exp <- paste0("name ",colNum,"...")
  name_partial <- str_split(header$text,regex(exp,dotall = TRUE))[[1]][2]
  name2 <- str_split(name_partial, regex(": ",dotall=TRUE))[[1]][1]
  name <- str_split(name2, regex("/",dotall=TRUE))[[1]][1]
  name
}

# false is lake erie, true if its saginaw bay/lake huron
useSaginawBaySpecifics <- function(lake_name, sitename, site_shortname){
  is_sagbay <- FALSE
  if(!(tolower(lake_name) %like% "erie") & 
     (tolower(site_shortname) %like% "hu" 
      | tolower(site_shortname) %like% "sb" 
      | tolower(sitename) %like% "hu" 
      | tolower(sitename) %like% "sb" 
      | tolower(sitename) %like% "sagbay"
      | tolower(site_shortname) %like% "sagbay"
      | tolower(lake_name) %like% "huron")
     | tolower(lake_name) %like% "sagbay"){
    is_sagbay <- TRUE
  }
  
  is_sagbay
}
############################################


############################################
# Initial configuration from user input
############################################
global_params <- read.csv("global_parameters.csv")
station_details <- read.csv("station_details.csv")

# take user input for working directory
wd <- global_params["input_folder"][[1]]

# take user input for output directory
out_dir <- global_params["output_folder"][[1]]
setwd(out_dir)
dir.create("OUTPUT")

setwd(wd)

# set the name of the lake, vessel, and sampling program
lake_name <- global_params["lake"][[1]]
vessel_name <- global_params["vessel"][[1]]
program_name <- global_params["program"][[1]]
############################################

# initialize the mapping between the station details and the files
stations <- hashmap()
for(i in 1:length(station_details[[1]])){
  stations[[ as.numeric(station_details$file_ends_with[i]) ]] <- i
}

# read in the files in the input directory
files <- list.files(pattern=".asc")

# create summary excel sheet
wb <- createWorkbook()

# process each CTD file
for(file in files){
  # make sure we are back in the input directory
  setwd(wd)
  
  # print out the filename we are processing
  print(paste0("Processing file ", file,"... "))
  
  # read the data from the file
  data <- read.table(file,header=FALSE)
  
  station_order <- str_split(str_split(file, "[.]")[[1]][1], "_")[[1]]
  station_order <- as.numeric(station_order[length(station_order)])
  station_index <- stations[[station_order]]
  if(is.null(station_index)){
    print(paste0("No station associated with file ",file,". If you believe this is an error, check configuration in station_details.csv. Skipping..."))
    next
  }
  
  # print a preview of the file
  print("File preview... ")
  print(head(data))
  
  # parse the file name and create the name of the header file
  filename <- str_split(file, "[.]")[[1]][1]
  header <- readtext(paste0(filename,".hdr"))
  
  # get the start time of the cast
  start_time_partial <- str_split(header$text,regex("start_time...",dotall = TRUE))[[1]][2]
  start_time <- str_split(start_time_partial, regex("..Instrument's time stamp, header.",dotall=TRUE))[[1]][1]
  
  # get the names of the columns from the header file
  col_names <- c()
  name_count <- str_split(header$text,regex(" name [0-9]"))
  for (i in 1:(length(name_count[[1]]) - 1)){
    col_names <- append(col_names, getColumnNameDataFrame(header$text,i-1))
  }
  
  # assign the column names to the data
  colnames(data) <- col_names
  
  # parse the filename to extract the name of the sensor
  split_filename <- str_split(filename,"_")
  sensorname <- split_filename[[1]][1]
  file_sensorname <- sensorname
  if (sensorname == "SBE19plus") {
    sensorname = "SBE 19plus"
    file_sensorname = "CTD"
  }
  
  # get components of date from start_time to convert to UTC, and output it
  date_formatted <- as.POSIXct(start_time,format="%b %d %Y %H:%M:%OS")
  date <- str_remove_all(str_split(date_formatted," ")[[1]][1],"-")
  
  display_time <- str_split(date_formatted," ")[[1]][2]
  display_date <- str_split(start_time,regex(" "))[[1]]
  
  # site associated with the file (e.g. WE2): or skip to skip and process the next site
  sitename <- station_details$station[station_index]
  
  # if the user typed skip, do not process this file
  if (sitename == "skip") {
    print(paste0("Skipping file ",file," ..."))
    next
  }
  
  # read the site shortname from details file (e.g. will be 'we' for 'we8')
  site_shortname <- station_details$shortname[station_index]
  
  # determine if this sampling site is in saginaw bay
  is_sagbay <- useSaginawBaySpecifics(lake_name, sitename, site_shortname)
  
  # If surface sample only, then 1. If surface and depth sample, then 2
  sample_depth_measured <- station_details$surface_depth[station_index]
  
  # create first several rows of output csv
  output_indv_csvs <- data.frame(
    variable = c("Date","Lake","Site Name","Program","Vessel","Sensor",""),
    value = c(paste0(display_date[2],"-",display_date[1],"-",display_date[3]),lake_name,sitename, program_name, vessel_name, sensorname,""),
    time = c(display_time,"","","","","",""),
    tz = c("UTC","","","","","","")
  )
  
  # write to csv file
  setwd(paste0(out_dir,"/OUTPUT"))
  output_filename = paste0(date,"_",program_name,"_",file_sensorname,"_",sitename,".csv")
  
  # for each column name, bind them together into one data structure
  output_col_names <- NULL
  for (i in 1:(length(name_count[[1]]) - 2)){
    output_col_names <- cbind(output_col_names, getColumnName(header$text,i-1))
  }
  
  # actually write the data to the individual output csv files
  write.table(output_indv_csvs, output_filename, col.names=FALSE, sep=",")
  write.table(output_col_names, output_filename, col.names=FALSE, sep=",", append=TRUE)
  write.table(data[,1:(length(name_count[[1]]) - 2)], output_filename, col.names=FALSE, sep=",", append=TRUE)
  
  # then process the .asc file and add to the summary file as a new sheet
  setwd(paste0(out_dir))
  
  max_depth <- max(data$depFM)
  sample_depth <- max_depth - 0.5
  
  # always use 3m for the sample depth of a bottom sample for Sag Bay
  if(is_sagbay & as.numeric(sample_depth_measured) == 2){
    sample_depth <- 3
  }
  
  # calculate the min and max sample depths
  sample_min <- sample_depth - 0.25
  sample_max <- sample_depth + 0.25
  
  # find which rows have depth values less than 0 and begin processing after those rows
  start_index <- max(which(data$depFM < 0)) + 1
  subset_data <- data[start_index:nrow(data),]
  
  # find 0.5-1m depth range
  depth_range <- which(subset_data$depFM >= 0.5 & subset_data$depFM <= 1)
  sample_depth_range <- which(subset_data$depFM >= sample_min & subset_data$depFM <= sample_max)
  
  # create first several rows of output csv
  addWorksheet(wb, sitename)
  
  row1 <- data.frame(sitename,
                    "",
                    display_time,
                    paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename),
                    0.435, # Constant!
                    "","","","","","","",
                    "max depth",
                    max_depth)
  
  writeData(wb, sitename, row1, startRow = 1, startCol = 1,colNames = FALSE)
  
  # if this is a station with sample depth measurements, add these values in the sheet
  if(as.numeric(sample_depth_measured) == 2) {
    row3 <- data.frame("0.5-1m")
  
    for (i in 2:(length(name_count[[1]]) - 2)){
      mean_value <- mean(subset_data[min(depth_range):max(depth_range),i])
      row3<-cbind(row3, mean_value)
    }
    row3 <- cbind(row3, "")
    row3 <- cbind(row3, "sample depth")
    row3 <- cbind(row3, "n-0.25")
    row3 <- cbind(row3, "n+0.25")
  
    writeData(wb, sitename, row3, startRow = 3, startCol = 1,colNames = FALSE)
  
    row4 <- data.frame("sample depth +/-0.25m")
    if (is_sagbay){
      row4 <- data.frame("2.75-3.25m")
    }
  
    for (i in 2:(length(name_count[[1]]) - 2)){
      mean_value <- mean(subset_data[min(sample_depth_range):max(sample_depth_range),i])
      row4<-cbind(row4, mean_value)
    }
    row4 <- cbind(row4, "")
    row4 <- cbind(row4, sample_depth)
    row4 <- cbind(row4, sample_min)
    row4 <- cbind(row4, sample_max)
    
    writeData(wb, sitename, row4, startRow = 4, startCol = 1,colNames = FALSE)
  }
  else {
    row3 <- data.frame("0.5-1m")
    
    for (i in 2:(length(name_count[[1]]) - 2)){
      mean_value <- mean(subset_data[min(depth_range):max(depth_range),i])
      row3<-cbind(row3, mean_value)
    }
    
    writeData(wb, sitename, row3, startRow = 3, startCol = 1,colNames = FALSE)
  }
  
  # write the PAR depth column and attach to the subset data
  par_depth <- ifelse((subset_data$depFM - 0.435) < 0, "",(subset_data$depFM - 0.435))
  writable_data <- cbind(subset_data[,1:(length(name_count[[1]]) - 2)],par_depth)
  
  row6 <- NULL
  row7 <- NULL
  
  # check for each variable before writing to sumary sheet. 
  # Will break if hdr file short names change
  if(length(grep("depFM",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Depth")
    row7 <- cbind(row7,"(m)")
  }
  
  if(length(grep("tv290C",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Temperature")
    row7 <- cbind(row7,"(\u00B0C)")
    
    # plot depth x temp
    temp_plot <- ggplot(writable_data, aes(tv290C, depFM)) + 
      geom_point() + 
      theme_bw() + 
      scale_x_continuous(position = "top") +
      ggtitle(paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename)) +
      labs(x="Temperature (\u00B0C)", y="Depth (m)") +
      scale_y_reverse() 
    print(temp_plot)
    insertPlot(wb,sitename,startRow = 8, startCol = 13)
  }
  if(length(grep("c0uS",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Cond")
    row7 <- cbind(row7,"(\u00B5S/cm)")
  }
  if(length(grep("specc",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "SpCond")
    row7 <- cbind(row7,"(\u00B5S/cm)")
  }
  if(length(grep("par",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "PAR")
    row7 <- cbind(row7,"(\u00B5E/m2/s)")
    
    # plot par depth x par
    par_plot <- ggplot(writable_data, aes(par, as.numeric(par_depth))) + 
      geom_point() + 
      theme_bw() + 
      scale_x_continuous(position = "top") +
      ggtitle(paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename)) +
      labs(x="PAR", y="Depth (m)") +
      scale_y_reverse() 
    print(par_plot)
    insertPlot(wb,sitename,startRow = 48, startCol = 13)
    
  }
  if(length(grep("sbeox0Mg",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Oxygen")
    row7 <- cbind(row7,"(mg/L)")
    
    # plot depth x dissolved oxygen
    do_plot <- ggplot(writable_data, aes(sbeox0Mg, depFM)) + 
      geom_point() + 
      theme_bw() + 
      scale_x_continuous(position = "top") +
      ggtitle(paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename)) +
      labs(x="DO (mg/L)", y="Depth (m)") +
      scale_y_reverse() 
    
    print(do_plot)
    insertPlot(wb,sitename,startRow = 8, startCol = 20)
  }
  if(length(grep("sbeox0PS",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Oxygen")
    row7 <- cbind(row7,"(%)")
  }
  if(length(grep("CStarAt0",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Atten.")
    row7 <- cbind(row7,"(m-1)")
  }
  if(length(grep("CStarTr0",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Trans.")
    row7 <- cbind(row7,"(%)")
    
    # plot depth x trans.
    trans_plot <- ggplot(writable_data, aes(CStarTr0, depFM)) + 
      geom_point() + 
      theme_bw() + 
      scale_x_continuous(position = "top") +
      ggtitle(paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename)) +
      labs(x="% Trans", y="Depth (m)") +
      scale_y_reverse() 
    
    print(trans_plot)
    insertPlot(wb,sitename,startRow = 48, startCol = 20)
  }
  if(length(grep("chloroflTC0",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "chl a")
    row7 <- cbind(row7,"(\u00B5g/L)")
    
    # plot depth x chl a
    chla_plot <- ggplot(writable_data, aes(chloroflTC0, depFM)) + 
      geom_point() + 
      theme_bw() + 
      scale_x_continuous(position = "top") +
      ggtitle(paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename)) +
      labs(x="CHLa (\u00B5g/L)", y="Depth (m)") +
      scale_y_reverse() 
    
    print(chla_plot)
    insertPlot(wb,sitename,startRow = 28, startCol = 13)
  }
  if(length(grep("phycyflTC0",colnames(writable_data),value=FALSE)) > 0){
    row6 <- cbind(row6, "Phycocyanin")
    row7 <- cbind(row7,"(RFU)")
    
    # plot depth x phycocyanin
    pc_plot <- ggplot(writable_data, aes(phycyflTC0, depFM)) + 
      geom_point() + 
      theme_bw() + 
      scale_x_continuous(position = "top") +
      ggtitle(paste0(display_date[1]," ",display_date[2],", ",display_date[3],": ", sitename)) +
      labs(x="PC (RFU)", y="Depth (m)") +
      scale_y_reverse() 
    
    print(pc_plot)
    insertPlot(wb,sitename,startRow = 28, startCol = 20)
  }
  
  row6 <- cbind(row6, "PAR depth")
  row7 <- cbind(row7, "(m)")
  
  # write the column names and units
  writeData(wb, sitename, row6, startRow = 6, startCol = 1,colNames = FALSE)
  writeData(wb, sitename, row7, startRow = 7, startCol = 1,colNames = FALSE)
  
  #write the actual data
  writeData(wb, sitename, writable_data, startRow = 8, startCol = 1,colNames = FALSE)
  
  # save the final output to excel summary sheet
  summary_output_filename = paste0(date,"_",site_shortname,"_",program_name,"_Summary",".xlsx")
  saveWorkbook(wb, file = summary_output_filename, overwrite = TRUE)
}
