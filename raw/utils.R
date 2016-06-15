
###############################

read_bil_file = function(filename,nrow,ncol) {
  
  fid = file(filename,"rb")
  
  data = readBin(fid, n = nrow*ncol, what= integer(), size = 2, signed=FALSE, endian = "little")
  
  close(fid)
  
  return(data)
  
}


###############################

read_index_file = function(filename) {
  
  fid = file(filename)
  
  data <- readLines(fid)
  
  close(fid)
  
  data <- strsplit(data,split=" ")
  
  wsh_index <- vector("list", length(data))
  drainage_basin_key <- vector("integer", length(data))
  
  for (iwsh in 1:length(data)) {
    
    wsh_index[[iwsh]] <- as.integer(data[[iwsh]][5:length(data[[iwsh]])])
    
    drainage_basin_key[iwsh] <- data[[iwsh]][4]
    
  }
  
  names(wsh_index) <- drainage_basin_key
  
  return(wsh_index)
  
}


###############################

read_stat_file = function(filename) {
  
  # Read station metadata
  
  metadata = read.csv(filename,header=TRUE,sep = ";",dec = ",",stringsAsFactors = FALSE)
  
  # Remove duplicated stations
  
  idup = duplicated(metadata[,1:3])
  
  metadata = metadata[!idup,]
  
  # Add station name
  
  metadata$station  = paste(metadata$regine_area,metadata$main_no,sep=".")
  
  return(metadata)
  
}







