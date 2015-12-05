library(gdata)

rev_rows = function(dataframe){
  dataframe = dataframe[nrow(dataframe):1,]
  rownames(dataframe) = 1:nrow(dataframe)
  dataframe
}

exclude_invalid_lines = function(data){
  data[data==""] = NA
  data[data==0 ] = NA
  data = na.omit(data)
}

read_mmc_data = function(filename, names=c(), exclude_invalid=T){
  skip_line = 3
  loaded_data = read.xls(filename, skip=skip_line)

  end_line = grep("High", as.character((loaded_data$Session.Date)))
  all_data = loaded_data[-end_line:-(end_line+2),]
  
  if(length(names) > 0){
    for(name in names(all_data)){
      if(!is.element(name, names)){
        all_data[name] <- NULL
      }
    }
  }
  
  if(exclude_invalid){
    valid_data = exclude_invalid_lines(all_data)
  }
  
  return(valid_data)
}
