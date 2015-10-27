#Read in fits table, make data frame 
#Depends on FITSio library

mrdfits <- function(file, nullvals) {
  library(FITSio)
  tmp <- file(description = file, open = "rb")
  header0 <- readFITSheader(tmp)
  header <- readFITSheader(tmp)
  datain <- readFITSbintable(tmp, header)
  close(tmp)
  names <- tolower(datain$colNames)
  dataout <- as.data.frame(matrix(0, ncol = length(names), 
                                  nrow = length(datain$col[[1]])))
  if(missing(nullvals)) {
    for (i in 1:length(names)) {
      names(dataout)[i] <- names[i]
      dataout[[names[i]]] <- datain$col[[i]]
    } 
  } else {
    for (i in 1:length(names)) {
      names(dataout)[i] <- names[i]
      dataout[[names[i]]] <- datain$col[[i]]
      dataout[[names[i]]][dataout[[names[i]]] == nullvals] <- NA
    }
  }  
    return(dataout)
}