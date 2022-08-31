#' @title calculate the RCDF
#' @keywords internal
#' @param dataset
#' @param keyvar a variable
#' @param byvar a vector a variable
#' @description this is the support function for MakeRCDC function
#' @return a list of dataframe. list length depend on the byvar

calRCDF <- function(
  
  dataset,
  keyvar  = "AVAL",
  byvar   = NULL,
  connectsign = connectsign
  
){
  # remove NA value
  dataset <- dataset[!is.na(dataset[[keyvar]]),]
  
  if(is.null(byvar)){
    # sort by th keyvar
    counts          <- table(dataset[[keyvar]])  # get the counts
    key_var         <- as.numeric(names(counts)) # get the key_var at log10 scale
    key_var_log10   <- log10(key_var)
    counts          <- as.vector(counts)         # get the counts vector
    percent         <- counts/sum(counts)        # get the percentage
    alln            <- length(percent)           # get the number of value
    revperc         <- c()
    for (ii in 1:alln){revperc[ii] <- sum(percent[ii:alln])}
    vartable <- data.frame(key_var,
                           key_var_log10,
                           counts,
                           percent,
                           revperc,
                           trt=rep(dataset$TRT01A[1],alln))
    return(vartable)
  }else{
    
    allvar <- list()
    for (i in 1:length(byvar)) {allvar[[i]] <- names(table(dataset[[byvar[i]]]))}
    
    #all combination of byvar
    allcom           <- expand.grid(allvar)
    colnames(allcom) <- byvar
    
    alltable <- list()
    for (jj in 1:nrow(allcom)) {
      
      subdata <- dataset
      trtname <- allcom[jj,1]
      
      for (kk in 1:ncol(allcom)){
        subdata <- subdata[subdata[[byvar[kk]]]==allcom[jj,kk],]
        if(kk < ncol(allcom)){trtname <- paste(trtname,allcom[jj,kk+1],sep = connectsign)}
      }
      
      counts          <- table(subdata[[keyvar]])  # get the counts
      key_var         <- as.numeric(names(counts)) # get the key_var at log10 scale
      key_var_log10   <- log10(key_var)
      counts          <- as.vector(counts)         # get the counts vector
      percent         <- counts/sum(counts)        # get the percentage
      alln            <- length(percent)           # get the number of value
      revperc         <- c()
      for (ii in 1:alln){revperc[ii] <- sum(percent[ii:alln])}
      alltable[[jj]]  <- data.frame(key_var,
                                    key_var_log10,
                                    counts,
                                    percent,
                                    revperc,
                                    trt=rep(trtname,alln))
    }# end for loop
    return(alltable)
  }# end else
}# end function

