#' @title prepare_forest_data
#' @description function use to prepare the data for make_forest function
#' @keywords internal
#' @param data the data input from the sas file
#' @param which.row a vector to select which row to use in the plot
#' @param heading the head name of each serotype
#' @param key_col serotype column name
#' @param plot_col ratio difference column name
#' @param table_col column names what used to display in the table
#' @keywords internal
prepare_forest_data <- function(
  
  data=NULL,
  which.row=NULL,
  typelabel=NULL,
  typecol=NULL,
  compcol=NULL,
  estcol=NULL,
  datatype=NULL
  
  
){
  
  #Add heading column to the data
  data$heading <- typelabel
  
  #if RATIO column is NA, then set to (,)
  data[[compcol]][which(is.na(data[[compcol]]))] <- "(,)"
  
  #change the ratio columm to three numeric column for ploting
  myplotdata <- data[[compcol]] %>%
    as.matrix()  %>%
    as.character()  %>%
    strsplit(split = "[(,)]") %>%
    sapply(function(x){as.numeric(unlist(x))}) %>%
    t %>%
    as.data.frame
  
  colnames(myplotdata) <- c("FC","LCI","UCI")
  
  #combine the myplotdata to mydata
  data <- cbind(data,myplotdata)
  
  #fillter the data by row
  data <- data[which.row,]
  nr <- nrow(data)
  
  #prepare for the headings (due to the need for make_forest_data function)
  # headings <- data.frame(key_col = data[,key_col],heading1=rep(typehead,nr),
  #                        heading2=data[,"heading"],heading3=data[,key_col])
  headings <- data.frame(typecol  = data[,typecol],
                         heading1 = data[,"heading"],
                         heading2 = data[,typecol],
                         heading3 = rep(NA,nr))
  
  
  colnames(headings)[1] <- typecol
  
  #use the make_forest_data function to prepare the data
  datatoplot <- make_forest_data(headings     = headings,
                                 rows         = unique(headings[,2]),
                                 cols         = list(data),
                                 exponentiat  = F,
                                 col.key      = typecol,
                                 ci.delim     = ", ",
                                 col.estimate = "FC",
                                 col.lci      = "LCI",
                                 col.uci      = "UCI",
                                 col.right    = c(estcol,compcol),
                                 blankrows    = c(0, 0, 0, 0),
                                 scalepoints  = F)
  
  if(datatype == "R"){
  datatoplot$estimate <- log10(datatoplot$estimate)
  datatoplot$lci      <- log10(datatoplot$lci)
  datatoplot$uci      <- log10(datatoplot$uci)
  }
  
  return(datatoplot)
}
