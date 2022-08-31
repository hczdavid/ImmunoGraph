#' @title Make Forest Plot
#' @author Caizhi David Huang; Jianing Li; Ying Zhang
#' @description Support single-panel version and multiple-panel version of forest plots; allow top and bottom blocks in both versions.
#' @param dataset input dataset(s) and one or multiple datasets are allowed. When more than one dataset is provided, all datasets are required to have the same format and to be input using list(). The multi-panel forest plots will be displayed from left to right in accordance to the order of input datasets
#' @param datatype input dataset type. Must be "R" (ratio) or "D" (difference)
#' @param typecol variable in the dataset being displayed in the serotype column
#' @param estcol variables in the dataset being displayed in the within-group estimate columns
#' @param compcol variable in the dataset being displayed in the between-group comparison column
#' @param typelabel column name that being displayed under the header in the serotype column; different column names can be specified to the top and bottom blocks as well as within each block. If multiple dataset has different serotype, a dataframe with "typelabel" and "serotype" is required
#' @param estlabel  column names that being displayed under the header in the within-group estimate columns; they need to match the number of elements specified in estcol
#' @param complabel column name that being displayed under the header in the between-group comparison column
#' @param typehead header of serotype
#' @param esthead headers of the within-group estimates, typically the treatment name
#' @param comphead header of the between-group comparison
#' @param nblock number of blocks within each panel, 1 or 2; if nblock=2, the two blocks will be displayed top and bottom
#' @param topbot index or serotype names for top and bottom blocks; the order of the index or serotype is the order of display
#' @param ifest whether or not to display the within-group estimate columns; default is true
#' @param ifcomp whether or not to display the between-group column; default is true
#' @param xlabel label of x axis followed by "Log10 Scale". e.g. if xlabel="GMT Ratio", the plot will display "GMT Ratio Log10 Scale"
#' @param xsublabel the second line of xlabel
#' @param xticks x ticks for x axis; for multi-panel plot, xticks can be specified with different values for different panels
#' @param outlier outliers for the upper bound and lower bound; when outlier is specified and the plot goes beyond the specified bound (either upper bound or lower bound), the plot will display an arrow instead going beyond the boundary
#' @param refline  refence lines; multiple reference lines are allowed, and the first element of this parameter is considered as the key reference line which could drive a color change of the plot, see ifkey below
#' @param reflinecol  color of the reference line
#' @param reflinelty line type of the reference line
#' @param ifkey if ifkey is true, lower bound of the CI is less than the value of the key reference line, the plot for that serotype will change color
#' @param ifkeycol color when ifkey is true
#' @param pshape shape of the point estimate in the plot
#' @param plotcol color of the plot
#' @param axissize    font size of the ticks of the x-axis
#' @param tablesize   font size of the table contents
#' @param labelsize   font size of the headers and labels
#' @param adjhl horizontal position of horizontal lines in the table
#' @param adjvl horizontal position of the vertical lines in the table
#' @param adjcw column space between the plot and the table and column spaces between different columns within the table
#' @param adjtblprop proportion of the width of the table compared to the width of the plot
#' @param adjbtmprop proportion of the height of the bottom block compared to the height of the top block
#' @param adjtypehead horizonal position of typehead
#' @param res resolution of the output figure
#' @param marg margins of the top and/or bottom block; the same margins will be applied the top and bottom blocks and to each panel in the multi-panel plots; the order of the margin is top, right, bottom, and left
#' @param pwidth width of the output figure
#' @param pheight height of the output figure
#' @param outpath output pathway; if not specified, the generated plot is saved at the current working directory
#' @param outfile file name of the output figure with the extension, eg, filename.png. To embed the figure into an RTF file, it's recommended to use png as extension
#' @import ggplot2
#' @import ggpubr
#' @import grid
#' @import gridExtra
#' @import tidyverse
#' @export
makeForest <- function(

  dataset,
  datatype    = "R",  # need to be "R" ratio, "D" is difference    
  typecol,
  estcol      = NULL,
  compcol,

  typelabel,
  estlabel    = NULL,
  complabel   = NULL,

  typehead    = NULL,
  esthead     = NULL,
  comphead    = NULL,

  nblock      = 1,
  topbot      = list(NULL,NULL),

  ifest       = TRUE,
  ifcomp      = TRUE,

  xlabel      = NULL,
  xsublabel   = NULL,

  #set for multiple panel
  xticks      = NULL, #c(0.1,1,10,100)
  outlier     = NULL,

  refline     = NULL,
  ##############################

  reflinecol     = NULL, # reference line color
  reflinelty     = NULL, # reference line type

  ifkey       = FALSE,
  ifkeycol    = "red", # touch key line color


  pshape      = 20, # point shape
  plotcol     = "black", # CI line color

  axissize    = 9,
  tablesize   = 9,
  labelsize   = 9,

  #parameter about the plot

  adjhl        = list(c(0,0),c(0,0)), #adjust the line
  adjvl        = list(c(0,0),c(0,0)),

  adjcw        = c(0,0), #adjust the size of gap
  adjtblprop   = 0.5,  #adj the size of table
  adjbtmprop   = NULL, # adjust the bottow and top part
  adjtypehead  = -1,  ##control type top and bottom

  res         = 1000, # resolution
  marg        = c(1.5,3,2,1), # margin of the plot
  pwidth      = 6,
  pheight     = 8,
  outpath     = NULL,
  outfile    = "forestplot.png"

){



  if(length(estcol)!=length(estlabel)){
    stop("The estcol and estlabel need to have the same length")
  }

  if(nblock != 1 & nblock!= 2){stop("nblock needs to be 1 or 2")}

  #If once once dataset and it's dataframe, we need list
  if(is.data.frame(dataset)){dataset <- list(dataset)}
  ldata <- length(dataset)

  if(is.null(refline)){
    
    if(datatype == "R"){
      refline <- rep(list(1),nblock*ldata)
    }else{
      refline <- rep(list(0),nblock*ldata)
    }
  }
  if(is.null(outlier)){outlier <- rep(list(NULL),nblock*ldata)}
  if(is.null(xticks)){xticks <- rep(list(NULL),nblock*ldata)}
  if(is.numeric(refline)){refline <- rep(list(refline),nblock*ldata)}
  if(length(ifkey)==1){ifkey <- rep(ifkey,nblock*ldata)}
  if(length(ifkey)==2){ifkey <- rep(ifkey,ldata)}

  if(class(xticks)!="list"){stop("xticks need to be a list")}
  if(class(outlier)!="list"){stop("outlier need to be a list")}

  if(length(xticks)  == nblock){xticks <- rep(xticks,ldata)}
  if(length(outlier) == nblock){outlier <- rep(outlier,ldata)}
  if(length(refline) == nblock){refline <- rep(refline,ldata)}

  if(adjtblprop >= 1 | adjtblprop <= 0 ){
    stop("adjtblprop must be less than 1 and larger than 0")
  }
  if(!is.null(adjbtmprop)){
  if(adjbtmprop >= 1 | adjbtmprop <= 0 ){
    stop("adjbtmprop must be less than 1 and larger than 0")
  }
    }


  if(is.vector(esthead)){esthead <- as.list(esthead)}


  if(nblock == 2 & is.null(topbot[[1]])){
    stop("topbot need to be provided when nblock is 2")
  }

  if(length(xticks)!=ldata*nblock){
    stop("length xticks need to be the length of nblock or length of dataset * nblock")
  }
  if(length(outlier)!=ldata*nblock){
    stop("length outlier need to be the length of nblock or length of dataset * nblock")
  }
  if(length(refline)!=ldata*nblock){
    stop("length refline need to be the length of nblock or length of dataset * nblock")
  }
  if(length(ifkey)!=ldata*nblock){
    stop("length ifkey need to be the length of nblock or length of dataset * nblock or 1")
  }



  if(is.numeric(adjhl)){adjhl <- list(adjhl,adjhl)}
  if(is.numeric(adjvl)){adjvl <- list(adjvl,adjvl)}

  if(datatype !=  "R" & datatype != "D"){
    stop("Datatype need to be 'R'(ratio),or 'D'(difference)")
  }
  #-------------------------------------------------------#
  #remove NA row and check the column name of all datasets#
  #-------------------------------------------------------#
  fdataset <- list()
  allsero <- list()
  ifsame=TRUE #define a index to show if different dataset have the same name


  for(ww in 1:length(dataset)){
    fdataset[[ww]] <- dataset[[ww]][which(dataset[[ww]][[compcol]]!=""),]
    allsero[[ww]] <-  fdataset[[ww]][,typecol]
    if(ww==1){temp <- allsero[[ww]]}else{
      if(nrow(temp)!=nrow(allsero[[ww]])){ifsame=FALSE}else{
        if(prod(temp==allsero[[ww]])==0){ifsame=FALSE}
      }
    }
  }

  if(is.null(topbot[[1]])){topbot[[1]] <- unique(unlist(allsero))}

  if(!ifsame){

    if(!is.data.frame(typelabel)){
      stop("typelabel need to be a dataframe with two columns: serotype and typelabel")
    }
    if(sum(colnames(typelabel)%in%c("serotype","typelabel"))!=2){
      stop("typelabel column names are not 'serotype','typelabel'")
    }
    if(is.numeric(topbot[[1]])){
      stop("Different datasets has different serotype, topbot need to be the serotype name, instead of the index")
    }


    SEROTYPE <- unique(unlist(allsero))
    for(ww in 1:length(fdataset)){
      fdataset[[ww]] <- merge(as.data.frame(SEROTYPE),fdataset[[ww]],by.x="SEROTYPE",by.y=typecol,sort=F,all.x=T)
      fdataset[[ww]] <- merge(as.data.frame(SEROTYPE),fdataset[[ww]],by.x="SEROTYPE",by.y=typecol,sort=F,all.x=T)
    }
    typelabel <- merge(as.data.frame(SEROTYPE),typelabel,by.x="SEROTYPE",by.y="serotype",sort=F,all.x=T)$typelabel

    if(is.null(topbot[[1]])){topbot[[1]] <- SEROTYPE}
    topbot[[1]] <- match(topbot[[1]],SEROTYPE)
    topbot[[2]] <- match(topbot[[2]],SEROTYPE)

  }

  #------------------------------Done with ifsame----------------------------------------------#



  #------Check topbot-------#
  if(!is.numeric(topbot[[1]])){
    topbot[[1]] <- match(topbot[[1]],fdataset[[1]][[typecol]])
    topbot[[2]] <- match(topbot[[2]],fdataset[[1]][[typecol]])
  }


  #-------------------------------------------------------------------------#
  #                        Start to draw the plot                           #
  #-------------------------------------------------------------------------#

  allplot <- list()

  for(ww in 1:ldata){ #loop for datasets

    #all the first dataset has the ylabel
    if(ww==1){ifylabel=TRUE}else{ifylabel=FALSE}

    adjtblprop_new <- adjtblprop/(1-adjtblprop)
    #save for top and bottom
    myplot <- list()

    for(i in 1:nblock){#loop for panel

      datatoplot <- prepare_forest_data(data      = fdataset[[ww]],
                                        which.row = topbot[[i]],
                                        typelabel = typelabel,
                                        typecol   = typecol,
                                        compcol   = compcol,
                                        estcol    =  estcol,
                                        datatype  = datatype)

      #fix a color issue
      datatoplot$bold[which(!is.na(datatoplot$key))] <- "plain"

      #reset the CI color
      datatoplot$linecolour <- plotcol

      wind <- (ww-1)*nblock
      #------Check refline------#
      if(!is.null(refline[[wind+i]])){
        if(datatype=="R"){
        refline[[wind+i]] <- log10(refline[[wind+i]])}else{
          refline[[wind+i]] <- refline[[wind+i]]
        }
        }


      #change color if touch the reference line if key reference line exist.
      if(ifkey[wind+i]){datatoplot$linecolour[which(datatoplot$lci < refline[[wind+i]][1])] <- ifkeycol}

      #---------------------Deal with Outlier----------------------#
      if(!is.null(outlier[[wind+i]])){

        
        if(datatype=="R"){
          outlier[[wind+i]] <- log10(outlier[[wind+i]])}else{
            outlier[[wind+i]] <- outlier[[wind+i]]
          }

        #check the lci with outlier 1 and uci with outlier 2
        datatoplot$overlow <-   datatoplot$lci < outlier[[wind+i]][1]
        datatoplot$overup <-    datatoplot$uci > outlier[[wind+i]][2]
        #change the value of lci and uci
        datatoplot$lci[which(datatoplot$overlow==TRUE)] <- outlier[[wind+i]][1]
        datatoplot$uci[which(datatoplot$overup==TRUE)] <- outlier[[wind+i]][2]

        #change the value of estimate and estimate is it's out side the ourlier line
        datatoplot$estimate[which(datatoplot$estimate < outlier[[wind+i]][1])] <- outlier[[wind+i]][1]
        datatoplot$estimate[which(datatoplot$estimate > outlier[[wind+i]][2])] <- outlier[[wind+i]][2]

        x2side <- outlier[[wind+i]] #set the x2side based on the outlier
      }else{ #if no outlier,set overlow and overup to na
        datatoplot$overlow <- NA
        datatoplot$overup <- NA
      }
      #---------------------Done with Outlier----------------------#



      #set two side of x axis based on the max and min of CI value
      maxUCI <- max(datatoplot$uci,na.rm = T)
      minLCI <- min(c(datatoplot$lci, refline[[wind+i]]),na.rm = T)
      x2side <- c(minLCI,maxUCI)

      if(!is.null(xticks[[wind+i]])){
        
        if(datatype == "R"){
          x2side[2] <- max(maxUCI,log10(xticks[[wind+i]]))
        }else{
        x2side[2] <- max(maxUCI,xticks[[wind+i]])
        }
      }


      #------------deal with ticks---------#
      if(is.null(xticks[[wind+i]])){
        if(datatype == "R"){
        xticks[[wind+i]] <- floor(10^pretty(x2side,4))/2
        }else{
          xticks[[wind+i]] <- floor(pretty(x2side,4))/2
          
        }
      }
      #------------deal with ticks---------#

      if(ww!=1){typehead[i] <- NULL}


      #----plot without table---#
      myplot[[i]] <- make_forest(

        datatoplot = datatoplot,
        marg       = marg,
        datatype   = datatype,

        ifylabel   = ifylabel,
        xlabel     = xlabel,
        xsublabel  = xsublabel,
        pshape     = pshape,
        xticks     = xticks[[wind+i]],
        x2side     = x2side,

        refline    = refline[[wind+i]],
        linecol    = reflinecol,
        linelty    = reflinelty,
        axis_size   = axissize,
        label_size   = labelsize,
        table_size   = tablesize,
        ifest      = ifest,
        ifcomp     = ifcomp,
        adjtblprop       = adjtblprop_new,
        adjtypehead    = adjtypehead[i],
        typehead = typehead[i])
      #----Done without table---#



      #------------------------------------------------------------#
      #                 start to plot table                        #
      #------------------------------------------------------------#

      if(ifest | ifcomp){

        myplot[[i]] <- addtable_forest(fp         = myplot[[i]],
                                       pdata      = datatoplot,

                                       estcol     = estcol,
                                       complabel  = complabel,
                                       estlabel   = estlabel,

                                       esthead    = esthead,
                                       comphead   = comphead,

                                       ifest      = ifest,
                                       ifcomp     = ifcomp,

                                       table_size = tablesize,
                                       label_size  = labelsize,


                                       x2side     = x2side,

                                       adjhl       = adjhl[[i]],
                                       adjg       = adjcw[i],
                                       adjtblprop       = adjtblprop_new,
                                       adjvv      = adjvl[[i]])

      }




    }#end of panel loop
    allplot[[ww]] <- myplot
  }#end of dataset loop

  if(nblock == 2){
    #top and bottow panel
    finalplot_top <- ggplotGrob(allplot[[1]][[1]])
    finalplot_bot <- ggplotGrob(allplot[[1]][[2]])
    #if more than one data set
    if(length(dataset) > 1){for(ww in 1:c(length(dataset)-1)){
      finalplot_top <- cbind(finalplot_top,ggplotGrob(allplot[[ww+1]][[1]]),size="last")
      finalplot_bot <- cbind(finalplot_bot,ggplotGrob(allplot[[ww+1]][[2]]),size="last")
    }
      }

    if(is.null(adjbtmprop)){
      adjbtmprop <- (length(topbot[[2]])+4)/(length(topbot[[1]])+4)
    }
    #save the plot
    png(paste0(outpath,outfile),height = pheight,width = pwidth,units = "in",res = res)
    grid.arrange(finalplot_top,finalplot_bot,nrow=2,heights=c(1-adjbtmprop,adjbtmprop))
    dev.off()
    return(grid.arrange(finalplot_top,finalplot_bot,nrow=2,heights=c(1-adjbtmprop,adjbtmprop)))

  }else{ #only one panel

    finalplot <- ggplotGrob(allplot[[1]][[1]])
    if(length(dataset) >1){for(ww in 1:c(length(dataset)-1)){
      finalplot <- cbind(finalplot,ggplotGrob(allplot[[ww+1]][[1]]))
    }
    }

    if(is.null(outpath)){outpath <- getwd()}

    png(paste0(outpath,outfile),height = pheight,width = pwidth,units = "in",res = res)
    grid.newpage()
    grid.draw(finalplot)
    dev.off()
    return(grid.draw(finalplot))
  }#only one panel end
}
