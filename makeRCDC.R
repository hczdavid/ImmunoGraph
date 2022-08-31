

#' @title MakeRCDC
#' @description A function to make the RCDC plot
#' @param dataset input dataset in the dataframe format
#' @param keyvar  variable used to draw RCDC
#' @param PARAMCD a vector of the name(s) of the serotype(s) for RCDC plots; length of the vector equal to the number of RCDC plots/panels 
#' @param TRT01A a vector of treatment name(s) in the TRT01A column
#' @param TRT01Alabel label of TRT01A if it's not the same as TRT01A
#' @param ABLFL indicate if drawn by ABLFL in each RCDC plot
#' @param ABLFLlabel label of ABLFL if ABLFL is "Y"
#' @param byvar   other subgroup variable besides TRT01A and ABLFL used to draw RCDC in each plot
#' @param byvarlabel  label(s) of byvar(s). If length(byvar) > 1, byvarlabel needs to be a list
#' @param AVISITN  visit(s) used to draw RCDC in each plot; when more than one RCDC plots to be drawn, AVISITN can be specified separately for each plot using a list
#' @param AVISITNlabel label of the AVISITN
#' @param PPROTRFL indicate if PPROTRFL is used as filter variable
#' @param ANL01FL indicate if ANL01FL is used as filter variable
#' @param filtervar other filter variable(s) besides PPROTRFL and ANL01FL
#' @param filtercriteria filter criteria corresponding to the filtervar; need be a list
#' @param RCDCorder order of RCDC in each plot, which can be used to adjust the order of the legend 
#' @param linetype line type of the RCDC 
#' @param linecol line color of the RCDC 
#' @param ref reference lines; when more than one RCDC plot, ref can be specified differently for each plot using a vector
#' @param refcol color of reference lines
#' @param refvalue indicate if showing the value of reference lines
#' @param xlabel  label of the x axis; the label is followd by "Log10 scale"
#' @param ylabel label of the y axis
#' @param header headers of each RCDC plot; when more than one RCDC plots, headers can be specified differently for RCDC plots using a vector
#' @param xticks x ticks for x axis
#' @param yticks y ticks for y axis
#' @param legendsize size of the legend
#' @param headersize size of the header
#' @param axissize size of axis ticks
#' @param ncolumn indicate the number of the columns of the RCDC plot; used to adjust the arrangement of the plots 
#' @param comaxis indicate if same x axis is used in each plot; default is TRUE
#' @param connectsign specify the sign to connect subgroups in the legend
#' @param pwidth  width of the output figure
#' @param pheight  height of the output figure
#' @param res  resolution of the output figure
#' @param outpath output path; if not specified, the generated figure is saved at the current working directory
#' @param outfile file name of the output figure with the extension,eg, filename.png
#' @export
makeRCDC <- function(
  
  dataset           ,
  keyvar            = "AVAL",
  PARAMCD           , # REQUIRED can be a vector
  
  # by variable
  TRT01A            ,
  TRT01Alabel       = NULL,
  ABLFL             = NULL, 
  ABLFLlabel        = NULL, # c("", Y)
  byvar             = NULL, # can be null or a vector
  byvarlabel        = NULL, # need to be a list 
  
  # filter variable
  AVISITN           , # PARAMACD specific
  AVISITNlabel      , # PARAMACD specific
  
  PPROTRFL          = "Y",
  ANL01FL           = "Y",
  filtervar         = NULL,
  filtercriteria    = NULL,
  
  RCDCorder         = NULL,
  linetype          = NULL,
  linecol           = NULL,
  
  
  ref               = NULL,  # PARAMACD specific
  refcol            = "red", 
  refvalue          = FALSE, 
  
  xlabel            = NULL,
  ylabel            = NULL,
  header            = NULL,
  xticks            = NULL,
  yticks            = NULL,
  
  legendsize        = 10,
  headersize        = 10,
  axissize          = 10,
  ncolumn           = NULL,
  comaxis           = TRUE,
  connectsign           = " + ",
  
  
  pwidth            = 6,
  pheight           = 5,
  res               = 1000,
  outpath           = NULL,
  outfile          = "RCDC.png"
  
  
){
  
  #----------------Do filtering-------------#
  allfilter     <- c()
  allcriteria   <- list()
  if(!is.null(PPROTRFL)){
    allfilter   <- c(allfilter,"PPROTRFL")
    allcriteria <- append(allcriteria,as.list(PPROTRFL))
  }
  if(!is.null(ANL01FL)){
    allfilter   <- c(allfilter,"ANL01FL")
    allcriteria <- append(allcriteria,as.list(ANL01FL))
  }
  
  if(TRT01A[1]!="Y"){
    allfilter   <- c(allfilter,"TRT01A")
    allcriteria <- append(allcriteria,list(TRT01A))
  }
  
  if(!is.null(filtervar)){
    allfilter   <- c(allfilter,filtervar)
    allcriteria <- append(allcriteria,filtercriteria)
  }
  
  
  for (i in 1:length(allfilter)){
    fdataset    <- dataset[dataset[[allfilter[i]]]%in%allcriteria[[i]],]
  }
  
  #----------------Done filtering-------------#
  
  if(is.numeric(AVISITN)){AVISITN <- rep(list(AVISITN),length(PARAMCD))}
  
  
  finaltable <- c()
  
  for(i in 1:length(PARAMCD)){
    
    ffdataset <- fdataset[fdataset$PARAMCD==PARAMCD[i],]
    ffdataset <- ffdataset[ffdataset[["AVISITN"]]%in%AVISITN[[i]],]
    
    allbyvar <- c()
    if(length(TRT01A)>1 | TRT01A[1] == "Y"){
      allbyvar <- "TRT01A"
      if(!is.null(TRT01Alabel)){
        for(kk in 1:length(TRT01Alabel)){
          ffdataset$TRT01A[ffdataset$TRT01A==TRT01A[kk]] <- TRT01Alabel[kk]
        }
      }
    }
    
    if(!is.null(ABLFL)){
      allbyvar <- c(allbyvar,"ABLFL")
      if(!is.null(ABLFLlabel)){
        ffdataset$ABLFL[ffdataset$ABLFL=="Y"] <- ABLFLlabel[2]
        ffdataset$ABLFL[ffdataset$ABLFL==""]  <- ABLFLlabel[1]
      }
    }
    
    if(!is.null(byvar)){
      allbyvar <- c(allbyvar,byvar)
      if(!is.null(byvarlabel)){
        if(is.vector(byvarlabel)){byvarlabel <- list(byvarlabel)}
        
        for(kk in 1:length(byvar)){
          oldvar <- names(table(ffdataset[[byvar[kk]]]))
          for(jj in 1:length(oldvar)){
            ffdataset[[byvar[kk]]][ffdataset[[byvar[kk]]]==oldvar[jj]] <- byvarlabel[[kk]][jj]
          }
        }
      }
    }
    
    if(!is.null(AVISITNlabel)){
      for(kk in 1:length(AVISITNlabel)){
        ffdataset$AVISITN[ffdataset$AVISITN==kk] <- AVISITNlabel[kk]
      }
    }
    
    
    
    alltable <- calRCDF(ffdataset,keyvar=keyvar,byvar=allbyvar,connectsign = connectsign)
    
    #if(is.null(xlabel))(xlabel <- expression("Concentration,\u00B5g/mL"~ paste(mu,"g/mL") ~ 'Log'["10"]~"scale"))
    if(is.data.frame(alltable)){alltable <- list(alltable)}
    if(length(alltable)>1 ){
      for(jj in 1:(length(alltable)-1)){alltable[[1]] <- rbind(alltable[[1]],alltable[[jj+1]]) }
    }
    
    
    if(is.null(RCDCorder)){RCDCorder  <- 1:length(unique(alltable[[1]]$trt))}
    
    alltrt            <- unique(alltable[[1]]$trt)[RCDCorder]
    alltable[[1]]$trt <- factor(alltable[[1]]$trt,levels = alltrt)
    
    alltable[[1]]$head <- header[i]
    finaltable         <- rbind(finaltable, alltable[[1]])
  }# end for loop
  
  if(!is.null(xlabel)){xlabel <- bquote(.(xlabel)~"Log"["10"]~"scale")
  }else{
    xlabel <- expression("Concentration, \u00B5g/mL"~'Log'["10"]~"scale")
  }
  
  if(is.null(ylabel))(ylabel <- "Percent \u2265 concentartion")
  
  if(is.null(xticks)){xticks = c(0.1,1,10,100,1000,10000,100000)}
  if(is.null(yticks)){yticks = seq(0,100,by=10)}
  
  if(comaxis == FALSE){myscale = "free_x"}else{myscale = "fixed"}
  
  allfigure <- ggplot(data = finaltable,aes(x=key_var_log10,y=revperc*100)) +
    geom_step(aes(color=trt,linetype=trt),direction = "vh") +
    ylab(ylabel) +
    xlab(xlabel) +
    labs(col="",linetype="") +
    scale_x_continuous(labels = xticks,breaks = log10(xticks)) +
    scale_y_continuous(breaks = yticks) +
    coord_cartesian(clip = "off") +
    facet_wrap(.~ head,ncol=ncolumn,scales = myscale) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major.y = element_line(size=0.5,linetype = "solid",colour = "gray90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background =element_rect(fill=NA,colour = "black",size = 1),
          strip.text = element_text(size=headersize,colour = "black"),
          legend.position = "bottom",
          legend.text = element_text(size=legendsize),
          panel.border = element_rect(color="black",fill = NA),
          axis.text=element_text(size=axissize),
          axis.title=element_text(size=axissize))
  
  
  if(!is.null(ref)){
    reflab    <- ref
    ref     <- log10(ref)
    refdata <- data.frame(head = header,ref = ref,reflab=reflab)
    allfigure <-  allfigure +
      geom_segment(data = refdata,mapping = aes(x=ref, xend=ref, y=Inf, yend=-Inf),
                   col=refcol,lty=2)
    
    if(refvalue){
      allfigure <- allfigure +
        geom_text(data = refdata,mapping = aes(x=ref,y=-Inf,label=reflab),
                  hjust=-0.2,size=3,vjust=-0.4,col=refcol)
    }
  }
  
  
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  if(is.null(linecol)){linecol = cbbPalette[1:length(alltrt)]}
  if(is.null(linetype)){linetype = 1:length(alltrt)}
  
  allfigure <- allfigure +
    scale_color_manual(labels = alltrt,values = linecol) +
    scale_linetype_manual(labels = alltrt,values = linetype)
  
  
  if(is.null(outpath)){outpath <- getwd()}
  ggsave(paste(outpath,outfile),allfigure,width = pwidth,height = pheight,dpi = res)
  return(list(allfigure,finaltable))
  
}
