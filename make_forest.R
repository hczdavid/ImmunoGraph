#' @title make_forest function
#' @description make forset plot without table but save the space for table
#' @keywords internal
#' @param datatoplot the data get from prepare_forest_data function
#' @param marg margin of the plot
#' @param ifylabel indicate to show or not show y axis lable;
#' @param xlable label of x axis
#' @param pshape shape of point
#' @param outcol col of CI after touch key line
#' @param CIcol CI color
#' @param title title of plot
#' @param axissize font size of x and y axis
#' @param xticks ticks of x axis
#' @param x2side two end of x axis
#' @param refline reference line
#' @param linecol color of reference line
#' @param linelty line type of referen line
#' @param ifest if show table
#' @param ifcomp if show ratio difference
#' @param adjtblprop the width of the table compared to plot
#' @keywords internal

make_forest <- function(
  
  #data and margin
  datatoplot   = NULL,
  marg         = marg,
  datatype     = NULL,
  
  #label and CI setting
  ifylabel     = TRUE,
  xlabel       = xlabel,
  xsublabel    = xsublabel,
  pshape       = pshape,
  axis_size    = axis_size,
  label_size   = label_size,
  table_size   = table_size,
  xticks       = xticks,
  x2side       = x2side,
  
  #add vertical line
  refline      = refline,
  linecol      = linecol,
  linelty      = linelty,
  
  ifest        = T,
  ifcomp       = T,
  adjtblprop         = 1,
  adjtypehead      = adjtypehead,
  typehead   = NA
  
){
  options(warn = -1)
  
  #get the heading
  headings <- datatoplot %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(Heading = dplyr::first(Heading)) %>%
    dplyr::arrange(row) %>%
    dplyr::pull(Heading)
  
  #if y label if false, no headings
  if(!ifylabel){
    headings <- NULL
  }
  
  # Get a character vector of the style for headings
  boldheadings <- datatoplot %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(bold = dplyr::if_else(all(bold == "bold") | all(boldheading), "bold", "plain")) %>%
    dplyr::arrange(row) %>%
    dplyr::pull(bold)
  
  
  #------------Deal with reference line-------------#
  if(!is.null(refline)){
    nl <- length(refline)
    
    if(is.null(linecol)){linecol <- rep("black",nl)}
    if(is.null(linelty)){linelty <- rep(1,nl)}
    
    linecode <- list()
    for(i in 1:nl){
      linecode[[i]] <-
        sprintf('annotate(geom = "segment", x=-1.8, xend=-Inf, y=%s, yend=%s,lty=%s,col="%s")',
                refline[i], refline[i], linelty[i], linecol[i])
    }
  }
  #-----------Done with reference line--------------#
  
  
  #save the position for x label and x axis line
  label_p <- mean(c(x2side[1],x2side[2]))
  topx <- x2side[2]
  
  #save space for table
  if(ifest | ifcomp) {x2side[2] <- x2side[2] + (x2side[2] - x2side[1]) * adjtblprop}
  
  #--------------------Deal with X label---------------#
  if(!is.null(xlabel)){
    
    if(datatype == "R"){
      xlabel <- bquote(bold(.(xlabel)~"Log"["10"]~"Scale"))
    }
    
    if(is.null(xsublabel)){xlvadj <-  3.6}else{xlvadj  <-  3.3}
  }
  
  if(is.null(xlabel)){
    xlvadj <-  3.6
    if(datatype == "R"){
    xlabel <- expression(bold("Log"["10"]~"Scale"))}else{
      xlabel <- "Rate Difference"
    }
  }
  
  if(is.null(xlabel) & !is.null(xsublabel)){
    stop("please add x label if you want to add xsublabel")
  }
  #--------------------Done with X label---------------#
  
  
  #--------------------------------#
  #   Start to create the ggplot
  #--------------------------------#
  p1 <- ggplot(datatoplot, aes(x=-row, y=estimate))
  
  #add reference line if has
  if(!is.null(refline)){
    for(i in 1:nl)
      p1 <- p1 + eval(parse(text = linecode[[i]]))
  }
  
  #add second line of x axis if has
  if(!is.null(xsublabel)){
    p1 <- p1 + annotate(geom = "text",x = -Inf, y = label_p,label = xsublabel,hjust = 0.5,
                        size  = label_size*5/14,vjust = 5.3)
  }
  
  #add typehead if has
  if(!is.null(typehead)){
    p1 <- p1 + annotate(geom = "text",x = -0.5, y = -Inf,label = typehead,hjust = -adjtypehead,
                        size  = label_size*5/14,vjust = 0,fontface = "bold")
  }
  
  if(datatype == "R"){
    btick <- log10(xticks)
  }else{btick <- xticks}
  
  #Main code for the plot
  p1 <- p1+geom_point(size=3,shape = pshape,na.rm = TRUE,colour = datatoplot$linecolour) +
    
    geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate)),
                   aes(ymin = lci, ymax = uci, colour = linecolour), na.rm = TRUE) +
    scale_colour_identity() +
    
    geom_segment(data = ~ dplyr::filter(.x, overup),
                 aes(x=-row, y=uci-0.000001, xend=-row, yend=uci,colour = linecolour),
                 arrow = arrow(type = "closed", length = unit(6, "pt"))) +
    geom_segment(data = ~ dplyr::filter(.x, overlow),
                 aes(x=-row, y=lci+0.000001, xend=-row, yend=lci,colour = linecolour),
                 arrow = arrow(type = "closed", length = unit(6, "pt"))) +
    
    coord_flip(clip = "off",ylim = x2side)+
    scale_x_continuous(breaks = -1:-max(datatoplot$row),labels = headings,name   = "",expand = c(0,0)) +
    scale_y_continuous(labels= xticks, breaks = btick,expand = c(0,0),name   = "")+
    annotate(geom = "text",x = -Inf, y = label_p,label = xlabel,hjust = 0.5,
             size  = label_size*5/14,vjust = xlvadj,fontface = "bold")+
    #add x axis line
    annotate(geom = "segment", x=-Inf, xend=-Inf, y=x2side[1], yend=topx,lty=1,col="black")+
    # Control the overall looks of the plots
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x      = element_blank(),#element_line(size = 0.5)
          axis.title.x     = element_blank(),
          axis.ticks.x     = element_line(colour = "black"),
          axis.text.x      = element_text(colour = "black",
                                          margin = margin(t = 4.4),size   = axis_size,
                                          vjust  = 1),
          axis.ticks.y     = element_blank(),
          axis.text.y      = element_text(hjust  = 0.5,
                                          size   = table_size,
                                          colour = "black",
                                          face   = boldheadings,
                                          margin = margin(r = 1, unit = "lines")),
          panel.border     = element_blank(),
          panel.spacing    = unit(6, "lines"),
          strip.background = element_blank(),
          strip.placement  = "outside",
          strip.text       = element_text(face = "bold"),
          legend.position  = "none",
          plot.margin      = unit(marg, "lines"))
  
  return(p1)
  
}


