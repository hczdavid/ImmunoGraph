
#--------------------------------------#
#  Function 2: addtable_forest         #
#--------------------------------------#


#' @title addtable_forest
#' @description  add table to the forest plot
#' @keywords internal
#' @param fp forest plot from make_forest function
#' @param pdata data from prapare_forest_data function
#' @param trtname treatment name for columns of the table
#' @param tablecol column names what used to display in the table
#' @param tablelabel column names display in the table; need to correponding to tablecol and trtname
#' @param adjhl a two value vector to adjust the horizontal line of the table
#' @param adjtblprop adjust the size of table compare to the plot
#' @param adjgap adjust the gap between plot and table; two value for two panel
#' @param complabel ratio difference lable
#' @param adjvv adj for vertical line
#' @keywords internal

addtable_forest <- function(


  fp         = NULL,
  pdata      = NULL,

  estcol     = NULL,
  complabel  = NULL,
  estlabel   = NULL,

  esthead    = NULL,
  comphead   = NULL,

  ifest      = NULL,
  ifcomp     = NULL,

  table_size = NULL,
  label_size  = NULL,


  x2side     = NULL,

  adjhl       = NULL,
  adjg       = NULL,
  adjtblprop       = NULL,
  adjvv      = NULL

){


  maxUCI <- x2side[2]
  tablespace <- (x2side[2]- x2side[1])*adjtblprop*0.9


  if(!ifest | is.null(estcol)){
    gd <-tablespace*0.5
    fp_table <- fp +
                eval(parse(text=sprintf('geom_text(data=pdata,aes(x = -row, y = maxUCI + gd + %s, label = `textresult`, fontface = "bold"),
                size = %s,na.rm = TRUE,parse = T)', adjg, table_size*5/14))) +
                eval(parse(text=sprintf('annotate(geom="text",x = -1, y = maxUCI + gd + %s, label = "%s", fontface = "bold",
                size = %s, na.rm = TRUE, parse = F)',adjg,complabel,label_size*5/14))) +
                eval(parse(text=sprintf('annotate(geom="text", x = -0.4, y = maxUCI + gd + %s, label = "%s", fontface = "bold",
                size = %s, na.rm = TRUE, parse = F)', adjg, comphead, label_size*5/14)))
   return(fp_table)
  }else{

    #----------------Handle number ending with 0 issue---------#
    for(gg in 1:length(estcol)){
      indd <- which(!is.na(pdata[,estcol[gg]]))
      uuu <- prod(round(pdata[indd,estcol[gg]])==pdata[indd,estcol[gg]])
      if(uuu==0){
        pdata[[estcol[gg]]][indd] <- format(round(pdata[[estcol[gg]]][indd], digits=2), nsmall = 2)
        for(kk in indd){
          pdata[[estcol[gg]]][kk] <- paste0("`",pdata[[estcol[gg]]][kk],"`")
        }
      }
    }
    #----------------Done number ending with 0 issue---------#


    #length of the vertical line of the table
    ln <- sum(pdata$Heading != "")-1

    nest <- length(esthead)
    nlab <- length(unique(estlabel))
    nall <- length(estlabel)

    if(ifcomp){
      myseq <- (1:(nall+1))*tablespace/(nall+2) + adjg
      myseq[nall+1] <- myseq[nall+1]+tablespace/(nall+2)
      gd <- tablespace/((nall+2)*1.5) + adjg
    }else{
      myseq <- (1:nall)*tablespace/nall + adjg
      gd <- tablespace/((nall)*1.5) + adjg
      }

    #esthead gap distance
    estheadgd <- c()
    for(i in 1:nest){
      temp <- myseq[(1:nlab)+(i-1)*nlab]
      estheadgd[i] <- mean(temp)}

    #update esthead gap based on line
    for(i in 1:(nest-1)){estheadgd[i+1] <- (myseq[(i+1)*nlab]+adjvv[i+1]-myseq[i*nlab]-adjvv[i])/2+
      myseq[i*nlab]+gd+adjvv[i]
    }


    #-----------------------code for table, label and head---------------------#
    tablecode <- list()
    labelcode <- list()
    headcode  <- list()

    for (i in 1:nall){

      tablecode[[i]] <-
        sprintf('geom_text(data=pdata,aes(x = -row, y = maxUCI+%s, label = `%s`,
        fontface = "plain"),size = %s,na.rm = TRUE,parse = T)',myseq[i],estcol[i],table_size*5/14)
      labelcode[[i]] <-
        sprintf('annotate(geom="text",x = -1, y = maxUCI+%s, label = "%s",
                fontface = "bold",size = %s,na.rm = TRUE)',myseq[i],estlabel[i],label_size*5/14)
    }


    #-------------------------start to add table to the plot-------------------#
    for(i in 1:nall){
      fp <- fp +
        eval(parse(text = tablecode[[i]])) +
        eval(parse(text = labelcode[[i]]))
    }

    for(i in 1:nest){
        fp <- fp +  annotate(geom="text",x = -0.4, y = maxUCI+estheadgd[i], label = esthead[i],
                fontface = "bold",size =label_size*5/14 ,na.rm = TRUE)
      }


    for(i in 1:(nest-1)){

    fp <- fp + eval(parse(text = sprintf('annotate(geom = "segment", x=-1.5, xend=-1.4-%s, y=%s+%s,
                 yend=%s+%s)',ln,maxUCI+myseq[i*nlab]+gd,adjvv[i],maxUCI+myseq[i*nlab]+gd,adjvv[i])))

    }
    #-------------------------done to add table to the plot-------------------#


    if(ifcomp){

      fp <- fp +

        geom_text(data=pdata,aes(x = -row, y = maxUCI+myseq[nall+1], label = `textresult`, fontface = "bold"),
                  size = table_size*5/14,na.rm = TRUE,parse = T) +

        annotate(geom = "text",x = -1, y = maxUCI+myseq[nall+1], label = complabel,
                 fontface = "bold",size = label_size*5/14) +

        annotate(geom = "text",x = -0.4, y = maxUCI+myseq[nall+1], label = comphead,
                 fontface = "bold",size = label_size*5/14) +

        eval(parse(text = sprintf('annotate(geom = "segment", x=-1.5, xend=-1.4-%s, y=%s+%s,
                 yend=%s+%s)',ln,maxUCI+myseq[nall]+gd,adjvv[nest],maxUCI+myseq[nall]+gd,adjvv[nest])))
      fp_table <- fp+annotate(geom = "segment", x=-1.5, xend=-1.5, y=maxUCI+myseq[1]-gd*0.8+adjhl[1], yend=maxUCI+myseq[nall+1]+gd*2+adjhl[2])
      return(fp_table)

    }else{
      fp_table <- fp+annotate(geom = "segment", x=-1.5, xend=-1.5, y=maxUCI+myseq[1]-gd*0.8+adjhl[1], yend=maxUCI+myseq[nall]+gd+adjhl[2])
      return(fp_table)
    }



  }
}


