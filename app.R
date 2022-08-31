#-----------------------------------#
# 2020 Summer Intern Project
# VSARgraph app
# Intern: David Huang (author)
# Mentors: Jianing Li and Zhang Ying
# makeForest and makeRCDC
#-----------------------------------#


library(shiny)
library(ggplot2)
library(tidyverse)
library(haven)
library(shinythemes)
library(ggpubr)
library(grid)
library(gridExtra)

source("makeRCDC.R")

source("make_forest_data(ckbplotr).R")
source("make_forest.R")
source("makeForest.R")
source("prepare_forest_data.R")
source("addtable_forest.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("VSARgraph", theme = shinytheme("lumen"),
               
               tabPanel("Forest",
                        sidebarLayout(
                            sidebarPanel(titlePanel("Make a Forest Plot"),
                                         
                                         tabsetPanel(
                                             tabPanel("Data",
                                                      fileInput("file2","Upload a file",multiple = FALSE),
                                                      checkboxInput("ratio", "Ratio", value = T),
                                                      tags$hr(),
                                                      #checkboxInput("ifall", "Use all serotypes (one block)", value = F),
                                                      selectInput("topsero","Top block serotypes", NULL,multiple = T),
                                                      selectInput("botsero","Bottom block serotypes", NULL,multiple = T),
                                                      sliderInput("adjbtmprop","Bottom proportion", min=0,max=1,value = NULL),
                                             
                                                      
                                                     
                                                      tabsetPanel(
                                                          
                                                          tabPanel("Label 1",
                                                                   textInput("typelabe1", "Serotype label 1", value = "SeroLabel"),
                                                                   selectInput("label1sero","Label 1 serotype", NULL,multiple = T)
                                                          ),
                                                          
                                                          tabPanel("Label 2",
                                                                   textInput("typelabe2", "Serotype label 2", NULL),
                                                                   selectInput("label2sero","Label 2 serotype", NULL,multiple = T)
                                                          )
                                                          
                                                      ),
                                                      tags$hr(),
                                                      
                                                      selectInput("estcol","Estimate column", NULL,multiple = T),
                                                      textInput("estlabel","Estimate label (used ',' to seprate)",NULL),
                                                      checkboxInput("ifcomp", "Show Comparison", value = T),
                                                      selectInput("compcol","Comparison column", "RATIO",multiple = F),
                                                      sliderInput("adjtblprop","Table width proportion", min=0,max=1,value = 0.5),
                                                      
                                                                   
                                                      submitButton(text = "Plot", icon = NULL, width = NULL)),
                                             
                                             tabPanel("Parameter",
                                                      
                                                      textInput("complabel","Comparison label","FC 95% CI"),
                                                      textInput("xlabel","X label for plot",value = ""),
                                                      textInput("xsublabel","X sublabel for plot",value = ""),
                                                      checkboxInput("ifkey", "If key refline", value = F),
                                                      
                                                      tabsetPanel(
                                                          
                                                          tabPanel("Top",
                                                                   "Use ',' to separate numbers" ,
                                                                   textInput("xtickstop", "X ticks (top)",value = ""),
                                                                   textInput("outliertop", "Outlier (top)",value = ""),
                                                                   textInput("reftop", "Reference line (top)",NULL),                  
                                                                   
                                                          ),
                                                          
                                                          tabPanel("Bottom",
                                                                   "Use ',' to separate numbers" ,
                                                                   textInput("xticksbot", "X ticks (bottom)",value = ""),
                                                                   textInput("outlierbot", "Outlier (bottom)",value = ""),
                                                                   textInput("refbot", "Reference line (bottom)",NULL),                  
                                                                   
                                                          )
                                                          
                                                      ),
                                                      
                                                      
                                                      tabsetPanel(
                                                          
                                                          tabPanel("Labelsize",
                                                                   sliderInput("labelsize","Label Size", min=5,max=30,value = 12)
                                                          ),
                                                          
                                                          tabPanel("Tablesize",
                                                                   sliderInput("tablesize","Table Size", min=5,max=30,value = 12)
                                                          ),
                                                          
                                                          tabPanel("Axissize",
                                                                   sliderInput("axissize","Axis Size", min=5,max=30,value = 12)
                                                          )
                                                      ),
                                                      
                                                      
                                                     
                                                      selectInput("reflinelty","Select reference line types", choices=0:6,multiple = T),
                                                      selectInput("reflinecol","Select line colors", choices=c("red","blue","yellow","green","black"),multiple = T),
                                                      submitButton(text = "Apply Change", icon = NULL, width = NULL))
                                                      # sliderInput("labelsize","Label Size", min=5,max=30,value = 12),
                                                      # sliderInput("tablesize","Table Size", min=5,max=30,value = 12),
                                                      # sliderInput("axissize","Axis Size", min=5,max=30,value = 12))
                                         ),
                                         width = 3),
                            mainPanel(plotOutput("distPlotforest",height = "700px",width="60%"),
                                      width = 9)
                        )
               ),
               tabPanel("RCDC",
                        sidebarLayout(
                            sidebarPanel(titlePanel("Make a RCDC Plot"),
                                         
                                         tabsetPanel(
                                             tabPanel("Data",
                                                      fileInput("file1","Upload a file",multiple = FALSE),
                                                      tags$hr(),
                                                      selectInput("sero","Select a serotype", NULL),
                                                      selectInput("visit","Select visit(s)", NULL,multiple = T),
                                                      selectInput("trt","Select treatment(s)", NULL,multiple = T),
                                                      
                                                      checkboxGroupInput("byvar", "Select BY variable(s)",choices = c("ABLFL" = "ABLFL","AVISITN" ="AVISITN" ),selected = c("AVISITN") ),
                                                      checkboxGroupInput("filvar", "Select Filter variable(s)", choices = c("PPROTRFL" ="PPROTRFL", "ANL01FL" = "ANL01FL"),selected = c("PPROTRFL","ANL01FL") ),
                                                      numericInput("ref1", "Reference line",NULL),                  
                                                      submitButton(text = "Plot", icon = NULL, width = NULL)),
                                             
                                             tabPanel("Parameter",
                                                      textInput("trtname","Treatment name (used ',' to seprate)",NULL),
                                                      textInput("avtname","Visit name (used ',' to seprate)",NULL),
                                                      textInput("header","Header for plot",value = ""),
                                                      selectInput("linetype","Select line types", choices=c("solid.1", "dashed.1", "dotted.1", "dotdash.1","solid.2", "dashed.2", "dotted.2", "dotdash.2"),multiple = T),
                                                      selectInput("linecolor","Select line colors", choices=c("red.1","blue.1","yellow.1","green.1","red.2","blue.2","yellow.2","green.2"),multiple = T),
                                                      selectInput("lineorder","Select line order", choices=1:4,multiple = T),
                                                      sliderInput("legendsize","Lengend Size", min=5,max=30,value = 15),
                                                      sliderInput("headersize","Header Size", min=5,max=30,value = 15),
                                                      sliderInput("axissize1","Axis Size", min=5,max=30,value = 15))
                                         ),
                                         width = 3),
                            mainPanel(plotOutput("distPlot",height = "650px"),
                                      width = 9)
                        )
               )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    data <- reactive({
        
        inFile <- input$file2
        if (is.null(inFile)) return(NULL)
        haven::read_sas(inFile$datapath)
    })
    
    allsero <- reactive({
        mys <- data()[["SEROTYPE"]]
        mys <- mys[!mys==""]
        return(mys)
        
        })
    observeEvent(data(), {
        updateSelectInput(session, "topsero", choices = allsero(),selected = allsero())
        updateSelectInput(session, "botsero", choices = allsero())
        updateSelectInput(session, "label1sero", choices = allsero(),selected = allsero())
        updateSelectInput(session, "label2sero", choices = allsero())
        updateSelectInput(session, "estcol", choices = colnames(data()))
        updateSelectInput(session, "compcol", selected="RATIO",choices = colnames(data()))
    })
    
    
    estcol   <- reactive(input$estcol)
    estlabel <- reactive({
        unlist(strsplit(input$estlabel,","))
    })
    
    
    
    complabel <- reactive({
        input$complabel
    })
    
    compcol  <- reactive(input$compcol)
    
    ifcomp <- reactive(input$ifcomp)
    ifkey <- reactive(input$ifkey)
    ratio <- reactive({
        if(input$ratio)return("R")
        return("D")
    })
    
    nblock <- reactive({
        
        if(is.null(input$botsero)){return(1)}
        return(2)})
        
    topbot <- reactive({
        
        #if(ifall()){return(list(allsero(),NULL))}
        return(list(input$topsero, input$botsero))
    })
    
    typelabel <- reactive({
        
        newlabel <- rep(NA, length(allsero()))
        newlabel[1:length(input$label1sero)] <- input$typelabe1
        if(input$typelabe2!="")newlabel[(length(input$label1sero)+1):length(allsero())] <- input$typelabe2
        return(newlabel)
    })
    
    
    #output$testtext <- renderText(c(length(allsero())))
    adjbtmprop <- reactive({
        if(!is.null(input$adjbtmprop)){return(input$adjbtmprop)}
        lt <- length(topbot()[[1]])
        lb <- length(topbot()[[2]])
        return((lb+4)/(lt+lb+4))
    })
    
    reflinecol <- reactive({
        if(is.null(input$reflinecol))return(NULL)
        input$reflinecol})
    
    reflinelty  <- reactive({
        if(is.null(input$reflinelty))return(NULL)
        input$reflinelty   %>% as.numeric()})
    
    xlabel <- reactive({
        if(input$xlabel=="")return(NULL)
        input$xlabel})
    
    xsublabel <- reactive({
        if(input$xsublabel=="")return(NULL)
        input$xsublabel})
    
    xticks <- reactive({
        
        if(nblock()==2){
        if(input$xtickstop=="" & input$xticksbot=="")return(list(NULL,NULL))
        if(input$xtickstop!="" & input$xticksbot==""){
            toptick <- unlist(strsplit(input$xtickstop,",")) %>% as.numeric()
            return(list(toptick,NULL))
        }
        if(input$xtickstop!="" & input$xticksbot!=""){
            toptick <- unlist(strsplit(input$xtickstop,",")) %>% as.numeric()
            bottick <- unlist(strsplit(input$xticksbot,",")) %>% as.numeric()
            return(list(toptick,bottick))
        }}else{
            if(input$xtickstop=="")return(list(NULL))
                toptick <- unlist(strsplit(input$xtickstop,",")) %>% as.numeric()
                return(list(toptick))
        }
    })
    
    outlier <- reactive({
        
        if(nblock()==2){
            if(input$outliertop=="" & input$outlierbot=="")return(list(NULL,NULL))
            if(input$outliertop!="" & input$outlierbot==""){
                topout <- unlist(strsplit(input$outliertop,",")) %>% as.numeric()
                return(list(topout,NULL))
            }
            if(input$outliertop!="" & input$outlierbot!=""){
                topout <- unlist(strsplit(input$outliertop,",")) %>% as.numeric()
                botout <- unlist(strsplit(input$outlierbot,",")) %>% as.numeric()
                return(list(topout,botout))
            }}else{
                if(input$outliertop=="")return(list(NULL))
                if(input$outliertop!=""){
                    topout <- unlist(strsplit(input$outliertop,",")) %>% as.numeric()
                    return(list(topout))
                }
            }
    })
    
    
    ref <- reactive({
        
        if(nblock()==2){
            if(input$reftop=="" & input$refbot=="")return(list(1,1))
            if(input$reftop!="" & input$refbot==""){
                topref <- unlist(strsplit(input$reftop,",")) %>% as.numeric()
                return(list(topref,1))
            }
            if(input$reftop!="" & input$refbot!=""){
                topref <- unlist(strsplit(input$reftop,",")) %>% as.numeric()
                botref <- unlist(strsplit(input$refbot,",")) %>% as.numeric()
                return(list(topref,botref))
            }}else{
                if(input$reftop=="")return(list(1))
                if(input$reftop!=""){
                    topref <- unlist(strsplit(input$reftop,",")) %>% as.numeric()
                    return(list(topref))
                }
            }
    })
    
    
    
    
    
    labelsize <- reactive(input$labelsize)
    tablesize <- reactive(input$tablesize)
    axissize   <- reactive(input$axissize)
    adjtblprop <- reactive(input$adjtblprop)
    
    #output$testtext1 <- renderText(c(adjbtmprop()))
    mfplot <-  reactive({

        req(input$file2)
        req(typelabel)
        req(topbot)
        if(is.null(topbot()[[1]]))return(NULL)
        makeForest(

            dataset     = data(),
            typecol     = "SEROTYPE",
            datatype    = ratio(),
            estcol      = estcol(),
            compcol     = compcol(),

            typelabel   = typelabel(),
            estlabel    = estlabel(),
            complabel   = complabel(),

            typehead    = NULL,
            esthead     = c("V114","PCV13"),
            comphead    = NULL,

            nblock      = nblock(),
            topbot      = topbot(),

            ifest       = T,
            ifcomp      = ifcomp(),

            xlabel      = xlabel(),
            xsublabel   = xsublabel(),

            #set for multiple panel
            xticks      = xticks(), #c(0.1,1,10,100)
            outlier     = outlier(),

            refline     = ref(),
            ##############################

            reflinecol     = reflinecol(), # reference line color
            reflinelty     = reflinelty(), # reference line type

            ifkey       = ifkey(),
            ifkeycol    = "red", # touch key line color


            pshape      = 20, # point shape
            plotcol     = "black", # CI line color

            axissize    = axissize(),
            tablesize   = tablesize(),
            labelsize   = labelsize(),

            #parameter about the plot

            adjhl        = list(c(0,0),c(0,0)), #adjust the line
            adjvl        = list(c(0,0),c(0,0)),

            adjcw        = c(0,0), #adjust the size of gap
            adjtblprop   = adjtblprop(),  #adj the size of table
            adjbtmprop   = NULL, # adjust the bottow and top part
            adjtypehead  = -1,  ##control type top and bottom

            res         = 1000, # resolution
            marg        = c(1.5,3,2,1), # margin of the plot
            pwidth      = 6,
            pheight     = 8,
            outpath     = NULL,
            outfile    = "forestplot.png"
        )})

    
    output$distPlotforest <- renderPlot({
        req(mfplot())
        
        if(length(mfplot())==2){
            grid.newpage()
            grid.arrange(mfplot()[[1]],mfplot()[[2]],nrow=2,heights=c(1-adjbtmprop(),adjbtmprop()))
            
        }else{
            grid.newpage()
            grid.draw(mfplot())
        }
        
        })
    
    
    ########################################################################################################
    
    data1 <- reactive({
        
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        read.csv(inFile$datapath)
    })
    
    observeEvent(data1(), {
        updateSelectInput(session, "sero", choices = data1()[["PARAMCD"]])
        updateSelectInput(session, "visit", choices = unique(data1()[["AVISITN"]]))
        updateSelectInput(session, "trt", choices = unique(data1()[["TRT01A"]]))
        
    })
    
    sero  <- reactive(input$sero)
    visit <- reactive(input$visit %>% as.numeric() %>% list())
    trt   <- reactive({
        mytrt <- input$trt
        mytrt <- unlist(mytrt)
        mytrt <- as.character(mytrt)
        mytrt})
    
    trtname <- reactive({
        if(is.null(input$trtname))return(NULL) 
        return(unlist(strsplit(input$trtname,",")))})
    
    avtname <- reactive(unlist(strsplit(input$avtname,",")))
    ref1     <- reactive({
        if(is.null(input$ref1))return(NULL) 
        input$ref1})
    lineorder <- reactive({
        if(is.null(input$lineorder))return(NULL)
        input$lineorder  %>% as.numeric()})
    
    linecolor <- reactive({
        if(is.null(input$linecolor))return(NULL)
        #input$linecolor}
        sapply(strsplit(input$linecolor,split = "[.]"), function(x)x[1])}
        )
    
    linetype  <- reactive({
        if(is.null(input$linetype))return(NULL)
        sapply(strsplit(input$linetype,split = "[.]"), function(x)x[1])})
    
    header    <- reactive({
        if(input$header =="")return(input$sero) 
        input$header})
    
    legendsize <- reactive(input$legendsize)
    headersize <- reactive(input$headersize)
    axissize1   <- reactive(input$axissize1)
    
    mybar <- reactive(input$byvar)
    myfil <- reactive(input$filvar)
    
    ablfl <- reactive({if("ABLFL" %in% mybar()){return("Y") }else{return(NULL)}})
    avisit <- reactive({if("AVISITN" %in% mybar()){return("AVISITN") }else{return(NULL)}})
    PPROTRFL <- reactive({if("PPROTRFL" %in% myfil()){return("Y") }else{return(NULL)}})
    ANL01FL <- reactive({if("ANL01FL" %in% myfil()){return("Y") }else{return(NULL)}})
    
    myrcdcplot <-  reactive({
        
        req(input$file1)
        req(sero())
        req(trt())
        req(data1())
        makeRCDC(
            
            dataset           = data1(),
            keyvar            = "AVAL",
            PARAMCD           = sero(),
            
            TRT01A            = trt(),
            TRT01Alabel       = NULL,
            ABLFL             = ablfl(), 
            ABLFLlabel        = NULL,#c("", Y),
            byvar             = avisit(), 
            byvarlabel        = NULL,
            
            PPROTRFL          = PPROTRFL(),
            ANL01FL           = ANL01FL(),
            
            AVISITN           = visit(),
            AVISITNlabel      =  c("Baseline","Post Baseline","Post Baseline"),
            
            ref               = ref1(),
            lineorder         = lineorder(),
            linetype          = linetype(),
            linecol           = linecolor(),
            legendsize        = legendsize(),
            headersize        = headersize(),
            axissize          = axissize1(),
            ncolumn           = 2,
            header            = header()
        )})
    
    output$distPlot <- renderPlot({
        req(myrcdcplot())
        myrcdcplot()[[1]]})
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
