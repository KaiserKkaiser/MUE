if(!require(shiny)){install.packages("shiny")}
library(shiny)
source("MUE_code.r")
mue_server <- function (input, output) {
    # output$contents <- renderTable({
        # Display the data as a table; testing method
    M_vals_all<- reactive({
    inFile <- input$file1
    if(is.null(inFile))
        return(NULL)
    M_vals_all<- data.frame(read.csv(inFile$datapath, header = T))    
        # rv <- reactiveValues(index, CVs, years)
    })


    output$rawData <- renderTable({
        if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        indexNum <- (((ncol(M_vals_all())-1)/2)+1)
        index <<- M_vals_all()[, 2 : indexNum]
        CVs<<-M_vals_all()[,(((ncol(M_vals_all())-1)/2)+2):ncol(M_vals_all())]
        years<<-M_vals_all()[,1]
        # CVs
        # years
        # index
        }
       #  M_vals_all()
    })

    #Hubert's gamma for the assignment clusters
    output$huresult <- renderUI({
        if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        spp.Hg<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
        spp.Hg
        }
    })


    output$silhresult <- renderUI({
        if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        spp.Sil<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T) 
        spp.Sil
        }
    })



shinyServer(mue_server)
}