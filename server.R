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
        # index<<-M_vals_all[,2:(((ncol(M_vals_all-1)/2)+1)]()
        # CVs<<-M_vals_all[,(((ncol(M_vals_all)-1)/2)+2):ncol(M_vals_all)]()
        # years<<-M_vals_all[,1]
        M_vals_all()
        # if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        # M_vals_all[,2:(((ncol(M_vals_all)-1)/2)+1)]
        # }
    })

    output$huresult <- renderUI({
        if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        spp.Hg<-CPUE.sims.SPP(M_vals_all[,2:(((ncol(M_vals_all)-1)/2)+1)],1000,rep(1,length(M_vals_all[,2:(((ncol(M_vals_all)-1)/2)+1)])),M_vals_all[,(((ncol(M_vals_all)-1)/2)+2):ncol(M_vals_all)],19,colnames(M_vals_all[,2:(((ncol(M_vals_all)-1)/2)+1)]),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
        spp.Hg
        }
    })



shinyServer(mue_server)
}