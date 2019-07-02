if(!require(shiny)){install.packages("shiny")}
library(shiny)
if(interactive()) {
mue_server <- function (input, output) {
    # output$contents <- renderTable({
        # Display the data as a table; testing method
    M_vals_all<- reactive({
    inFile <- input$file1
    if(is.null(inFile))
        return(NULL)
    dat.in<-read.csv(inFile$datapath, header = T)
    #Split biomass from coefficient of variations (CVs)
    index<-dat.in[,2:(((ncol(dat.in)-1)/2)+1)]
    CVs<-dat.in[,(((ncol(dat.in)-1)/2)+2):ncol(dat.in)]
    years<-dat.in[,1]
    
    })

    output$Oplot <- renderPlot({
        #RUN clusters
        #Hubert's gamma for the assignment clusters
        spp.Hg<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
        #Make silhouette plot
        print(plot(pam(spp.Hg$D.matrix,2,diss=TRUE), main="HUBERT's GAMMA used for cluster assignment"))
        abline(v=c(0.25,0.5,0.75),col="red",lwd=c(1,2,3))
        print("Plot method completed")
    })
    
}

shinyServer(mue_server)
}