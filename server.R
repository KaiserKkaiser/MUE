if(!require(shiny)){install.packages("shiny")}
library(shiny)
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(cluster)){install.packages("cluster")}
if(!require(factoextra)) {install.packages("factoextra")}
if(!require(reshape2)) {intall.packagesrt("reshape2")}
library(ggplot2)
source("MUE_code.r")
mue_server <- function (input, output) {
    ### Process Sample Data ###
    dat.in<-read.csv(paste0("example_B_CVs.csv"),header=T)
    output$sample <- downloadHandler(
        filename = "sampleData.csv",
        content = function(file) {
            write.csv(dat.in, file, row.names=FALSE)
        }
    )

    output$paper <- downloadHandler(
        filename = "Cope_Punt_2009.pdf",
        content = function(file) {
            file.copy("1.pdf", file)
        }
    )

     M_vals_all<- eventReactive(input$file1,{
    inFile <<- input$file1
    if(is.null(inFile)) {
        return(NULL)
    }
    M_vals_all<- data.frame(read.csv(inFile$datapath, header = T))    
        # rv <- reactiveValues(index, CVs, years)
        
    })

    # output$contents <- renderTable({
    #     # Display the data as a table; testing method
    #     if(!is.null(input$file1)){
    #     indexNum <- (((ncol(M_vals_all())-1)/2)+1)
    #     index <<- M_vals_all()[, 2 : indexNum]
    #     CVs<<-M_vals_all()[,(((ncol(M_vals_all())-1)/2)+2):ncol(M_vals_all())]
    #     years<<-M_vals_all()[,1]
    #     M_vals_all()
    #     }
    # })

    # ### Uncomment to display raw data ##### Testing only #######
    # output$rawData <- renderTable({
    #     if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
    #     # indexNum <- (((ncol(M_vals_all())-1)/2)+1)
    #     # index <<- M_vals_all()[, 2 : indexNum]
    #     # CVs<<-M_vals_all()[,(((ncol(M_vals_all())-1)/2)+2):ncol(M_vals_all())]
    #     # years<<-M_vals_all()[,1]
    #     # CVs
    #     # years
    #     # index
    #     M_vals_all()
    #     } else {
    #         xx <- c(0, 1, 2)
    #         xx
    #     }
       #  M_vals_all()
    #    ans <- c(min(CVs), max(CVs))
    #    df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
    #                variable=rep(paste0("category", 1:9), each=5))
    #     newY <- years
    #     newA <- index[, 1]
    #     color <- rep("Area1", times=length(years))
    #     for(i in 2:length(index)) {
    #         # newY <- newY.cbind(years)
    #         # newA <- newA.cbine(index[, i])
    #         newY <- c(newY, years)
    #         newA <- c(newA, index[, i])
    #         color <- c(color, rep(paste0("Area", i), times=length(years)))
    #     }
    #     data <- data.frame(newY, newA, color)
    #     data
     #})

 ### Plot the input values when people upload a file ###

 ################ Initialize data and plot raw data and sample data ###############
    output$inputData1 <- renderPlot({
            inFile <- input$file1
            if(!is.null(input$file1)) {
                M_vals_all<- data.frame(read.csv(inFile$datapath, header = T))
                indexNum <- (((ncol(M_vals_all())-1)/2)+1)
                index <<- M_vals_all()[, 2 : indexNum]
                CVs<<-M_vals_all()[,(((ncol(M_vals_all())-1)/2)+2):ncol(M_vals_all())]
                years<<-M_vals_all()[,1]
            newY <- years
            newA <- index[, 1]
            # newC is potentially the span
            newC <- CVs[, 1]
            color <- rep("Area1", times=length(years))
            for(i in 2:length(index)) {
                newY <- c(newY, years)
                newA <- c(newA, index[, i])
                newC <- c(newC, CVs[, i])
                color <- c(color, rep(paste0("Area", i), times=length(years)))
            }
            data <- data.frame(newY, newA, color)
            rdp <<- reactive(ggplot(data, aes(x=newY, y=newA)) + geom_line(aes(colour=color), size=1) + ggtitle("Area with respect of time(year)") + theme_light())
            print(rdp())
            }
    })

    output$inputData2 <- renderPlot({

        if(!anyNA(M_vals_all()) && length(M_vals_all) > 0) {
        indexNum <- (((ncol(M_vals_all())-1)/2)+1)
        index <<- M_vals_all()[, 2 : indexNum]
        CVs<<-M_vals_all()[,(((ncol(M_vals_all())-1)/2)+2):ncol(M_vals_all())]
        years<<-M_vals_all()[,1]
        inFile <- input$file1
        if(!is.null(input$file1)) {
        newY <- years
        newA <- index[, 1]
        # newC is potentially the span
        newC <- CVs[, 1]
        color <- rep("Area1", times=length(years))
        for(i in 2:length(index)) {
            newY <- c(newY, years)
            newA <- c(newA, index[, i])
            newC <- c(newC, CVs[, i])
            color <- c(color, rep(paste0("Area", i), times=length(years)))
        }
        data2 <- data.frame(newY, newC, color)
        rdp2 <<- reactive(ggplot(data2, aes(x=newY, y=newC)) + geom_line(aes(colour=color), size=1) + ggtitle("CV each Area with respect of time(year)") + theme_light())
        print(rdp2())
        }
        }
    })

    ### Download Raw Data ###
    output$rawDataDownload <- downloadHandler(
        filename = "rawData.png",
        content = function(file) {
            png(file)
            print(rdp())
            dev.off()
        })

    output$rawDataDownloadCV <- downloadHandler(
        filename = "rawDataCV.png",
        content = function(file) {
            png(file)
            print(rdp2())
            dev.off()
        })


 ################################ Run The Result #################################
    # #Hubert's gamma for the assignment clusters
    # output$huresult <- renderUI({
    #     if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
    #     numberOfSimul <- as.numeric(input$noS)
    #     spp.Hg<<-CPUE.sims.SPP(index,numberOfSimul,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
    #     spp.Hg
    #     }
    # })

    # output$silhresult <- renderUI({
    #     if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
    #     numberOfSimul <- as.numeric(input$noS)
    #     spp.Sil<<- CPUE.sims.SPP(index,numberOfSimul,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T)
    #     spp.Sil
    #     }
    # })

    ### Print Comparable Plot for User to Decide the nubmer of clusters ###

    ## Output Hubert Gamma ##
    output$comparePlotHuHu <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            numberOfSimul <- as.numeric(input$noS)
            ## Progress bar
            progress <<- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "Running", value = 0)
            ## Progress bar


            spp.Hg<<-CPUE.sims.SPP(index,numberOfSimul,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
            dataa <- data.frame(xv = c(2:(length(CVs)-1)), yv = spp.Hg$Final.Cluster.Stats$Hubert.gamma)
            cpHU <<- ggplot(dataa, aes(x=xv, y=yv)) + geom_point(size = 3) +
                xlab("Clusters") + ylab("Average Hubert Gamma") +
                ggtitle("Hubert Gamma") + 
                geom_point(aes(x=match(max(spp.Hg$Final.Cluster.Stats$Hubert.gamma), spp.Hg$Final.Cluster.Stats$Hubert.gamma)+1, y=max(spp.Hg$Final.Cluster.Stats$Hubert.gamma)), fill="blue", color="darkred", size=4, shape=23)
            print(cpHU)
        } else if (input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            numberOfSimul <- as.numeric(input$noS)
            ## Progress bar
            progress <<- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "Running", value = 0)
            ## Progress bar
            spp.Sil<<- CPUE.sims.SPP(index,numberOfSimul,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T)
            dataa <- data.frame(xv = c(2:(length(CVs)-1)), yv = spp.Sil$Final.Cluster.Stats$Hubert.gamma)
            cpSil <<- ggplot(dataa, aes(x=xv, y=yv)) + geom_point(size = 3) +
                xlab("Clusters") + ylab("Average Hubert Gamma") +
                ggtitle("Hubert Gamma") + 
                geom_point(aes(x=match(max(spp.Sil$Final.Cluster.Stats$Hubert.gamma), spp.Sil$Final.Cluster.Stats$Hubert.gamma)+1, y=max(spp.Sil$Final.Cluster.Stats$Hubert.gamma)), fill="blue", color="darkred", size=4, shape=23)
            print(cpSil)
        }
        
    })

    output$cphh <- downloadHandler(
        filename = "cpHubertGamma.png",
        content = function(file) {
            png(file)
            if(input$button == 1) {
                print(cpHU)
            } else {
                print(cpHU2)
            }
            dev.off()
        })
    ## Output Silhouette ##
    output$comparePlotHuSil <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            dataa <- data.frame(xv = c(2:(length(CVs)-1)), yv = spp.Hg$Final.Cluster.Stats$Avg.Sil)
            cpHU2 <<- ggplot(dataa, aes(x=xv, y=yv)) + geom_point(size = 3) +
                xlab("Clusters") + ylab("Average Silhouette") +
                ggtitle("Silhouette") + 
                geom_point(aes(x=match(max(spp.Hg$Final.Cluster.Stats$Avg.Sil), spp.Hg$Final.Cluster.Stats$Avg.Sil)+1, y=max(spp.Hg$Final.Cluster.Stats$Avg.Sil)), fill="blue", color="darkred", size=4, shape=23)
            print(cpHU2)
        } else if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            dataa <- data.frame(xv = c(2:(length(CVs)-1)), yv = spp.Sil$Final.Cluster.Stats$Avg.Sil)
            cpSil2 <<- ggplot(dataa, aes(x=xv, y=yv)) + geom_point(size = 3) +
                xlab("Clusters") + ylab("Average Silhouette") +
                ggtitle("Silhouette") + 
                geom_point(aes(x=match(max(spp.Sil$Final.Cluster.Stats$Avg.Sil), spp.Sil$Final.Cluster.Stats$Avg.Sil)+1, y=max(spp.Sil$Final.Cluster.Stats$Avg.Sil)), fill="blue", color="darkred", size=4, shape=23)
            print(cpSil2)
        }
    })

    output$cphs <- downloadHandler(
        filename = "cphs.png",
        content = function(file) {
            png(file)
            if(input$button == 1) {
            print(cpHU2)
            } else {
                print(cpSil2)
            }
            dev.off()
        })

####################### Let the User Run with Their Number of Clusters ########################

    output$huplot <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            numberOfCluster <- as.numeric(input$noC)
            pp <<- pam(spp.Hg$D.matrix,numberOfCluster,diss=TRUE)
            hp <<- fviz_silhouette(pp, label=TRUE)
            avghp <<- dcast(hp$data,cluster~1,mean,value.var ="sil_width")
            for(i in 0:(numberOfCluster-1)) {
                hp <- hp + geom_hline(yintercept = avghp[[2]][i]) 
            }
            hp <- hp + geom_hline(yintercept=0.25, size=1, color="green") + 
                geom_hline(yintercept=0.5, size=1.5, color="green") + geom_hline(yintercept=0.75, size=2, color="green")
            print(hp)
        } else if (input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            numberOfCluster <- as.numeric(input$noC)
            spp <<- pam(spp.Sil$D.matrix,numberOfCluster,diss=TRUE)
            sp <<- fviz_silhouette(spp, label=TRUE)
            avgsp <<- dcast(sp$data,cluster~1,mean,value.var ="sil_width")
            for(i in 0:(numberOfCluster-1)) {
                sp <- sp + geom_hline(yintercept = avgsp[[2]][i])
            }
            sp <- sp + geom_hline(yintercept=0.25, size=1, color="green") + 
                geom_hline(yintercept=0.5, size=1.5, color="green") + geom_hline(yintercept=0.75, size=2, color="green")
            print(sp)
        }
    })
        # Download Hubert's gamma plot #
    output$huplotDownload <- downloadHandler(
        filename = "finalResult.png",
        content = function(file) {
            png(file)
            if(input$button == 0) {
                print(hp)
            } else {
                print(sp)
            }
            dev.off()
        })


######### Render Final Summary Table #############
    output$areaCluster <- renderTable({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            # When plotting huber gamma
            res <<- data.frame(pp$silinfo$widths)
            # Initialize Average for Silhouette
            res[, "AverageSilhouette"] <- numeric(nrow(res))
            # Rename the length
            names(res) <- sub("^sil_width$", "IndividualSilhouette", names(res))
            names(res) <- sub("^cluster$", "Cluster", names(res))
            avg <<- data.frame(pp$silinfo$clus.avg.widths)
            for(row in 1:nrow(res)) {
                res[row, 4] <- avg[res[row, 1],1]
                # 4 is the column of Avg, 1 is the column of cluster
            }
            res
        } else if (input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
            res <<- data.frame(spp$silinfo$widths)
            #res <- res[order(cluster),]
            avg <- data.frame(spp$silinfo$clus.avg.widths)
            names(res) <- sub("^sil_width$", "IndividualSilhouette", names(res))
            names(res) <<- sub("^cluster$", "Cluster", names(res))
            for(row in 1:nrow(res)) {
                res[row, 4] <<- avg[res[row, 1],1]
                # 4 is the column of Avg, 1 is the column of cluster
            }
            res
        }
    }, include.rownames=TRUE)

    output$resultDownload <- downloadHandler(
        filename = "finalResult.DMP",
        content = function(file) {
            if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0 && input$rd) {
                save(spp.Hg, file=file)
            } else if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0){
                save(spp.Sil, file=file)
            }
        })

    output$summaryTable <- downloadHandler(
        filename = "summaryData.csv",
        content = function(file) {
            write.csv(res, file)
        }
    )

shinyServer(mue_server)
}