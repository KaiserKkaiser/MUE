if(!require(cluster)){install.packages("cluster")}
library(cluster) #pam()
if(!require(fpc)){install.packages("fpc")}
library(fpc) #cluste.stats()

#############################################
##############CPUE Simulations###############
#############################################
#Trend_true: matrix of true trend to simulate with error
#Nsims: number of simulations to perform
#num.by.pop: vector specifying how many of each true population trend to generate
#CVs_in: CVs used to generate data sets
#seed.num: specifying seed gives control over where to start simulations
#label: labels for the populations generated
#cutoff: Acceptable number of years with this number of missing values. Used when you have sample areas with missing years.
#qt.low/qt.hi= quantile probabilities for the validity outputs.
#op.type: 1=print; 0= leave out; input is a vector of 0s and 1s in the following order
#         K.Avg.Sil,Avg.Sil,SD.Avg.Sil,Gamma,G3,Hubert.gamma,Dunn,WB.ratio
#k.max.m: choose measure (1= Sil; 2= Hubert gamma) to apply assigment recording (2nd cluster)
#Z-score=T. Standardize the input Trend_true. If you want to maintain the abosolute difference between variables comprising the clustering metrics, then do not standardize them (Z_score=F).
#############################################
CPUE.sims.SPP<-function(Trend_true,Nsims,num.by.pop,CVs_in,seed.num=15,label,cutoff,qt.low=5,qt.hi=95,op.type=rep(1,8),k.max.m=2,Z_score=T)
{
    set.seed(seed.num)
    num_series<-sum(num.by.pop)
    num.series.cum<-cumsum(num.by.pop)
    sim.CPUE<-CPUE.sim.fakies(Trend_true,num.by.pop,CVs_in,runif(1,min=0,max=100000000),label)
    sim.CPUE_prep<-prep_Z_clust(sim.CPUE,1,dim(sim.CPUE)[1],"N",cutoff)
    CPUE.clust<-as.data.frame(matrix(nrow=Nsims,ncol=dim(sim.CPUE_prep)[2]))
    CPUE.clust.t<-as.data.frame(t(CPUE.clust))
    sil.max<-GK.max<-G3.min<-Hg.max<-Dunn.max<-matrix(nrow=Nsims,ncol=(dim(sim.CPUE_prep)[2]-2))
#    sil.max<-GK.max<-G3.min<-Hg.max<-Dunn.max<-matrix(nrow=Nsims,ncol=dim(CPUE.clust)[2])
   colnames(sil.max)<-colnames(GK.max)<-colnames(G3.min)<-colnames(Hg.max)<-colnames(Dunn.max)<-c(2:(dim(sim.CPUE_prep)[2]-1))
    #colnames(sil.max)<-colnames(GK.max)<-colnames(G3.min)<-colnames(Hg.max)<-colnames(Dunn.max)<-c(2:(dim(CPUE.clust)[2]))
    k.opt<-matrix(0,nrow=2,ncol=Nsims)
    colnames(k.opt)<-c(1:Nsims)
    rownames(k.opt)<-c("Sil","Hg")

    if(op.type[1]==1)
    {
        K.Avg.Sil<-list()
        for(ii in 1:Nsims)
        {
           #K.Avg.Sil[[ii]]<-matrix(nrow=(num_series-1),ncol=(dim(sim.CPUE_prep)[2]-2))
       K.Avg.Sil[[ii]]<-matrix(nrow=(num_series-1),ncol=dim(Trend_true)[2]-1)
         # K.Avg.Sil[[ii]]<-matrix(nrow=(num_series-1),ncol=dim(CPUE.clust)[2])
            names(K.Avg.Sil)[ii]<-paste("K.Avg.Sil", ii, sep=" ")
        }
    }
   Avg.Sil<-SD.Avg.Sil<-GK.gamma<-G3<-Hubert.gamma<-Dunn<-WB.ratio<-list(matrix(nrow=Nsims,ncol=(dim(sim.CPUE_prep)[2]-2)))
#    Avg.Sil<-SD.Avg.Sil<-GK.gamma<-G3<-Hubert.gamma<-Dunn<-WB.ratio<-list(matrix(nrow=Nsims,ncol=dim(CPUE.clust)[2]))
    names(Avg.Sil)<-"Avg.Sil"
    names(SD.Avg.Sil)<-"SD.Avg.Sil"
    names(GK.gamma)<-"GK.gamma"
    names(G3)<-"G3"
    names(Hubert.gamma)<-"Hubert.gamma"
    names(Dunn)<-"Dunn"
    names(WB.ratio)<-"WB.ratio"

for (i in 1:Nsims)
    {
    print(i)
    ## Code added for progress bar head ##
    progress$inc(1/Nsims, detail = paste("Doing", i, "simulation(s)"))
    ## Code added for progress bar tail ##

    sim.CPUE<-matrix(nrow=(dim(Trend_true)[1]),ncol=1)
    #Generate CPUE trends
    for(j in 1:dim(Trend_true)[2])
        {
        sim.CPUE.temp<-CPUE.sim.fakies(Trend_true[j],num.by.pop[j],CVs_in[j],runif(1,min=0,max=100000000),label[j])
        sim.CPUE<-cbind(sim.CPUE,sim.CPUE.temp)
        }
    sim.CPUE<-sim.CPUE[,-1]
    colnames(sim.CPUE)<-label
    if(Z_score==T){sim.CPUE<-prep_Z_clust(sim.CPUE,1,dim(sim.CPUE)[1],"N",cutoff)}
    #
    K_dist<-dist(t(sim.CPUE), method="euclidean")
    ##Record cluster stats
       K.clust.stats<-clust.validity(K_dist,(dim(sim.CPUE)[2]-1))
#        K.clust.stats<-clust.validity(K_dist,attributes(K_dist)$Size-1)
        if(op.type[1]==1)
            {K.Avg.Sil[[i]]<-K.clust.stats$K.Avg.Sil}
        Avg.Sil$Avg.Sil[i,]<-K.clust.stats$Avg.Sil
        SD.Avg.Sil$SD.Avg.Sil[i,]<-K.clust.stats$SD.Avg.Sil
        GK.gamma$GK.gamma[i,]<-K.clust.stats$GK.gamma
        G3$G3[i,]<-K.clust.stats$G3
        Hubert.gamma$Hubert.gamma[i,]<-K.clust.stats$Hubert.gamma
        Dunn$Dunn[i,]<-K.clust.stats$Dunn
        WB.ratio$WB.ratio[i,]<-K.clust.stats$WB.ratio

    ##
    ##Track frequencies of max values
    sil.index<-Avg.Sil$Avg.Sil[i,]==max(Avg.Sil$Avg.Sil[i,],na.rm=TRUE)
    GK.index<-GK.gamma$GK.gamma[i,]==max(GK.gamma$GK.gamma[i,],na.rm=TRUE)
    G3.index<-G3$G3[i,]==min(G3$G3[i,],na.rm=TRUE)
    Hg.index<-Hubert.gamma$Hubert.gamma[i,]==max(Hubert.gamma$Hubert.gamma[i,],na.rm=TRUE)
    Dunn.index<-Dunn$Dunn[i,]==max(Dunn$Dunn[i,],na.rm=TRUE)

    sil.max.temp<-GK.max.temp<-G3.min.temp<-Hg.max.temp<-Dunn.max.temp<-c(2:(dim(sim.CPUE)[2]-1))*0
#    sil.max.temp<-GK.max.temp<-G3.min.temp<-Hg.max.temp<-Dunn.max.temp<-c(2:(dim(CPUE.clust)[2]+1))*0
    sil.max.temp[sil.index]<-1
    if(sum(sil.max.temp)>1)
    {sil.max.temp[sil.index]<-0}
    GK.max.temp[GK.index]<-1
    if(sum(GK.max.temp)>1)
    {GK.max.temp[GK.index]<-0}
    G3.min.temp[G3.index]<-1
    if(sum(G3.min.temp)>1)
    {G3.min.temp[G3.index]<-0}
    Hg.max.temp[Hg.index]<-1
    if(sum(Hg.max.temp)>1)
    {Hg.max.temp[Hg.index]<-0}
    Dunn.max.temp[Dunn.index]<-1
    if(sum(Dunn.max.temp)>1)
    {Dunn.max.temp[Dunn.index]<-0}

    sil.max[i,]<-sil.max.temp
    GK.max[i,]<-GK.max.temp
    G3.min[i,]<-G3.min.temp
    Hg.max[i,]<-Hg.max.temp
    Dunn.max[i,]<-Dunn.max.temp

    #Record optimal k based on each cluster validity diagnostic
    max.k<-(2:(dim(sim.CPUE)[2]-1))
#    max.k<-(2:(dim(CPUE.clust)[2]+1))
    if(length(max.k[sil.index])==1)
    {k.opt[1,i]<-max.k[sil.index]}

    if(length(max.k[Hg.index])==1)
    {k.opt[2,i]<-max.k[Hg.index]}

   #Records group assignment based on max sil
   maxsil.ind<-c(2:(dim(sim.CPUE)[2]-1))
 #   maxsil.ind<-c(2:(dim(CPUE.clust)[2]+1))
    if(k.max.m==1)
        {max.k.clara<-k.opt[1,i]}
    if(k.max.m==2)
        {max.k.clara<-k.opt[2,i]}

    #silmax.k<-maxsil.ind[sil.index]
    #K.sim<-clara(K_dist, silmax.k)
    K.sim<-pam(K_dist, max.k.clara,diss=TRUE,stand=TRUE)
    CPUE.clust[i,]<-K.sim$clustering
    CPUE.clust.t[,i]<-as.factor(t(CPUE.clust[i,]))
    ##
    }

    k.opt<-list(k.opt)
    names(k.opt)<-"Sims_optimum_k"

    sil.op<-GK.op<-G3.op<-Hg.op<-Dunn.op<-list(matrix(nrow=6,ncol=(dim(sim.CPUE)[2]-2)))
#    sil.op<-GK.op<-G3.op<-Hg.op<-Dunn.op<-list(matrix(nrow=6,ncol=dim(CPUE.clust)[2]))

    sil.op[[1]][1,]<-colSums(sil.max)
    sil.op[[1]][2,]<-colMeans(Avg.Sil$Avg.Sil)
    sil.op[[1]][3,]<-sd(Avg.Sil$Avg.Sil)
    sil.op[[1]][4,]<-apply(Avg.Sil$Avg.Sil,2, median)
    sil.op[[1]][5,]<-apply(Avg.Sil$Avg.Sil,2, quantile,probs=qt.low/100)
    sil.op[[1]][6,]<-apply(Avg.Sil$Avg.Sil,2, quantile,probs=qt.hi/100)
    if(op.type[4]==1)
    {
      GK.op[[1]][1,]<-colSums(GK.max)
      GK.op[[1]][2,]<-colMeans(GK.gamma$GK.gamma)
      GK.op[[1]][3,]<-sd(GK.gamma$GK.gamma)
      GK.op[[1]][4,]<-apply(GK.gamma$GK.gamma,2, median)
      GK.op[[1]][5,]<-apply(GK.gamma$GK.gamma,2, quantile,probs=qt.low/100)
      GK.op[[1]][6,]<-apply(GK.gamma$GK.gamma,2, quantile,probs=qt.hi/100)
    }
      if(op.type[5]==1)
    {
      G3.op[[1]][1,]<-colSums(G3.min)
      G3.op[[1]][2,]<-colMeans(G3$G3)
      G3.op[[1]][3,]<-sd(G3$G3)
      G3.op[[1]][4,]<-apply(G3$G3,2, median)
      G3.op[[1]][5,]<-apply(G3$G3,2, quantile,probs=qt.low/100)
      G3.op[[1]][6,]<-apply(G3$G3,2, quantile,probs=qt.hi/100)
    }
    if(op.type[6]==1)
    {
      Hg.op[[1]][1,]<-colSums(Hg.max)
      Hg.op[[1]][2,]<-colMeans(Hubert.gamma$Hubert.gamma)
      Hg.op[[1]][3,]<-sd(Hubert.gamma$Hubert.gamma)
      Hg.op[[1]][4,]<-apply(Hubert.gamma$Hubert.gamma,2, median)
      Hg.op[[1]][5,]<-apply(Hubert.gamma$Hubert.gamma,2, quantile,probs=qt.low/100)
      Hg.op[[1]][6,]<-apply(Hubert.gamma$Hubert.gamma,2, quantile,probs=qt.hi/100)
    }
      if(op.type[7]==1)
    {
      Dunn.op[[1]][1,]<-colSums(Dunn.max)
      Dunn.op[[1]][2,]<-colMeans(Dunn$Dunn)
      Dunn.op[[1]][3,]<-sd(Dunn$Dunn)
      Dunn.op[[1]][4,]<-apply(Dunn$Dunn,2, median)
      Dunn.op[[1]][5,]<- apply(Dunn$Dunn,2, quantile,probs=qt.low/100)
      Dunn.op[[1]][6,]<-apply(Dunn$Dunn,2, quantile,probs=qt.hi/100)
    }
    names(sil.op)<-"Sil.max"
    names(GK.op)<-"GK.gamma.max"
    names(G3.op)<-"G3.min"
    names(Hg.op)<-"Hubert.gamma.max"
    names(Dunn.op)<-"Dunn.max"
    colnames(sil.op[[1]])<-colnames(GK.op[[1]])<-colnames(G3.op[[1]])<-colnames(Hg.op[[1]])<-colnames(Dunn.op[[1]])<-c(2:(dim(sim.CPUE)[2]-1))
 #   colnames(sil.op[[1]])<-colnames(GK.op[[1]])<-colnames(G3.op[[1]])<-colnames(Hg.op[[1]])<-colnames(Dunn.op[[1]])<-c(2:(dim(CPUE.clust)[2]+1))
    rownames(sil.op[[1]])<-rownames(GK.op[[1]])<-rownames(G3.op[[1]])<-rownames(Hg.op[[1]])<-rownames(Dunn.op[[1]])<- c("Sums","Means","Std.Dev","Median","5%Qu.","95%Qu.")

    colnames(CPUE.clust)<-names(K.sim$clustering)
    rownames(CPUE.clust.t)<-names(K.sim$clustering)
    K.dist.all<-daisy(CPUE.clust.t,metric="gower")
    K.sim.cl.stats.final<-clust.validity(K.dist.all,(dim(sim.CPUE)[2]-1))
#    K.sim.cl.stats.final<-clust.validity(K.dist.all,attributes(K_dist)$Size-1)
    sil.index.final<-K.sim.cl.stats.final$Avg.Sil==max(K.sim.cl.stats.final$Avg.Sil)
    k.sil.final<-maxsil.ind[sil.index.final]
    K.sim.final.sil<-pam(K.dist.all,k.sil.final,diss=TRUE,stand=TRUE)
    Hg.index.final<-K.sim.cl.stats.final$Hubert.gamma==max(K.sim.cl.stats.final$Hubert.gamma)
    k.Hg.final<-maxsil.ind[Hg.index.final]
    K.sim.final.Hg<-pam(K.dist.all,k.Hg.final,diss=TRUE,stand=TRUE)
    K.dist.all<-list(K.dist.all)
    names(K.dist.all)<-"D.matrix.final"
    K.sim.final.sil<-list(K.sim.final.sil)
    names(K.sim.final.sil)<-"K_means.sil.final"
    K.sim.final.Hg<-list(K.sim.final.Hg)
    names(K.sim.final.Hg)<-"K_means.Hg.final"
    #sil.coefs.all<-sil.plot(K.dist.all,(num_series-1),"N")
    K.sim.cl.stats.final<-list(K.sim.cl.stats.final)
    names(K.sim.cl.stats.final)<-"Final.Cluster.Stats"

    final.out<-list(CPUE.clust)
    names(final.out)<-"K_Clusters"
    if(op.type[1]==1)
    {final.out<-c(final.out,K.Avg.Sil)}
    if(op.type[2]==1)
    {final.out<-c(final.out,sil.op)}
    if(op.type[3]==1)
    {final.out<-c(final.out,SD.Avg.Sil)}
    if(op.type[4]==1)
    {final.out<-c(final.out,GK.op)}
    if(op.type[5]==1)
    {final.out<-c(final.out,G3.op)}
    if(op.type[6]==1)
    {final.out<-c(final.out,Hg.op)}
    if(op.type[7]==1)
    {final.out<-c(final.out,Dunn.op)}
    if(op.type[8]==1)
    {final.out<-c(final.out,WB.ratio)}

    final.out<-c(final.out, K.dist.all,K.sim.final.sil,K.sim.final.Hg, K.sim.cl.stats.final)

   # final.out<-list(CPUE.clust,sil.coefs,sil.max,K.sim.cl.stats$Avg.Clust.Sil,K.sim.cl.stats$GK.gamma,K.sim.cl.stats$G3, K.sim.cl.stats$Hubert.gamma, K.sim.cl.stats$Dunn, K.sim.cl.stats$WB.ratio,K.dist.all,K.sim.final,sil.coefs.all,K.sim.cl.stats.final$g2,K.sim.cl.stats.final$g2,K.clust.stats.final$hubertgamma,K.clust.stats.final$dunn,K.clust.stats.final$wb.ratio)
   # names(final.out)<-c("K_Clusters","Sil.C.","Max.Sil.C.","Avg.Cluster.Sils","G&K.gamma","G3","Hubert.gamma","Dunn","WB.ratio","D.matrix.final","K_means.final","Sil.C.final","G&K.gamma.final","G3.final","Hubert.gamma.final","Dunn.final","WB.ration.final")
    return(final.out)
}

##############################################################
#Adds specified number of fake CPUE series to current data set
##############################################################
#CPUEorZ: Whether the sim.CPUE.dat is CPUE or Z-score data. Enter "CPUE" if CPUE being used.
CPUE.sim.fakies<-function(sim.CPUE.dat,num.fakes, CV,seed.num, label)
{
label.fake<-paste(label,c(1:sum(num.fakes)),sep="")
rnums<-dim(sim.CPUE.dat)[1]
ncols<-sum((rep(1,dim(sim.CPUE.dat)[2])*num.fakes))
set.seed(seed.num)
fakies<-matrix(nrow=rnums,ncol=ncols)
sd.cpue<-CV*abs(sim.CPUE.dat)
mean.cpue<-sim.CPUE.dat
#sim.CPUE.op<-NULL
j<-1
for(g in 1:dim(sim.CPUE.dat)[2])
{
for(i in 1:num.fakes[g])
    {
    fakies[,j]<-rnorm(rnums,mean = mean.cpue[,g], sd= sd.cpue[,g])
    j<-i+j
    }

#sim.CPUE.op<-cbind(sim.CPUE.op,fakies)
}
#sim.CPUE.op
names(fakies)[1:ncols]<-label.fake
return(fakies)
}

######################################################################################
#*****************************Z-scores************************************************
######################################################################################
##Prepare Z-scores for clustering by removing NA, areas that don't meet the number of needed years
#cutoff: Acceptable number of years missing values
prep_Z_clust<-function(stDataFile,start_Z, end_Z, single,cutoff)
{
Z.temp<-cpue_Z_score(stDataFile,start_Z, end_Z,single)
block.edL<-is.na(Z.temp)
block.edL[block.edL=="TRUE"]<-1
block.sumsL<-colSums(block.edL)
Z.series<-na.omit(Z.temp[,as.logical(block.sumsL<cutoff)])
return(Z.series)
}

#single Z-score ("Y") or several Z-scores ("N")
cpue_Z_score<-function(DataFile,start_Z, end_Z, single)
{
if (single == "Y")
{
Z_scores<-DataFile
Z.dat<-Z_scores
Z.dat[Z.dat==0]<-NA
for (k in start_Z:end_Z)
    {
    avg.cpue<-mean(Z.dat, na.rm=TRUE)
    sd.cpue<-sd(Z.dat, na.rm=TRUE)
    Z_scores<-(Z.dat-avg.cpue)/sd.cpue
    }
}

if (single == "N")
{
Z_scores<-DataFile[start_Z:end_Z,]
Z.dat<-Z_scores
#Z.dat[Z.dat==0]<-NA
for (k in 1:dim(DataFile)[2])
    {
    avg.cpue<-mean(Z.dat[,k], na.rm=TRUE)
    sd.cpue<-sd(Z.dat[,k], na.rm=TRUE)
    Z_scores[,k]<-(Z.dat[,k]-avg.cpue)/sd.cpue
    }
}
return(Z_scores)
}

##########################################################
################ Cluster valdity #########################
##########################################################
#dist.file: dissimilarity (distance) matrix
#tot_k: The maximum k (i.e. number of clusters) to explore
#plots each cluster validity statistic

clust.validity<-function(dist.file,tot_k,plot_sils="N")
{
clustval.op<-list(matrix(data=NA,nrow=tot_k,ncol=(tot_k-1)),rep(-99,(tot_k-1)),rep(-99,(tot_k-1)),rep(-99,(tot_k-1)),rep(-99,(tot_k-1)),rep(-99,(tot_k-1)),rep(-99,(tot_k-1)),rep(-99,(tot_k-1)))
#names(clustval.op)<-c("K.Avg.Sil","Avg.Sil","SD.Avg.Sil","GK.gamma","G3","Hubert.gamma","Dunn","WB.ratio","Obj.funk")
names(clustval.op)<-c("K.Avg.Sil","Avg.Sil","SD.Avg.Sil","GK.gamma","G3","Hubert.gamma","Dunn","WB.ratio")
rownames(clustval.op$K.Avg.Sil)<-c(1:tot_k)
colnames(clustval.op$K.Avg.Sil)<-c(2:tot_k)
for(j in 2:tot_k)
{
    k.med<-pam(dist.file,j,diss=TRUE,stand=TRUE)
    k.clust_stats<-cluster.stats(dist.file,as.numeric(k.med$clustering),silhouette=TRUE,G2=TRUE,G3=TRUE)
    clustval.op$K.Avg.Sil[1:j,(j-1)]<-k.clust_stats$clus.avg.silwidths
    #clustval.op$Avg.Sil[j-1]<-k.clust_stats$avg.silwidth
    clustval.op$Avg.Sil[j-1]<-k.med$silinfo$avg.width
    clustval.op$SD.Avg.Sil[j-1]<-sd(k.clust_stats$clus.avg.silwidths)
    clustval.op$GK.gamma[j-1]<-k.clust_stats$g2
    clustval.op$G3[j-1]<-k.clust_stats$g3
    clustval.op$Hubert.gamma[j-1]<-k.clust_stats$pearsongamma
    clustval.op$Dunn[j-1]<-k.clust_stats$dunn
    clustval.op$WB.ratio[j-1]<-k.clust_stats$wb.ratio
#    clustval.op$Obj.funk[j-1]<-k.med$objective
}
if (plot_sils=="Y")
    {
    par(mfrow=c(2,2))
    plot(c(2:tot_k),clustval.op$Avg.Sil, ylim=c(0,1),xlab="Number of clusters", ylab="Index value", pch=21, bg="red", col="black", cex=1.1, type="b")
    points(c(2:tot_k),clustval.op$GK.gamma,pch=22, bg="blue", col="black", cex=1, type="b")
    points(c(2:tot_k),clustval.op$G3,xlab="Number of clusters", ylab="Index value",pch=22, bg="purple", col="black", cex=1, type="b")
    points(c(2:tot_k),clustval.op$Hubert.gamma,pch=24, bg="orange", col="black", cex=1, type="b")
    par(new=TRUE)
    plot(c(2:tot_k),clustval.op$Dunn,ylim=c(0,ceiling(max(clustval.op$Dunn))),xaxt = "n", yaxt = "n" ,pch=25, bg="green", col="black", cex=1,xlab="",ylab="", type="b")
    axis(4)
    mtext(4, text = "Dunn value", line = 3, padj=-0.75, cex=0.85)
    #points(c(2:tot_k),clustval.op$WB.ratio,pch=22, bg="purple", col="black", cex=1.1)
    }
return(clustval.op)
}

