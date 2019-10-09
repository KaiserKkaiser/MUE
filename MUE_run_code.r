#Put both the source code and 
Dir<-"" #Change this to match the path for the folder you put the
source(paste0(Dir,'MUE_code.r'), echo=TRUE)
dat.in<-read.csv(paste0(Dir,"smallData.csv"),header=T)
#Split biomass from coefficient of variations (CVs)
index<-dat.in[,2:(((ncol(dat.in)-1)/2)+1)]
CVs<-dat.in[,(((ncol(dat.in)-1)/2)+2):ncol(dat.in)]
years<-dat.in[,1]

#RUN clusters
#Hubert's gamma for the assignment clusters
spp.Hg<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
#Make silhouette plot
plot(pam(spp.Hg$D.matrix,2,diss=TRUE), main="HUBERT's GAMMA used for cluster assignment")
abline(v=c(0.25,0.5,0.75),col="red",lwd=c(1,2,3))

#Silhouette for the assignment clusters
spp.Sil<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T)
#Make silhouette plot
plot(pam(spp.Sil$D.matrix,2,diss=TRUE), main="SILHOUETTE used for cluster assignment")
abline(v=c(0.25,0.5,0.75),col="red",lwd=c(1,2,3))

