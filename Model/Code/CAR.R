###############################################################################################
###############################################################################################
###############################################################################################
### 
### Function used for running an Intrinsic CAR model, theory can be found in CAR.pdf
###															  								
### Y=Y 	nx1 matrix of outcome observations
### X=X		nxp matrix of covariates
### W=W		nxn adjacency matrix
### Hypers	hyperparameters, default set to list("Beta"=10e4,"Sigma2"=c(3,1),"Tau2"=c(3,1))
### Inits	initial values, default set to list("Beta"=rep(0,dim(X)[2]),"Sigma2"=1,"Tau2"=1)
### NSims	scalar, default set to 10000
### NBurn	scalar, default set to 1000
### NThin	scalar, default set to 1
### Output	output folder for raw samples
###
###############################################################################################
###############################################################################################
###############################################################################################

car<-function(Y=Y,
			  X=X,
			  W=W,
		      Hypers=list("Beta"=10e4,"Sigma2"=c(3,1),"Tau2"=c(3,1)),
			  Inits=list("Beta"=rep(0,dim(X)[2]),"Sigma2"=1,"Tau2"=1),
			  NSims=9000,
			  NBurn=1000,
			  NThin=1,
			  Output="/Users/Sam/Documents/Sam/School/Graduate/PA/Output/") {
	
###Function Inputs
# Y=Y
# X=X
# W=W
# Hypers=list("Beta"=10e4,"Sigma2"=c(3,1),"Tau2"=c(3,1))
# Inits=list("Beta"=rep(0,dim(X)[2]),"Sigma2"=1,"Tau2"=1)
# NSims=9000
# NBurn=1000
# NThin=1
# Output="/Users/Sam/Documents/Sam/School/Graduate/PA/Output/"

###Load libraries
suppressMessages(library(mvtnorm)) #multivariate normal
suppressMessages(library(pscl)) #inverse gamma
suppressMessages(library(msm)) #truncated normal
suppressMessages(library(coda)) #mcmc

###Data objects
n<-dim(X)[1]
p<-dim(X)[2]

###Matrix objects
EyeP<-diag(p)
EyeN<-diag(n)

###Initial values
Beta<-matrix(Inits$Beta,nrow=p)
Sigma2<-Inits$Sigma2
Tau2<-Inits$Tau2
Theta<-matrix(rep(0,n),nrow=n)

###ICAR piece
Dw<-diag(apply(W,1,sum))
WStar<-Dw-W
G<-1 #Number of contiguous locations

###Hyperparameters
SigmaBeta2<-Hypers$Beta[1]
ASigma2<-Hypers$Sigma2[1]
BSigma2<-Hypers$Sigma2[2]
ATau2<-Hypers$Tau2[1]
BTau2<-Hypers$Tau2[2]

###MCMC objects
NTotal<-NBurn+NSims
NKeep<-NBurn+(1:(NSims/NThin))*NThin
OutProgress<-(1:10)*NTotal/10
write(1:n,file=paste(Output,"THETA.csv",sep=""),sep=",",ncolumns=n)
write(1:p,file=paste(Output,"BETA.csv",sep=""),sep=",",ncolumns=p)
write(1,file=paste(Output,"SIGMA2.csv",sep=""),sep=",",ncolumns=1)
write(1,file=paste(Output,"TAU2.csv",sep=""),sep=",",ncolumns=1)
write(1,file=paste(Output,"DEVIANCE.csv",sep=""),sep=",",ncolumns=1)
write(1:n,file=paste(Output,"PPD.csv",sep=""),sep=",",ncolumns=n)

###Verbose function
format2<-function(x) format(round(x,2),nsmall=2,width=6,justify="right")
format3<-function(x) format(round(x,3),nsmall=3,width=6,justify="right")

###Time MCMC sampler
begin<-Sys.time()

###Begin sampler
for (s in 1:NTotal) {
	
	###Theta full conditional
	CovTheta<-chol2inv(chol(EyeN/Sigma2+WStar/Tau2))
	MeanTheta<-CovTheta%*%((Y-X%*%Beta)/Sigma2)
	Theta<-matrix(rmvnorm(1,MeanTheta,CovTheta),nrow=n,ncol=1)
	if (s%in%NKeep) write(as.numeric(Theta),file=paste(Output,"THETA.csv",sep=""),sep=",",ncolumns=n,append=TRUE)
	
	##Beta full conditional
	CovBeta<-chol2inv(chol(t(X)%*%X/Sigma2+EyeP/SigmaBeta2))
	MeanBeta<-CovBeta%*%(t(X)%*%(Y-Theta)/Sigma2)
	Beta<-matrix(rmvnorm(1,MeanBeta,CovBeta),nrow=p,ncol=1)
	if (s%in%NKeep) write(as.numeric(Beta),file=paste(Output,"BETA.csv",sep=""),sep=",",ncolumns=p,append=TRUE)
	
	##Sigma2 full conditional
	AlphaSigma2<-n/2+ASigma2
	Gamma<-Y-(X%*%Beta+Theta)
	BetaSigma2<-BSigma2+t(Gamma)%*%Gamma/2
	Sigma2<-rigamma(1,AlphaSigma2,BetaSigma2)
	if (s%in%NKeep) write(Sigma2,file=paste(Output,"SIGMA2.csv",sep=""),sep=",",ncolumns=1,append=TRUE)
	
	##Tau2 full conditional
	AlphaTau2<-(n-G)/2+ATau2
	BetaTau2<-BTau2+t(Theta)%*%WStar%*%Theta/2
	Tau2<-rigamma(1,AlphaTau2,BetaTau2)
	if (s%in%NKeep) write(Tau2,file=paste(Output,"Tau2.csv",sep=""),sep=",",ncolumns=1,append=TRUE)

	##Compute deviance for DIC
	JointMean<-X%*%Beta+Theta
	Deviance<-(-2*dmvnorm(Y,JointMean,Sigma2*EyeN,log=TRUE))
	if (s%in%NKeep) write(Deviance,file=paste(Output,"DEVIANCE.csv",sep=""),sep=",",ncolumns=1,append=TRUE)

	##Sample from the posterior predictive distribution
	if (s%in%NKeep) write(rnorm(n,JointMean,Sigma2),file=paste(Output,"PPD.csv",sep=""),sep=",",ncolumns=n,append=TRUE)
	
	##Verbose
			
		##Scan number
		cat("Iteration: ",s,"\n",sep="")
		cat("\n",sep="")	
			
		##Current sample
		cat("Current Sample:\n",sep="")
		cat("\n",sep="")		
		cat("				Beta:     ",paste(format3(Beta)," ",sep=""),"\n")
		cat("				Sigma2:   ",paste(format3(Sigma2)," ",sep=""),"\n")
		cat("				Tau2: 	  ",paste(format3(Tau2)," ",sep=""),"\n")
		cat("				Deviance: ",paste(format3(Deviance)," ",sep=""),"\n")		
		cat("\n",sep="")	
		cat(rep("-",100),"\n",sep="")
		cat("\n",sep="")	

	###Time MCMC Sampler
	if (s==NTotal) {
		after<-Sys.time()
		time<-after-begin
		cat("Run Time:",paste0(format2(time)," "),"\n")				
	}
	
###End MCMC Sampler	
}

###Read in MCMC samples
Theta<-as.mcmc(read.csv(paste(Output,"THETA.csv",sep="")))
Beta<-as.mcmc(read.csv(paste(Output,"BETA.csv",sep="")))
Sigma2<-as.mcmc(read.csv(paste(Output,"SIGMA2.csv",sep="")))
Tau2<-as.mcmc(read.csv(paste(Output,"TAU2.csv",sep="")))
Deviance<-as.mcmc(read.csv(paste(Output,"DEVIANCE.csv",sep="")))
PPD<-as.mcmc(read.csv(paste(Output,"PPD.csv",sep="")))

###Compute diagnostics
##DIC, pD
ThetaPosterior<-apply(Theta,2,mean)
BetaPosterior<-apply(Beta,2,mean)
Sigma2Posterior<-apply(Sigma2,2,mean)
JointMeanPosterior<-X%*%BetaPosterior+ThetaPosterior
DHat<-(-2*dmvnorm(Y,JointMeanPosterior,Sigma2Posterior*EyeN,log=TRUE))
DBar<-apply(Deviance,2,mean)
pD<-DBar-DHat
DIC<-DBar+pD

##DInf, P, G
PPDPosteriorVariance<-apply(PPD,2,var)
PPDPosteriorMean<-apply(PPD,2,mean)
P<-sum(PPDPosteriorVariance)
G<-sum((PPDPosteriorMean-Y)^2)
D<-G+P
	
##Summarize diagnostics
Diagnostics<-c(DIC,pD,P,G,D)
names(Diagnostics)<-c("DIC","pD","P","G","D")

###Summarize posterior results
OutSummary<-cbind(Beta,Sigma2,Tau2)
OutputSummary<-cbind(apply(OutSummary,2,mean),apply(OutSummary,2,sd),t(apply(OutSummary,2,quantile,probs=c(0.025,0.5,0.975))))
colnames(OutputSummary)[c(1,2,4)]<-c("Mean","SD","Median")
rownames(OutputSummary)<-c(paste0("Beta",1:p),"Sigma2","Tau2")
OutputSummary<-round(OutputSummary,digits=3)

###Return object
FinalObject<-list(Theta,Beta,Sigma2,Tau2,Diagnostics,OutputSummary)
names(FinalObject)<-c("Theta","Beta","Sigma2","Tau2","Diagnositcs","Summary")
return(FinalObject)

}