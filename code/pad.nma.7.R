# Script to draw PAD network plots and do basic analysis
# Howard Thom 01-September-2019

# Extended from pad.network.plots.3.R
# V2 uses only a connected subnetwork for analysis

# V3 updated to run both FP and InPop analyses

# TODO: Remove redundant 'time' from bugs.data as 'times' is the same

# V6 includes inconsistency checks and uses PAD_Dataset_10Oct2020_final_v2

# OpenBUGS interface
library(R2OpenBUGS)

# Load the NMA models
source("code/model.binomial.cloglog.1.R")
source("code/model.binomial.cloglog.inconsistency.1.R")
# Code to check for NA, NaN, or NULL
source("code/is.blank.1.R")
# Code to draw forest plots
source("code/draw.forest.plot.pad.4.R")

# Network plotting function
source("code/mtm.networkplot.fun.R")

# Network connectedness testing function
source("code/test.connectedness.fun.5.R")

# Comparison of deviance under consistency and inconsistency function
source("code/plot.inconsistency.dev.1.R")

# Subnetwork creation functions
# Subgroup based on treatments
source("code/create.treatment.subnetwork.2.R")
# Subgroup based on studies
source("code/nma.subgroup.analysis.1.R")



# Load formatted data (see "create.pad.nma.data.1.R")
# treatment.codes, bugs.data, outcome.names, time.pairs, lesion.locations
load(file="results/BUGS datasets/pad.bugs.data.10.rda")


# Hack to make sure code works for InPop 0 to 2y+
if(names(treatment.codes[[4]][[2]][[6]])[4] == "ns")
{
  treatment.codes[[4]][[2]][[6]] <- treatment.codes[[4]][[2]][[6]][-4]
}
                               
# MCMC Simulation settings
n.chains<-2		 # 2
num.sims=30000*n.chains  # 150000*n.chains
burn.in=30000*n.chains	 # 100000*n.chains

# List to store connected datasets and treatment lists
bugs.data.connected<-treatment.codes.connected<-list()
# Set up lists for all initial values and results
bugs.inits.fe<-bugs.inits.re<-bugs.object.fe<-bugs.object.re<-rank.summary.fe<-rank.summary.re<-list()
bugs.object.inconsistency.fe <- bugs.object.inconsistency.re <- list()

data.summary.matrix<-matrix(nrow=length(outcome.names)*length(lesion.locations)*length(time.pairs),ncol=5)
colnames(data.summary.matrix)<-c("N connected treatments","Disconnected treatments","N disconnected studies","N connected studies","N patients")
rownames(data.summary.matrix)<-paste(rep(outcome.names,each=length(lesion.locations)*length(time.pairs)),rep(lesion.locations,each=length(time.pairs)),names(time.pairs))

inconsistency.summary.matrix <- matrix(nrow=length(outcome.names)*length(lesion.locations)*length(time.pairs),ncol=11)
colnames(inconsistency.summary.matrix) <- c("N datapoints", "FE DIC", "FE totresdev", 
                                            "FE inconsistency DIC", "FE inconsistency totresdev", 
                                            "RE DIC", "RE totresdev", "RE sd",
                                            "RE inconsistency DIC", "RE inconsistency totresdev", "RE inconsistency sd")
rownames(inconsistency.summary.matrix)<-paste(rep(outcome.names,each=length(lesion.locations)*length(time.pairs)),rep(lesion.locations,each=length(time.pairs)),names(time.pairs))


for(i.outcome in c(1:length(outcome.names))[4])
{
	print(outcome.names[i.outcome])
	# Set up lists for the connected data and treatments
	bugs.data.connected[[i.outcome]]<-treatment.codes.connected[[i.outcome]]<-list()
	# Set up lists for initial values and results for this outcome
	bugs.inits.fe[[i.outcome]]<-bugs.object.fe[[i.outcome]]<-list()
	bugs.inits.re[[i.outcome]]<-list()
	bugs.object.re[[i.outcome]]<-rank.summary.fe[[i.outcome]]<-rank.summary.re[[i.outcome]]<-list()
	bugs.object.inconsistency.fe[[i.outcome]] <- bugs.object.inconsistency.re[[i.outcome]] <- list()
	
	for(i.lesion.location in c(1:length(lesion.locations)))
	{
		print(lesion.locations[i.lesion.location])
		# Set up lists for lesion locations on this outcome
		bugs.data.connected[[i.outcome]][[i.lesion.location]]<-treatment.codes.connected[[i.outcome]][[i.lesion.location]]<-
		bugs.inits.fe[[i.outcome]][[i.lesion.location]]<-bugs.object.fe[[i.outcome]][[i.lesion.location]]<-
		bugs.inits.re[[i.outcome]][[i.lesion.location]]<-bugs.object.re[[i.outcome]][[i.lesion.location]]<-
		rank.summary.fe[[i.outcome]][[i.lesion.location]]<-rank.summary.re[[i.outcome]][[i.lesion.location]]<-
		  bugs.object.inconsistency.fe[[i.outcome]][[i.lesion.location]] <- bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]] <- list()

		# Want to analyse 3m to 2y and >2y
		for(i.time.pair in c(1:length(time.pairs))[5:6])
		{
			print(names(time.pairs)[i.time.pair])
			# First check if there is any data
			if(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns!=0)
			{
			# Draw the network plots
			x<-bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t
			for(i in 1:dim(x)[1])
			{
				x[i,]<-i
			}
			t1<-c(x[!is.na(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t)])
			t2<-c(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t[!is.na(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t)])		

			win.metafile(file=paste0("results/final data network diagrams/",lesion.locations[i.lesion.location],"/",outcome.names[i.outcome],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".wmf"))
			mtm.networkplot.fun(t1=t1,t2=t2,percomparison=FALSE,nameoftreatments=names(treatment.codes[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			dev.off()
			jpeg(file=paste0("results/final data network diagrams/",lesion.locations[i.lesion.location],"/",outcome.names[i.outcome],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".jpg"),height=960,width=1450)
			mtm.networkplot.fun(t1=t1,t2=t2,percomparison=FALSE,nameoftreatments=names(treatment.codes[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			dev.off()
			
			
			# Temporary connectedness testing object
			c.test<-test.connectedness.fun(t1=t1,t2=t2,nameoftreatments=names(treatment.codes[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			# Now generate a subgroup connected to PTA
			c.temp<-create.treatment.subnetwork(b.data=bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]],t.codes=treatment.codes[[i.outcome]][[i.lesion.location]][[i.time.pair]],
				t.subset=c.test$indirect.connection.matrix[,"PTA"]==1)

			# Form connected data for analysis
			bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-c.temp$b.data.subset
			treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-c.temp$t.codes.subset
			
			# Draw the connected network plots
			x<-bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t
			for(i in 1:dim(x)[1])
			{
			  x[i,]<-i
			}
			t1<-c(x[!is.na(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t)])
			t2<-c(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t[!is.na(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$t)])		
			
			win.metafile(file=paste0("results/final data network diagrams/",lesion.locations[i.lesion.location],"/",outcome.names[i.outcome],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".connected..wmf"))
			mtm.networkplot.fun(t1=t1,t2=t2,percomparison=FALSE,nameoftreatments=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			dev.off()
			jpeg(file=paste0("results/final data network diagrams/",lesion.locations[i.lesion.location],"/",outcome.names[i.outcome],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".connected.jpg"),height=960,width=1450)
			mtm.networkplot.fun(t1=t1,t2=t2,percomparison=FALSE,nameoftreatments=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			dev.off()
			
			
			data.summary.matrix[paste(outcome.names[i.outcome],lesion.locations[i.lesion.location],names(time.pairs)[i.time.pair]),
				c("N connected treatments","Disconnected treatments","N disconnected studies","N connected studies","N patients")]<-
				c(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt,
				paste(names(treatment.codes[[i.outcome]][[i.lesion.location]][[i.time.pair]])[c.test$indirect.connection.matrix[,"PTA"]==0],collapse=","),
				bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns-bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns,
				bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns,
				sum(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$n,na.rm=TRUE))


			# Set initial values for the OpenBUGS MCMC simulations
			inits1<-list(d=c(NA,rep(1,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt-1)),mu=rep(0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns))
			inits2<-list(d=c(NA,rep(0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt-1)),mu=rep(0.25,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns))
			inits1.re<-list(d=c(NA,rep(1,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt-1)),mu=rep(-0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns),sd=1)
			inits2.re<-list(d=c(NA,rep(-1,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt-1)),mu=rep(0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns),sd=0.5)
			bugs.inits.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-list(inits1,inits2)	
			bugs.inits.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-list(inits1.re,inits2.re)

      # Initial values for inconsistency model
			# Initial values for inconsistency analysis
			# Inconsistency initial values for d
			d.matrix<-list()
			d.matrix[[1]]<-d.matrix[[2]]<-matrix(NA,nrow=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt,ncol=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt)
			for(i in 1:(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt-1))
			{
			  for(j in (i+1):bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt)
			  {
			    d.matrix[[1]][i,j]<-0.1
			    d.matrix[[2]][i,j]<--0.1
			  }
			}
			inits1<-list(d=d.matrix[[1]],mu=rep(0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns))
			inits2<-list(d=d.matrix[[2]],mu=rep(0.25,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns))
			inits1.re<-list(d=d.matrix[[1]],mu=rep(-0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns),sd=1)
			inits2.re<-list(d=d.matrix[[2]],mu=rep(0.5,bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns),sd=0.5)
			bugs.inits.inconsistency<-list(inits1,inits2)
			bugs.inits.inconsistency.re<-list(inits1.re,inits2.re)
			

			# Now run the simulations using a simple binomial-cloglog NMA model
			# Both fixed and random effects
			print("Fixed effects")
			bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-bugs(data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
				inits=bugs.inits.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]],
				parameters.to.save=c("d","prob","rk","totresdev"),
				model=model.binomial.cloglog.fe,clearWD=TRUE,summary.only=FALSE,n.iter=(num.sims+burn.in),n.burnin=burn.in,n.chains=n.chains,bugs.seed=1,debug=FALSE)			
			print("Fixed effects inconsistency")
			bugs.object.inconsistency.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-bugs(data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			                                                                      inits=bugs.inits.inconsistency,
			                                                                      parameters.to.save=c("dev", "resdev", "totresdev"),
			                                                                      model=model.binomial.cloglog.inconsistency.fe,clearWD=TRUE,summary.only=FALSE,n.iter=(num.sims+burn.in),n.burnin=burn.in,n.chains=n.chains,bugs.seed=1,debug=FALSE)			
			
			print("Random effects")
			bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-bugs(data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
				inits=bugs.inits.re[[i.outcome]][[i.lesion.location]][[i.time.pair]],
				parameters.to.save=c("d","sd","rk","totresdev", "prob"),
				model=model.binomial.cloglog.re,clearWD=TRUE,summary.only=FALSE,n.iter=(num.sims+burn.in),n.burnin=burn.in,n.chains=n.chains,bugs.seed=2,debug=FALSE)
			bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-bugs(data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			                                                                      inits=bugs.inits.inconsistency.re,
			                                                                      parameters.to.save=c("sd", "dev", "resdev", "totresdev"),
			                                                                      model=model.binomial.cloglog.inconsistency.re,clearWD=TRUE,summary.only=FALSE,n.iter=(num.sims+burn.in),n.burnin=burn.in,n.chains=n.chains,bugs.seed=2,debug=FALSE)
			
			# Plot a comparison of residual deviances
			jpeg(paste0("results/inconsistency deviance plots/",lesion.locations[i.lesion.location],"/inconsistency deviance.",outcome.names[i.outcome],".",lesion.locations[i.lesion.location],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".fe.jpg"))
			plot.inconsistency.dev(b.object = bugs.object.inconsistency.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			  b.object.inconsistency = bugs.object.inconsistency.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]])
			dev.off()
			jpeg(paste0("results/inconsistency deviance plots/",lesion.locations[i.lesion.location],"/inconsistency deviance.",outcome.names[i.outcome],".",lesion.locations[i.lesion.location],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".re.jpg"))
			plot.inconsistency.dev(b.object = bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			                       b.object.inconsistency = bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]][[i.time.pair]])
			dev.off()
			
			# Save model comparison and inconsistency assessment
			inconsistency.summary.matrix[paste(outcome.names[i.outcome],lesion.locations[i.lesion.location],names(time.pairs)[i.time.pair]),]<-
			  c(sum(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$na),
			    bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$DIC,
			    format.results(bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["totresdev",c(5,3,7)],n.digits=4),
			    bugs.object.inconsistency.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$DIC,
			    format.results(bugs.object.inconsistency.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["totresdev",c(5,3,7)],n.digits=4),
			    bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$DIC,
			    format.results(bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["totresdev",c(5,3,7)],n.digits=4),
			    format.results(bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["sd",c(5,3,7)]),
			    bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$DIC,
			    format.results(bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["totresdev",c(5,3,7)],n.digits=4),
			    format.results(bugs.object.inconsistency.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["sd",c(5,3,7)]))
			
			# Export a forest plot relative to PTA of results
			jpeg(paste0("results/forest plots/",lesion.locations[i.lesion.location],"/forest.",outcome.names[i.outcome],".",lesion.locations[i.lesion.location],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".fe.jpg"))
			draw.forest.plot(b.object=bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			                 t.names=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]),
						b.data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
						analysis.name=paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]), 
						reference.treatment = "PTA",
						display.p.value = FALSE)
			dev.off()
			win.metafile(paste0("results/forest plots/",lesion.locations[i.lesion.location],"/forest.",outcome.names[i.outcome],".",lesion.locations[i.lesion.location],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".fe.wmf"))
			draw.forest.plot(b.object=bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			                 t.names=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]),
			                 b.data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
			                 analysis.name=paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]), 
			                 reference.treatment = "PTA",
			                 display.p.value = FALSE)
			dev.off()
			
			win.metafile(paste0("results/forest plots/",lesion.locations[i.lesion.location],"/forest.",outcome.names[i.outcome],".",lesion.locations[i.lesion.location],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".re.wmf"))
			draw.forest.plot(b.object=bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]],t.names=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]),
						b.data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
						analysis.name=paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]), 
						reference.treatment = "PTA",
            display.p.value = FALSE)
			dev.off()
			jpeg(paste0("results/forest plots/",lesion.locations[i.lesion.location],"/forest.",outcome.names[i.outcome],".",lesion.locations[i.lesion.location],".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".re.jpg"))
			draw.forest.plot(b.object=bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]],t.names=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]),
			                 b.data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],analysis.name=paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]), reference.treatment = "PTA",
			                 display.p.value = FALSE)
			dev.off()
	
	

			# Save the median ranks
			# Fixed effects
			raw.rank.matrix<-bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary[grep("rk",rownames(bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary)),c(5,3,7)]
			# Name rows after the treatments
			rownames(raw.rank.matrix)<-names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]])
			# Sort by median rank
			raw.rank.matrix<-raw.rank.matrix[order(raw.rank.matrix[,1]),]
			# Now convert to a nice format
			rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-matrix(nrow=dim(raw.rank.matrix)[1],ncol=1)
			rownames(rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]])<-rownames(raw.rank.matrix)
			for(i in 1:length(rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			{	
				rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]][i]<-paste0(raw.rank.matrix[i,1]," (",raw.rank.matrix[i,2],", ",raw.rank.matrix[i,3],")")
			}

			# Random effects
			raw.rank.matrix<-bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary[grep("rk",rownames(bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary)),c(5,3,7)]
			# Name rows after the treatments
			rownames(raw.rank.matrix)<-names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]])
			# Sort by median rank
			raw.rank.matrix<-raw.rank.matrix[order(raw.rank.matrix[,1]),]
			# Now convert to a nice format
			rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]<-matrix(nrow=dim(raw.rank.matrix)[1],ncol=1)
			rownames(rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]])<-rownames(raw.rank.matrix)
			for(i in 1:length(rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]))
			{	
				rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]][i]<-paste0(raw.rank.matrix[i,1]," (",raw.rank.matrix[i,2],", ",raw.rank.matrix[i,3],")")
			}

			}else{ # End if no data		
				print("No data")
			}
		} # End loop over time pair
	} # End loop over lesion location
} # End loop over outcomes

# Save the connected datasets for use in piecewise constant models
save(bugs.data.connected, treatment.codes.connected, file=paste0("results/bugs.data.connected.pad.2.R" ))

# Save the bugs objects for later use
save(bugs.object.fe,bugs.object.re,file=paste0("results/bugs.objects.pad.5.R"))


# Export the data summary
write.csv(data.summary.matrix,file=paste0("results/data summary/data.summary.pad.5.csv"))

# Code to put all the rank summaries together in a single table
for(i.lesion.location in c(1:length(lesion.locations)))
{
	ranking.matrix.fe<-ranking.matrix.re<-matrix(nrow=50,ncol=2*length(outcome.names)*length(time.pairs))
	col.names<-c()
	for(i.outcome in c(1:length(outcome.names)))
	{
		for(i.time.pair in c(1:length(time.pairs)))
		{
			col.names<-c(col.names,rep(paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]),2))	
			if(i.outcome==4 & is.element(i.time.pair,c(5,6))){
			# Fixed effects
			ranking.matrix.fe[1:length(rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]),2*(length(time.pairs)*(i.outcome-1)+i.time.pair)-1]<-rownames(rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]])
			ranking.matrix.fe[1:length(rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]),2*(length(time.pairs)*(i.outcome-1)+i.time.pair)]<-rank.summary.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]
			# Random effects
			ranking.matrix.re[1:length(rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]),2*(length(time.pairs)*(i.outcome-1)+i.time.pair)-1]<-rownames(rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]])
			ranking.matrix.re[1:length(rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]),2*(length(time.pairs)*(i.outcome-1)+i.time.pair)]<-rank.summary.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]	
			}
		} # End loop over time pair
	} # End loop over outcomes
	colnames(ranking.matrix.fe)<-colnames(ranking.matrix.re)<-col.names
	# Export the ranking summaries
	write.csv(ranking.matrix.fe,paste0("results/ranks/",lesion.locations[i.lesion.location],".ranking.matrix.fe.csv"))
	write.csv(ranking.matrix.re,paste0("results/ranks/",lesion.locations[i.lesion.location],".ranking.matrix.re.csv"))
} # End loop over lesion location



# Extract model assessment statistics
# This actually just duplicates what's in inconsistency.summary.matrix so instead now saving that
write.csv(inconsistency.summary.matrix,paste0("results/model.assessment.pad.csv"))

model.assessment<-matrix(nrow=length(outcome.names)*length(lesion.locations)*length(time.pairs),ncol=6)
colnames(model.assessment)<-c("N datapoints","FE DIC","FE Totresdev","RE DIC","RE Totresdev","sd")
rownames(model.assessment)<-paste(rep(outcome.names,each=length(lesion.locations)*length(time.pairs)),rep(lesion.locations,each=length(time.pairs)),names(time.pairs))
for(i.outcome in c(1:length(outcome.names))[4])
{
	for(i.lesion.location in c(1:length(lesion.locations)))
	{
		for(i.time.pair in c(1:length(time.pairs))[5:6])
		{
		# First check if there is data and that analyses were conducted
		if(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns!=0){
		model.assessment[paste(outcome.names[i.outcome],lesion.locations[i.lesion.location],names(time.pairs)[i.time.pair]),]<-
			c(sum(bugs.data[[i.outcome]][[i.lesion.location]][[i.time.pair]]$na),
			bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$DIC,
			format.results(bugs.object.fe[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["totresdev",c(5,3,7)],n.digits=4),
			bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$DIC,
			format.results(bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["totresdev",c(5,3,7)],n.digits=4),
			format.results(bugs.object.re[[i.outcome]][[i.lesion.location]][[i.time.pair]]$summary["sd",c(5,3,7)]))
		} # End if no data
		} # End loop over time pairs
	} # End loop over lesion location
} # End loop over outcomes



# Sensitivity analyses on risk of bias?
# Only run for the final selected model (random effects)
# And only for the primary network (FP, 3 months to 2 years or 2y+, patency or restenosis)
for(i.outcome in c(1:length(outcome.names))[4])
{
  for(i.lesion.location in c(1:length(lesion.locations)))
  {
    for(i.time.pair in c(1:length(time.pairs))[5:6])
    {
      print(names(time.pairs)[i.time.pair])
      if(i.outcome==4 & (i.time.pair==5 | i.time.pair==6) & i.lesion.location==1)
      #if(0)
      {
        # Exclude studies at high risk of bias in four different domains
        # note that rob 1= low, 2= some concerns, 3= high
        for(rob.domain in c( "Randomization process","Mising outcome data", "Measurement of the outcome",
                             "Overall Bias"))
        {
          print(rob.domain)
          # How many studies included?
          print(paste0("Including ",sum(bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$rob[,rob.domain]!=3),
                       " of ",bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$ns," studies"))
          # Connected subgroup of non-high risk studies
          rob.subgroup.data<-extract.subgroup.data(b.data=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]],
                                                   t.names=names(treatment.codes.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]),
                                                   subgroup.indicator=bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$rob[,rob.domain]!=3)
          
          # Give report on connected studies and treatments
          print(paste("Only ",rob.subgroup.data$b.data.subgroup$ns," studies connected"))
          print(paste(rob.subgroup.data$b.data.subgroup$nt," of ",bugs.data.connected[[i.outcome]][[i.lesion.location]][[i.time.pair]]$nt,
                      " treatments included"))
          # Run random effects analysis
          rob.subgroup.results<-run.subgroup.nma(b.data.subgroup=rob.subgroup.data$b.data.subgroup,	
                                                 t.names=names(rob.subgroup.data$t.codes.subgroup),
                                                 num.sims=num.sims,burn.in=burn.in,bugs.debug=FALSE,random=TRUE, fixed=FALSE)
          # Draw a forest plot of the results
          jpeg(filename=paste0("results/forest plots/",
                               lesion.locations[i.lesion.location], "/subgroups/forest.", rob.domain,
                               ".",outcome.names[i.outcome],".", lesion.locations[i.lesion.location],
                               ".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".re.jpg"))
          draw.forest.plot(b.object=rob.subgroup.results$b.object.re,t.names= names(rob.subgroup.data$t.codes.subgroup),
                           b.data=rob.subgroup.data$b.data.subgroup,analysis.name=paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]), reference.treatment = "PTA",
                           display.p.value = FALSE)
          dev.off()
          win.metafile(filename=paste0("results/forest plots/",
                                       lesion.locations[i.lesion.location], "/subgroups/forest.", rob.domain,
                                       ".",outcome.names[i.outcome],".", lesion.locations[i.lesion.location],
                                       ".",gsub(">2y","2y+",names(time.pairs))[i.time.pair],".re.wmf"))
          draw.forest.plot(b.object=rob.subgroup.results$b.object.re,t.names= names(rob.subgroup.data$t.codes.subgroup),
                           b.data=rob.subgroup.data$b.data.subgroup,analysis.name=paste(outcome.names[i.outcome],names(time.pairs)[i.time.pair]), reference.treatment = "PTA",
                           display.p.value = FALSE)
          dev.off()
          
        }
      }
    } # End loop over time pairs
  } # End loop over lesion location
} # End loop over outcomes


























