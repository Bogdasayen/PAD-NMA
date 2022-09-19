# Functions to extract and run NMA subgroup analyses
# Designed for ICL NMA rob subgroup analyses
# Howard Thom 4-April-2018
# Adapted to PAD 9-December-2019


# Example subgroup indicator for PAD data
#subgroup.indicator<-bugs.data[[4]][[1]][[3]]$rob[,"Overall Bias"]==3
#t.names<-names(treatment.codes[[4]][[1]][[3]])



# Function to build a bugs.data for the subgroup
extract.subgroup.data<-function(b.data, t.names, subgroup.indicator, reference.treatment="PTA")
{
	b.data.subgroup<-b.data
	na<-b.data$na[subgroup.indicator]
	r<-b.data$r[subgroup.indicator,]
	n<-b.data$n[subgroup.indicator,]
	times<-b.data$times[subgroup.indicator,]
	tr<-b.data$t[subgroup.indicator,]
	ns<-sum(subgroup.indicator)
	nt<-b.data$nt
	rob<-b.data$rob[subgroup.indicator,]
	
	# Renumber treatments so that they are consecutive
	# Only if any treatments have dissappeared

	nt<-length(unique(c(tr[!is.na(tr)])))
	if(b.data$nt!=nt)
	{
		tr.new<-matrix("",nrow=dim(tr)[1],ncol=dim(tr)[2])
		t.nonconsec<-unique(c(tr[!is.na(tr)]))
		for(i in 1:nt)
		{
			tr.new[tr==t.nonconsec[i]]<-i
		}
		tr<-tr.new
		tr[tr==""]<-NA
		# Rename the treatments
		t.names.subgroup<-t.names[t.nonconsec]

		# Ensure reference.treatment (default is PTA) is first 
		# Swap in both treatment matrices and treatment name list
		tr[tr==1 & !is.na(tr)]<-9999
		tr[tr==which(t.names.subgroup==reference.treatment) & !is.na(tr)]<-1
		tr[tr==9999 & !is.na(tr)]<-which(t.names.subgroup==reference.treatment)
		t.names.subgroup[which(t.names.subgroup==reference.treatment)]<-t.names.subgroup[1]
		t.names.subgroup[1]<-reference.treatment	

		# Ensure tr is a numeric
		tr.num<-matrix(nrow=dim(tr)[1],ncol=dim(tr)[2])
		for(i in 1:ns)
		{
			tr.num[i,]<-as.numeric(tr[i,])
		}
		tr<-tr.num
		rownames(tr)<-rownames(r)

		# Now ensure that each trial reports the treatments in the correct order
		# Need to sort matrices by the order of tr[i,]
		for(i in 1:ns)
		{
			n[i,]<-n[i,order(tr[i,])]
			times[i,]<-times[i,order(tr[i,])]
			r[i,]<-r[i,order(tr[i,])]
			tr[i,]<-tr[i,order(tr[i,])]
		}
	}else{t.names.subgroup<-t.names}

	# Create a BUGS data object (later updated to a connected subgroup)
	b.data.subgroup<-list("na"=na,"r"=r,"n"=n,"t"=tr,"ns"=ns,"nt"=nt,"times"=times, "rob"=rob)
	
	# Create a treatment codes vector (later updated to connected subgroup)
	t.codes.subgroup<-c(1:length(t.names.subgroup))
	names(t.codes.subgroup)<-t.names.subgroup


	# Now only export a connected subgroup of this subgroup
	# Draw the network plots
	x<-b.data.subgroup$t
	for(i in 1:dim(x)[1])
	{
		x[i,]<-i
	}
	t1<-c(x[!is.na(b.data.subgroup$t)])
	t2<-c(b.data.subgroup$t[!is.na(b.data.subgroup$t)])		

	# Temporary connectedness testing object
	c.test<-test.connectedness.fun(t1=t1,t2=t2,nameoftreatments=t.names.subgroup)
	# Now generate a subgroup connected to PTA
	c.temp<-create.treatment.subnetwork(b.data=b.data.subgroup,t.codes=t.codes.subgroup,
	t.subset=c.test$indirect.connection.matrix[,"PTA"]==1)

	# Form connected data for analysis
	b.data.subgroup.connected<-c.temp$b.data.subset
	t.codes.subgroup.connected<-c.temp$t.codes.subset
	
	# Export the connected subgroup and treatment codes
	return(list(b.data.subgroup=b.data.subgroup.connected, t.codes.subgroup=t.codes.subgroup.connected))
}

#test.output<-extract.subgroup.data(b.data=bugs.data[[4]][[1]][[3]],
#		t.names=names(treatment.codes[[4]][[1]][[3]]),
#		subgroup.indicator=bugs.data[[4]][[1]][[3]]$rob[,"Overall Bias"]==3)


#nma.output<-run.subgroup.nma(b.data.subgroup=test.output$b.data.subgroup,	
#			t.names=names(test.output$t.codes.subgroup),
#			num.sims=300,burn.in=300,bugs.debug=TRUE)


# Run a binary outcomes cloglog NMA with fixed and random effects
# Has to use 2 chains (or modify initial values)
run.subgroup.nma<-function(b.data.subgroup,t.names.subgroup,num.sims,burn.in,
			bugs.debug=FALSE, fixed=FALSE, random=TRUE)
{
	# Generate initial values
	# Initial values for models
	inits1<-list(d=c(NA,rep(1,b.data.subgroup$nt-1)),mu=rep(0.5,b.data.subgroup$ns),B=0.5,G=c(0.5,0.5))
	inits2<-list(d=c(NA,rep(0.5,b.data.subgroup$nt-1)),mu=rep(0.25,b.data.subgroup$ns),B=-0.5,G=c(-0.5,-0.5))
	inits1.re<-list(d=c(NA,rep(1,b.data.subgroup$nt-1)),mu=rep(-0.5,b.data.subgroup$ns),sd=1,B=0.5,G=c(0.5,0.5))
	inits2.re<-list(d=c(NA,rep(-1,b.data.subgroup$nt-1)),mu=rep(0.5,b.data.subgroup$ns),sd=0.5,B=-0.5,G=c(-0.5,-0.5))
	bugs.inits.fe<-list(inits1,inits2)
	bugs.inits.re<-list(inits1.re,inits2.re)

	output.list<-list()
	# Run BUGS for fixed and/or random effects models
	if(fixed){
		print("fixed")
		b.object.fe<-bugs(data=b.data.subgroup,
			inits=bugs.inits.fe,
			parameters.to.save=c("d","prob","rk","totresdev"),
			model=model.binomial.cloglog.fe,clearWD=TRUE,summary.only=FALSE,n.iter=(num.sims+burn.in),n.burnin=burn.in,n.chains=n.chains,bugs.seed=1,debug=bugs.debug)				
		output.list[["b.object.fe"]]<-b.object.fe
	}
	if(random){	
	print("random")
		b.object.re<-bugs(data=b.data.subgroup,
			inits=bugs.inits.re,
			parameters.to.save=c("d","sd","prob","rk","totresdev"),
			model=model.binomial.cloglog.re,clearWD=TRUE,summary.only=FALSE,n.iter=(num.sims+burn.in),n.burnin=burn.in,n.chains=n.chains,bugs.seed=1,debug=bugs.debug)
		output.list[["b.object.re"]]<-b.object.re
	}

	return(output.list)
}


