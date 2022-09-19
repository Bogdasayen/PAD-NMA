# Function to create a subnetwork only of specific treatments
# Works on binomial-cloglog data but just remove 'time' for binomial-logistic
# Used in PAD to create connected subsets

# Howard Thom 3-Sep-2019

# V2 Updated to also subset the risk of bias matrix rob

create.treatment.subnetwork<-function(b.data,t.codes,t.subset)
{
	# Which studies a treatment of interest
	subset.studies<-rep(0,b.data$ns)
	for(i in 1:b.data$ns)
	{
		# Is one of the treatments of interest?
		if(sum(is.element(b.data$t[i,],t.codes[t.subset]))>0){
			subset.studies[i]=1
		}
	}
	# Names of the included/excluded studies
	included.studies<-rownames(b.data$t[subset.studies==1,])
	excluded.studies<-rownames(b.data$t[subset.studies!=1,])
	# Numbers of included/excluded studies
	ns.included<-length(included.studies)	
	ns.excluded<-length(excluded.studies)

	# Now create the subset of interest
	b.data.subset<-b.data
	b.data.subset$ns<-sum(subset.studies)
	b.data.subset$t<-b.data$t[subset.studies==1,]
	b.data.subset$r<-b.data$r[subset.studies==1,]
	b.data.subset$n<-b.data$n[subset.studies==1,]
	b.data.subset$times<-b.data$times[subset.studies==1,]
	b.data.subset$na<-b.data$na[subset.studies==1]
	b.data.subset$rob<-b.data$rob[subset.studies==1,]
	b.data.subset$nt<-sum(t.subset)

	# Subet of treatment codes and names
	t.codes.subset<-t.codes[t.subset]
	
	# Now relabel the treatments
	for(i in 1:length(t.codes.subset))
	{
		# Map to unique consecutive number
		b.data.subset$t[b.data.subset$t==t.codes.subset[i]]<-9999+i
	}
	# Map back to 1:nt
	b.data.subset$t<-b.data.subset$t-9999

	t.codes.subset[1:length(t.codes.subset)]<-1:length(t.codes.subset)

	return(list("b.data.subset"=b.data.subset,"t.codes.subset"=t.codes.subset))
}