# Data cleaning funtions for the PAD dataset
# Howard Thom 1-August-2019

# Updated to ensure consistency with dataset shared on 8-August-2019

# Function to extract patient numbers
# Uses n measured if available and n randomized if not
# This is for use in binomial-cloglog model with number at risk as the unit
harmonise.pad.patients<-function(n.patients.raw)
{
	n.patients.clean<-rep(NA, dim(n.patients.raw)[1])
	n.studies<-length(n.patients.clean)
	for(i in 1:n.studies)
	{
		n.patients.clean[i]<-as.numeric(toString(n.patients.raw[i,"Measured.N"]))
		# Try some cleaning
		if(is.na(n.patients.clean[i]) | n.patients.clean[i]=="")
		{
			n.patients.clean[i]<-substr(toString(n.patients.raw[i,"Measured.N"]),start=0,	
					stop=regexpr("at risk",toString(n.patients.raw[i,"Measured.N"]))[1]-2)
		}
		if(is.na(n.patients.clean[i]) | n.patients.clean[i]=="")
		{
			n.patients.clean[i]<-substr(toString(n.patients.raw[i,"Measured.N"]),start=0,	
					stop=regexpr("(N)",toString(n.patients.raw[i,"Measured.N"]))[1]-2)
		}
		# If still not reported, use the number randomized
		if(is.na(n.patients.clean[i]) | n.patients.clean[i]=="")
		{
			n.patients.clean[i]<-as.numeric(toString(n.patients.raw[i,"Randomised.N"]))
		}
	}
	return(n.patients.clean)
}

# Function to convert all percentages to number of events in PAD data
# Exports vectors of events and percentage
convert.pad.percentage.to.n<-function(events.raw,data.type,n.patients.clean)
{
	events.raw<-unlist(lapply(events.raw,toString))
	n.events.clean<-matrix(NA, nrow=2, ncol=length(events.raw))
	rownames(n.events.clean)<-c("Events","Percentage")
	n.studies<-length(events.raw)
	for(i.study in 1:n.studies)
	{
		n.events.clean["Events", i.study]<-as.numeric(events.raw[i.study])
		# If the events are a percentage
		if(length(grep("%",data.type[i.study]))!=0 & !is.na(n.events.clean["Events",i.study]))
		{
			# If it's a percentage out of 100 then need to scale (otherwise it's already on 0-1 scale)
			# Now convert to number of events
			if(n.events.clean["Events",i.study]>1){
				n.events.clean["Percentage",i.study]<-n.events.clean["Events",i.study]/100
			}else{
				n.events.clean["Percentage",i.study]<-n.events.clean["Events",i.study]				
			}
			# Now convert to number of events
			n.events.clean["Percentage",i.study]<-n.events.clean["Percentage",i.study]*as.numeric(n.patients.clean[i.study])
		}
	}
	# Set the events to NA if it isn't event data
	n.events.clean["Events",data.type!="n"]<-NA
	return(n.events.clean)
}



