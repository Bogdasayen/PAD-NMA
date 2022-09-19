# Function to convert timepoints reported by PAD to days

convert.pad.times.to.years<-function(timepoint.raw)
{
	timepoint.raw<-unlist(lapply(timepoint.raw,toString))

	timepoint.years<-timepoint.raw

	# Remove the non-numbers
	timepoint.years[timepoint.years=="Overall mean follow-up time was 352 days (range, 1–1,252 days) for PTA and 353 days (range, 1–1,215 days) for the stent placement group"]<-1
	timepoint.years[timepoint.years=="baseline"]<-0
	timepoint.years[timepoint.years=="Pre-discharge"]<-0	
	timepoint.years[timepoint.years=="During hospitalisation"]<-0
	timepoint.years[timepoint.years=="Baseline"]<-0
	timepoint.years[timepoint.years=="< = 36 months"]<-3
	timepoint.years[timepoint.years=="18m-24m"]<-1.75
	timepoint.years[timepoint.years=="Baseline (before PTA)"]<-0
	timepoint.years[timepoint.years=="0m (after surgery)"]<-0
	timepoint.years[timepoint.years=="Baseline (before surgery)"]<-0
	# Next is Saxon 2003 with comment in PAD data: "Patients were followed clinically for a mean of 37.8 months (95% CI: 27.7� 47.9; range, 6�48 months). Objective follow-up was performed for up to 2 years (mean, 22.9 months; range, 6�24 months; 26 patients were followed to the 2 year end-point). None of the patients were lost to follow-up."
	timepoint.years[timepoint.years=="Last follow-up"]<-37.8/12 
	timepoint.years[timepoint.years=="11 months ±11 (mean ± SD)"]<-11/12
	timepoint.years[timepoint.years=="7 months ± 10 (mean ± SD)"]<-7/12
	timepoint.years[timepoint.years=="-"]<-NA
	timepoint.years[timepoint.years=="Acute"]<-NA
	timepoint.years[timepoint.years=="acute"]<-NA
	timepoint.years[timepoint.years=="immediately after sugery"]<-0
	# Convert yrs to numbers
	year.index<-c(grep("yr",timepoint.years))
	for(i in year.index)
	{
		timepoint.years[i]<-
			substr(timepoint.years[i],start=0,stop=gregexpr("yr",timepoint.years[i])[[1]][1]-1)
	}
	years.index<-c(grep("yrs",timepoint.years))
	for(i in years.index)
	{
		timepoint.years[i]<-
			substr(timepoint.years[i],start=0,stop=gregexpr("yrs",timepoint.years[i])[[1]][1]-1)
	}

# These timepoints are not being converted correctly!
#timepoint.years[pad.data[,"Trial.ID"]=="21"]

	# Convert months (m) to years
	month.index<-c(grep("m",timepoint.years))
	for(i in month.index)
	{
		timepoint.years[i]<-
			as.numeric(substr(timepoint.years[i],start=0,stop=gregexpr("m",timepoint.years[i])[[1]][1]-1))/12
	}



	# Convert days (d) to years
	day.index<-c(grep("d",timepoint.years))
	for(i in day.index)
	{
		timepoint.years[i]<-
			as.numeric(substr(timepoint.years[i],start=0,stop=gregexpr("d",timepoint.years[i])[[1]][1]-1))/365
	}
	return(timepoint.years)
}