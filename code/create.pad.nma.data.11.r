# Script to create BUGS compatible PAD datasets
# Howard Thom 2-August-2019

# V2 updated work with pad dataset shared on 8-August-2019
# Renamed to "create.pad.nma.data.1.R" from pad network plot function. It had gotten to be a very long file.

# V2 updated to use 10th October data extraction sheet and exclude studies with ID 98 and 440
# Also excluded IDs 98 and 440: They were excluded because they included patients with iliac arteries (>20%) and no subset results were available.

# V3 uses percentage data only if actual number of events not reported.
# This avoids anomaly of duplicated arms (as in 4448)

# V4 uses Dec 6th data extraction which corrects an error in Rastan 2013 ID 493 data
# V5 adds an InPop group

# V6 uses Jan 3rd dataset
# v8 uses PAD_Dataset_10Oct2019_final
# V9 uses the 14th July 2021 KM data and PAD_Dataset_10Oct2020_final
# V10 modifies time pairs to have a starting time for counting
# V10 also uses PAD_Dataset_10Oct2020_final_v2
# previously counted only events between lower and upper but can now count events up to the timepoint between lower and upper


# TODO go through every row of multiple.timepoint.studies and ensure no duplicates
# Probably need to do the same for TLR as well as patency. E.g. ID 770 has 4 duplicates for every arm and timepoint. 
# TODO check that does Tepe 2015 use Outcome = TLR, TLR-CD, and TLR-CD free from? Or just TLR?

# TODO On Monday: See why Tepe 2015 patency is including 8 rather than 6 arms.

# NOTE: When combining patency, restenosis, and TLR and if an arm reports TLR but not restenosis, made decision to still
# only use restenosis. This applies to study 892 where 'DCM' has TLR data but not restenosis data

# Check with Vincent correct to remove rows where group and trt are blank (i.e. arm.treatments== "NA")
# Unsure about 6674 BMS being labelled BS rather than MS?



# Switched from xlsx package to readxl as it is faster and memory efficient
# Old code written for xlsx so needed to rename columns of pad.data
library(readxl)

# Function to convert times to years
source("code/clean.pad.data.functions.3.R")
source("code/convert.pad.times.to.years.2.R")
# Network plot function
source("code/mtm.networkplot.fun.R")
# Check if NA or NULL
source("code/is.blank.1.R")

# Function to choose favoured analysis (e.g. ITT over unclear)
source("code/pad.favoured.analysis.1.R")

# Load PAD outcomes and RoB data
pad.data<-read_excel(paste0("data/PAD_Dataset_10Oct2020_final_v2.xlsx"),sheet="Outcome")
rob.data<-read_excel(paste0("data/PAD_Dataset_10Oct2020_final_v2.xlsx"),sheet="RoB")
pad.data<-as.matrix(pad.data)
rob.data<-as.matrix(rob.data)

# Only keep unique rows
pad.data <- unique(pad.data)

# Load the PAD KM summary
km.summary <- read_excel(paste0("data/Multiple study timepoints to check FINAL.xlsx"), sheet=1)
km.summary <- as.matrix(km.summary)
km.filename<-paste0("data/pad.guyot.2.xlsx")
km.trial.list <- as.matrix(read_excel(km.filename, sheet = "KM data list"))
# Remove white space
km.summary[, "Study ID"] <- trimws(km.summary[, "Study ID"])
rownames(km.summary) <- km.summary[, "Study ID"]
km.summary[, "Associated ID"] <- trimws(km.summary[, "Associated ID"])
# Remove NA rows
km.summary <- km.summary[!is.na(km.summary[, "Study ID"]), ]

# For compatability with excel sheet loaded using read.xlsx need to rename columns
colnames(pad.data)<-c("Trial.ID","Associate.IDs", "First.Author", "PubYear", "Category", "Outcome", "Count.by", "Time.point", 
"Unit..measurement.", "Class", "Trt", "Group..not.use.", "Lesion.location", "Analysis", "Data.Type", "Randomised.N", 
"Measured.N", "Observed.Events.or.rate..N.or...", "Dispersion", "Lower.limit", "Upper.limit", "Comment.1..Stat..",
"Comment.2..Others.", "Comment.Extra", "Source")

# Also remove whitesapce from trial ids
pad.data[,"Trial.ID"]<-trimws(pad.data[,"Trial.ID"])
rob.data[,"Unique ID"]<-trimws(rob.data[,"Unique ID"])

# Cut off the NAs (none in the 8-Aug dataset)
pad.data<-pad.data[rowSums(is.na(pad.data))!=dim(pad.data)[2],]

# Remove the unneeded alternative patency and TLR definitions
source("code/pad.remove.unwanted.patency.2.R")
source("code/pad.remove.unwanted.tlr.1.R")


# Exclude IDs 98 and 440 as they included patients with iliac arteries (>20%) and no subset results were available.
# They are likely already exluded due to high risk of bias
pad.data<-pad.data[!is.element(pad.data[,"Trial.ID"], c("98","440")),]

# Rastan 2013 (ID 493) uses  type 1, in which provisional stent placement was regarded as a loss of patency, 
# and type 2, in which provisional stent placement did not constitute a loss of patency).
# We've simply excluded all type 2 measurements.
pad.data<-pad.data[-grep("type 2",pad.data[,"Comment.2..Others."]),]

# Remove any rows which duplicate the data
source("code/pad.remove.duplicate.rows.1.R")

# treatments
# Use the Class if not "Combined" and Trt if "Combined"
# If the class and trt are NA use Group ("Group..not.use.")
arm.treatments<-unlist(lapply(pad.data[,"Class"],toString))
arm.treatments[arm.treatments=="Combined"]<-unlist(lapply(pad.data[arm.treatments=="Combined","Trt"],toString))

# Remove studies with NA
pad.data <- pad.data[arm.treatments != "NA"  & !is.na(arm.treatments), ]
arm.treatments <- arm.treatments[arm.treatments != "NA" & !is.na(arm.treatments)]


# Count the number of studies (38 (85 including high risk of bias) in total)
# Went up to 104 after 10October update (v8 of code)
total.ns<-length(unique(pad.data[,"Trial.ID"],na.rm=TRUE))
trial.ids<-unique(pad.data[,"Trial.ID"],na.rm=TRUE)
trial.ids <- trial.ids[!is.na(trial.ids)]

# Need to divide studies into 1-3 months (excl. 30 days), 3 months to 2 years, and =2 years
# But first have to convert all timepoints to an annual scale so categories are <0.25, 0.25-2, >2
pad.data[,"Time.point"]<-timepoint.years<-convert.pad.times.to.years(timepoint.raw=pad.data[,"Time.point"])

# Need a matrix that indicates if there is KM data for this study and the upper time limit for such data
km.data.timepoints <- as.data.frame(matrix(NA, nrow = dim(pad.data)[1], ncol = 3))
colnames(km.data.timepoints) <- c("Trial.ID", "KM data", "Latest time years")
km.data.timepoints[, "Trial.ID"] <- pad.data[, "Trial.ID"]
km.data.timepoints[, "KM data"] <- FALSE
km.study.name.list <- km.trial.list
for(i in 1:length(km.study.name.list)) {
  km.study.name.list[i] <- substr(km.study.name.list[i], start = 0, stop = regexpr("_", km.study.name.list[i])[[1]] - 1)
}
for(study.name in unique(km.data.timepoints[, "Trial.ID"])) {
  km.trial.ids <- NA
  treatment.names <- unique(arm.treatments[km.data.timepoints[, "Trial.ID"] == study.name])
  # Hack to make sure it works with trials that have only one unique treatment
  # These will be omitted later so unimportant
  if(length(treatment.names) == 1)treatment.names <- paste0(rep(treatment.names, 2), c("_1", "_2"))
  if(is.element(study.name, rownames(km.summary))) {
    km.data.timepoints[km.data.timepoints[, "Trial.ID"] == study.name, "KM data"] <- km.summary[study.name, "Is KM data extracted"] == "Yes"
    if(km.summary[study.name, "Is KM data extracted"] == "Yes") {
      km.trial.ids <- paste0(rep(km.summary[km.summary[, "Study ID"] == study.name, "Associated ID"], 2),
                             "_",treatment.names, ".txt")
    }
  }
  if(is.element(study.name, km.study.name.list)) {
    km.data.timepoints[km.data.timepoints[, "Trial.ID"] == study.name, "KM data"] <- TRUE
    km.trial.ids <- paste0(study.name, "_",treatment.names, ".txt")
  }
  # Now read the KM data and check latest timepoint
  if(!is.na(km.trial.ids[1])) {
    max.timepoints <- rep(NA, length(km.trial.ids)) 
    for(i in 1:length(km.trial.ids)) {
      km.dataset.temp <- read_excel(km.filename, sheet = km.trial.ids[i])
      max.timepoints[i] <- max(km.dataset.temp[, "t.S"])
    }
    # Convert to years
    km.data.timepoints[km.data.timepoints[, "Trial.ID"] == study.name, "Latest time years"] <- max(max.timepoints) / 12
  }
}

# Harmonise the numbers of patients, whether they are n randomised or n measured.
# Use n measured if available
n.patients.clean<-harmonise.pad.patients(pad.data[,c("Randomised.N","Measured.N")])

# Convert the number of events from % to n if % are reported
n.events.clean<-convert.pad.percentage.to.n(events.raw=pad.data[,"Observed.Events.or.rate..N.or..."],data.type=pad.data[,"Unit..measurement."],n.patients.clean=n.patients.clean)


# Only analyse the following three outcomes
outcome.categories<-c("Patency","Restenosis","TLR")
# Analyse studies reporting in times between 1st and 2nd, 2nd and 3rd, and above 3rd.
cutoff.times<-c(31/365,3/12,2,1000)

# Only analyse femoropopliteal and In popliteal
lesion.locations<-c("FP","InPop")

# Data structure to store results
bugs.data<-list()
treatment.codes<-list()

time.pairs<-list(c(1,4),c(1,2),c(2,3),c(3,4),c(2,3),c(3,4))
# TRUE: Count events from 0 to the timepoint between lower and upper
# FALSE: Only count events that occur between lower and upper
time.pairs.start.zero <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
names(time.pairs)<-c("All timepoints","30d to 3m","3m to 2y",">2y", "0 to 3m-2y", "0 to 2y+")
names(time.pairs.start.zero)
# First entry is lower limit and will be 1, 1, 2, 3 for all timepoints, 30 days to 3 months, 3 months to 2 years, >2 years
# Second entry is upper limit 4, 2, 3, 4 for all timepoints, 30 days to 3 months, 3 months to 2 years, >2 years

zero.event.studies <- matrix(NA, nrow = length(outcome.categories) * length(lesion.locations) * length(time.pairs), ncol = 5)
colnames(zero.event.studies) <- c("Outcome", "Lesion location", "Time period", "Studies removed", "Treatments in studies")
zero.event.studies[, "Outcome"] <- rep(outcome.categories, each = length(lesion.locations) * length(time.pairs))
zero.event.studies[, "Lesion location"] <- rep(rep(lesion.locations, each = length(time.pairs)), length(outcome.categories))
zero.event.studies[, "Time period"] <- rep(names(time.pairs), length(outcome.categories) * length(lesion.locations))
zero.event.studies <- as.data.frame(zero.event.studies)

# Create for all outcomes (combined outcomes are done afterwards)
for(i.outcome in 1:length(outcome.categories))
{
	# Data structure to store results
	bugs.data[[outcome.categories[i.outcome]]]<-list()
	treatment.codes[[outcome.categories[i.outcome]]]<-list()

	# FP and InPop
	for(i.lesion.location in 1:length(lesion.locations))
	{
	bugs.data[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]]<-list()
	treatment.codes[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]]<-list()

	for(i.time.pair in 1:length(time.pairs))
	{

	# Set the upper and lower limits on timepoints
	i.time.lower<-time.pairs[[i.time.pair]][1]
	i.time.upper<-time.pairs[[i.time.pair]][2]


	# Rows reporting patency in the appropriate lesion and for the appropriate timepoint
	# Either aggregate data reported within this time interval
	# or KM data later than lower bound of this interval
	row.index<-pad.data[,"Category"]==outcome.categories[i.outcome] & 
	  pad.data[,"Lesion.location"]==lesion.locations[i.lesion.location] &
			 ((as.numeric(timepoint.years)>=cutoff.times[i.time.lower] & as.numeric(timepoint.years)<cutoff.times[i.time.upper]) |
			    (km.data.timepoints$`KM data` & km.data.timepoints$`Latest time years` >= cutoff.times[i.time.lower]))

	# Only include a row if there is non-NA numbers of patients and events (And no KM data)
	row.index<-row.index & (((!is.na(n.events.clean["Events",]) | !is.na(n.events.clean["Percentage",])) & !is.na(n.patients.clean)) |
	  km.data.timepoints$`KM data`)

	# Also don't include a row if the arm.treatment is blank
	row.index<-row.index & arm.treatments!="NA"
	
	# And only include studies that report more than one treatment for this outcome
	# This may need to be updated as Studies 123, 4391 and 4552 should be included as they have KM data
	# for both arms.
	index.to.remove <- c()
	for(i.id in unique(pad.data[row.index,"Trial.ID"])) {
	  # Is this a single treatment study?
	  study.arm.treatments <- arm.treatments[row.index][pad.data[row.index, "Trial.ID"] == i.id]
	  
	  if(length(unique(study.arm.treatments[!is.na(study.arm.treatments)])) == 1) {
	    # Remove if yes
	    print(paste0("Removing: ", i.id, ". Single treatment in study for outcome ", outcome.categories[i.outcome]))
	    index.to.remove <- c(index.to.remove, i.id)
	    
	  }
	}
	row.index[is.element(pad.data[, "Trial.ID"], index.to.remove)] <- FALSE 
	
	# If a row.index is NA set it to FALSE
	row.index[is.na(row.index)] <- FALSE

	subset.trial.ids<-unique(pad.data[row.index,"Trial.ID"])
	ns<-length(subset.trial.ids)
	
	# Create a table of studies with multiple timepoints
	# This is to identify those for which we may need to use KM data
	multiple.timepoint.studies <- as.data.frame(matrix(0, nrow = ns, ncol = 4))
	colnames(multiple.timepoint.studies) <- c("Trial.ID", "n.timepoints", "n.arms", "time.per.arm")

	# Only use the latest timepoint in this range
	rows.to.keep<-c()
	for(i.study in 1:ns)
	{
		# Now loop through the interventions
		study.treatments<-unique(arm.treatments[row.index][pad.data[row.index,"Trial.ID"]==subset.trial.ids[i.study]])
		# Remove NA from the study treatments
		study.treatments <- study.treatments[!is.na(study.treatments)]
		
		multiple.timepoint.studies[i.study, "Trial.ID"] <- subset.trial.ids[i.study]
		multiple.timepoint.studies[i.study, "n.arms"] <- length(study.treatments)
		for(arm.treatment in study.treatments)
		{
			# Timepoints reported in this range, for this study, and this treatment
			reported.timepoints<-as.numeric(timepoint.years[row.index][pad.data[row.index,"Trial.ID"]==subset.trial.ids[i.study] &
						arm.treatments[row.index]==arm.treatment])
			# Remove NA
			reported.timepoints <- reported.timepoints[!is.na(reported.timepoints)]
			
			# Remove all but the favoured analysis type if multiple datapoints for the same timepoint
			favoured.analysis.index <- which.favoured.analysis(
			  analysis.types = pad.data[row.index, "Analysis"][pad.data[row.index,"Trial.ID"]==subset.trial.ids[i.study] &
          arm.treatments[row.index]==arm.treatment],
			  analysis.timepoints = reported.timepoints)
			
			# If multiple favoured analyses at the same timepoint then indicates multiple arms
			# with the same treatment (those with only one treatment across arms have been removed)
			multiple.timepoint.studies[i.study, "n.arms"]  <- multiple.timepoint.studies[i.study, "n.arms"]  +
        max(table(reported.timepoints[favoured.analysis.index])) - 1
			
			
      multiple.timepoint.studies[i.study, "n.timepoints"] <- multiple.timepoint.studies[i.study, "n.timepoints"] + length(reported.timepoints[favoured.analysis.index])

			# Are there multiple timepoints in this range for this study and this treatment?
			if(length(reported.timepoints)>1){
				# Only keep the row corresponding to this trial and treatment
			  rows.to.keep.new <- which(pad.data[row.index,"Trial.ID"]==subset.trial.ids[i.study] &
			                              arm.treatments[row.index]==arm.treatment)
			  # Only keep favoured analyses within each timepoint
			  rows.to.keep.new <- rows.to.keep.new[favoured.analysis.index]
			  # Only keep rows corresponding to the maximum timepoint
			  rows.to.keep.new <- rows.to.keep.new[is.element(rows.to.keep.new, 
          which(as.numeric(timepoint.years[row.index])==max(reported.timepoints)))]
			  rows.to.keep<-c(rows.to.keep,rows.to.keep.new)
			}else{
				# Otherwise include all rows
				rows.to.keep<-c(rows.to.keep,which(pad.data[row.index,"Trial.ID"]==subset.trial.ids[i.study] &
						arm.treatments[row.index]==arm.treatment))
			}
		} # End loop over study treatments
	} # End loop over i.study
	# Calculate timepoints per arm (anything greater than 1 needs care)
	multiple.timepoint.studies[, "time.per.arm"] <- multiple.timepoint.studies[, "n.timepoints"] /  multiple.timepoint.studies[, "n.arms"]
	# Finally, update the row indices
	row.index[row.index]<-is.element(1:sum(row.index),rows.to.keep)
	

	
	# How many arms?
	na<-rep(NA, ns)
	for(i in 1:ns)
	{
		na[i]<-sum(pad.data[row.index,"Trial.ID"]==subset.trial.ids[i])
	}


	# Only include studies with at least 2 arms
	subset.trial.ids<-subset.trial.ids[na>1]
	na<-na[na>1]
	ns<-length(na)



	# Merge the event and percentage data for convenience
	n.events.clean.merged<-n.events.clean["Events",]
	n.events.clean.merged[is.na(n.events.clean.merged)]<-n.events.clean["Percentage",is.na(n.events.clean.merged)]
	# Only proceed if a nonzero number of studies are included
	if(ns>0)
	{
		# What level of bias?
		rob<-matrix(NA,ncol=6,nrow=ns)
		colnames(rob)<-c("Randomization process", "Deviations from intended interventions", "Mising outcome data", "Measurement of the outcome", "Selection of the reported result", "Overall Bias")
		rownames(rob)<-subset.trial.ids
		for(i in 1:ns)
		{
			# If the ROB data is missing it should be "high" in all categories
			if(is.element(subset.trial.ids[i],rob.data[,"Unique ID"])){
			# Convert the rob to numeric (1="Low",2="Some concerns",3="High") to avoid OpenBUGS errors
				study.rob<-rob.data[rob.data[,"Unique ID"]==subset.trial.ids[i],colnames(rob)]
				study.rob[study.rob=="Low"]<-1
				study.rob[study.rob=="Some concerns"]<-2
				study.rob[study.rob=="High"]<-3			
				rob[i,]<-as.numeric(study.rob)
			}else{	
				rob[i,]<-rep(3,6)
			}
		}

		
		
		# It's at this point KM data is used if it is available
		r<-n<-tr<-times<-matrix(nrow=ns,ncol=max(na))
		for(i in 1:ns)
		{
			times[i,1:na[i]]<-pad.data[row.index & pad.data[,"Trial.ID"]==subset.trial.ids[i],"Time.point"]		
			tr[i,1:na[i]]<-arm.treatments[row.index & pad.data[,"Trial.ID"]==subset.trial.ids[i]]
			n[i,1:na[i]]<-as.numeric(n.patients.clean[row.index & pad.data[,"Trial.ID"]==subset.trial.ids[i]])
			r[i,1:na[i]]<-n.events.clean.merged[row.index & pad.data[,"Trial.ID"]==subset.trial.ids[i]]
			# Assume no KM data and then check if available 
			km.trial.ids <- NA
			# First check if it is a multiple outcomes study
			if(is.element(subset.trial.ids[i], km.summary[, "Study ID"])) 
			{
			  # Then check if has Kaplan Meier data
			  if(km.summary[km.summary[, "Study ID"] == subset.trial.ids[i], "Is KM data extracted"] == "Yes") 
			  {
			    # All KM trials have only 2 arms
  			    km.trial.ids <- paste0(rep(km.summary[km.summary[, "Study ID"] == subset.trial.ids[i], "Associated ID"], 2),
			                           "_",tr[i, 1:2], ".txt")
			  }
			} else {
			  # And if not a multiple outcomes study check if it's in the KM list
			  # Only use KM data if both arms included
			  if(sum(is.element(paste0(subset.trial.ids[i], "_",tr[i,1:na[i]]), km.trial.list)) == 2) {
			    km.trial.ids <- paste0(subset.trial.ids[i], "_",tr[i,1:na[i]], ".txt")
			  }
			}
			if(!is.na(km.trial.ids[1])) {
			  # Now extract the KM data
			  for(km.arm.i in c(1:2)) 
			  {
			    # Use latest timepoint available before cutoff.times[i.time.upper]
			    km.dataset.temp <- read_excel(km.filename, sheet = km.trial.ids[km.arm.i])
			    # Note that t.S is in months but cutoff.times are in years
			    # Use a fuzzy end and start barrier to include events within 0.01 of the cutoff as otherwise 2 year data from (e.g.) 1016 are excluded
			    n[i, km.arm.i] <- as.numeric(km.dataset.temp[min(which(km.dataset.temp[, "t.S"]/12 >= (cutoff.times[i.time.lower]-0.01))), "n.hat"])
			    # Count all patients at risk for interval from 0 to i.time.upper
			    if(time.pairs.start.zero[i.time.pair]) {
			      n[i, km.arm.i] <- as.numeric(km.dataset.temp[min(which(km.dataset.temp[, "t.S"]/12 >= 0)), "n.hat"])  
			    }
			    
			    # How many events (not counting censoring) occur in the time at risk
			    # First check if it is zero (e.g. 1016 has no events before 3 months in KM data)
			    if(is.infinite(max(which(km.dataset.temp[, "t.S"]/12 < cutoff.times[i.time.upper])))) {
			      r[i, km.arm.i] <- 0
			      # Timepoints that are used (this is just the upper limit if no events)
			      times[i, km.arm.i] <- cutoff.times[i.time.upper]
			    } else {
			      r[i, km.arm.i]<- sum(km.dataset.temp[
			        min(which(km.dataset.temp[, "t.S"]/12 >= (cutoff.times[i.time.lower]-0.01))):max(which(km.dataset.temp[, "t.S"]/12 < cutoff.times[i.time.upper])), "d"])
			      if(time.pairs.start.zero[i.time.pair]) {
			        r[i, km.arm.i]<- sum(km.dataset.temp[
			          min(which(km.dataset.temp[, "t.S"]/12 >= 0)):max(which(km.dataset.temp[, "t.S"]/12 < cutoff.times[i.time.upper])), "d"])
			      }
			      # Timepoints that are used
			      times[i, km.arm.i] <- max(km.dataset.temp[km.dataset.temp[, "t.S"]/12 < cutoff.times[i.time.upper], "t.S"])/12
			    }
			    # Invert the r so that they are number still with patency
			    # This is necessary to align with aggregate data
			    # They are re-inverted at the very end
			    r[i, km.arm.i] <- n[i, km.arm.i] - r[i, km.arm.i]
			  } # End loop over treatments      
			} # End if
		} # End loop over ns for filling in r, n, tr, and times matrices
		
		# Remove any studies that have zero events in both arms
		# or in which all patients experience an event
		# First identify such studies
		studies.to.remove <- c()
		for(i in 1:ns) {
		  if(sum(r[i, ], na.rm = TRUE) == 0 | sum(r[i, ] == n[i, ], na.rm = TRUE) == na[i]) {
		    studies.to.remove <- c(studies.to.remove, i)
		  }
		}
		if(length(studies.to.remove) > 0) {
		  # Record the removal of studies and (potentially) treatments
		  zero.event.studies[zero.event.studies$Outcome == outcome.categories[i.outcome] &
		                       zero.event.studies$`Lesion location` == lesion.locations[i.lesion.location] &
		                       zero.event.studies$`Time period` == names(time.pairs)[i.time.pair], 
		                     c("Studies removed", "Treatments in studies")] <-
		    c(paste(subset.trial.ids[studies.to.remove], collapse = ", "),
		      paste(unique(c(tr[studies.to.remove, ])), collapse = ", "))
		  
		  # Now remove such studies
		  ns <- ns - length(studies.to.remove)
		  r <- r[-studies.to.remove, ]
		  n <- n[-studies.to.remove, ]
		  times <- times[-studies.to.remove, ]
		  tr <- tr[-studies.to.remove, ]
		  rob <- rob[-studies.to.remove, ]
		  na <- na[-studies.to.remove]
		  subset.trial.ids <- subset.trial.ids[-studies.to.remove]
		  
		}
		
		# Apply a continuity correction to remaining studies
		# Subtract 0.5 from all arms if one r=n and 1 from all patients numbers
		# Add 0.5 to all arms if one r=0 and 1 to all patient numbers
		
		n.temp <- n
		if(sum(r == n, na.rm = TRUE) > 0) {
		  n[r == n & !is.na(r)] <- n[r == n & !is.na(r)] + 1
		  r[r == n.temp & !is.na(r)] <- r[r == n.temp & !is.na(r)] + 0.5  
		}
		if(sum(r == 0, na.rm = TRUE) > 0) {
		  n[r == 0 & !is.na(r)] <- n[r == 0 & !is.na(r)] + 1
		  r[r == 0 & !is.na(r)] <- r[r == 0 & !is.na(r)] + 0.5  
		}
		
		
		# Names and number of treatments
		treatment.names<-unique(c(tr[!is.na(tr)]))
		nt<-length(treatment.names)


		# Ensure PTA is first (this was most common treatment but need to choose reference)
		reference.treatment<-"PTA"
		treatment.names[which(treatment.names==reference.treatment)]<-treatment.names[1]
		treatment.names[1]<-reference.treatment	

		# Treatment codes, used to convert tr matrix to a numeric
		treatment.codes[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]]<-c(1:nt)
		names(treatment.codes[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]])=treatment.names
	
		# Convert the tr matrix to numeric
		tr.num<-tr
		for(i in 1:ns)
		{
			for(j in 1:nt)
			{
				tr.num[i,tr.num[i,]==names(treatment.codes[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]])[j]]<-treatment.codes[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]][j]
			}
		}
		tr<-matrix(as.numeric(tr.num),nrow=ns)	

		# Now ensure that each trial reports the treatments in the correct order
		# Need to sort matrices by the order of tr[i,]
		for(i in 1:ns)
		{
			n[i,]<-n[i,order(tr[i,])]
			times[i,]<-times[i,order(tr[i,])]
			r[i,]<-r[i,order(tr[i,])]
			tr[i,]<-tr[i,order(tr[i,])]
		}

		# Ensure the data are numeric
		n<-matrix(as.numeric(n),nrow=ns)
		times<-matrix(as.numeric(times),nrow=ns)
		r<-matrix(as.numeric(r),nrow=ns)

		# And ensure the rows are named
		rownames(r)<-rownames(n)<-rownames(tr)<-rownames(times)<-rownames(rob)<-names(na)<-subset.trial.ids	

		# Save the data for use in BUGS
		bugs.data[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]]<-list(n=n,times=times, r=r, t=tr, na=na, ns=ns, nt=nt, rob=rob)
	}else{ # ns==0
		# If no studies found
		bugs.data[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]]<-list("ns"=0)			
		treatment.codes[[outcome.categories[i.outcome]]][[lesion.locations[i.lesion.location]]][[names(time.pairs)[i.time.pair]]]<-list("ns"=0)
	} # End if else ns>0



	} # End loop over i.time.pair
	} # End loop over i.lesion.location
} # End loop over i.outcome

# Export summary of studies removed due to zero events
write.csv(zero.event.studies, file = paste0("results/data summary/zero.event.studies.csv"))

# Now create the patency OR restenosis and patency OR restenosis OR TLR datasets
# Need to ensure 'treatment.codes' is consistent

outcome.names<-c(outcome.categories,"Patency or Restenosis","Patency or Restenosis or TLR")

# Patency OR restenosis
i.outcome<-4

# Data structure to store results
bugs.data[[outcome.names[i.outcome]]]<-list()
treatment.codes[[outcome.names[i.outcome]]]<-list()


# There is actually only one lesion location (FP) that is being analysed here
for(i.lesion.location in 1:length(lesion.locations))
{
bugs.data[[outcome.names[i.outcome]]][[lesion.locations[i.lesion.location]]]<-list()
treatment.codes[[outcome.names[i.outcome]]][[lesion.locations[i.lesion.location]]]<-list()

for(i.time.pair in 1:length(time.pairs))
	{
	# Ensure there is some data
	if(bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$ns!=0 | bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$ns!=0)
	{
	# Create a mapping between treatment codes in each of the outcomes to the combined outcome
	treatment.names.combined<-unique(c(names(treatment.codes[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]),names(treatment.codes[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]])))
	treatment.codes.combined<-matrix(nrow=3,ncol=length(treatment.names.combined))
	colnames(treatment.codes.combined)<-treatment.names.combined
	rownames(treatment.codes.combined)<-c("Patency codes","Restenosis codes","Combined codes")
	treatment.codes.combined["Combined codes",]<-c(1:length(treatment.names.combined))
	treatment.codes.combined["Patency codes",names(treatment.codes[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]])]<-treatment.codes[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]
	# Only if there is restenosis data
	if(bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$ns!=0){
		treatment.codes.combined["Restenosis codes",names(treatment.codes[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]])]<-treatment.codes[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]
	}

	# Form a vector of the study names
	study.names.combined<-unique(c(rownames(bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$t),rownames(bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$t)))
	# Order them in ascending numeric order
	study.names.combined<-unlist(lapply(sort(as.numeric(study.names.combined)),toString))
	ns<-length(study.names.combined)

	# Maximum number of arms across all outcomes
	max.na<-max(c(bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na,bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na))


	# Build a data structure to store the combined data
	na<-rep(NA, ns)
	r<-n<-tr<-times<-matrix(nrow=ns,ncol=max.na)
	rob<-matrix(nrow=ns,ncol=6)
	colnames(rob)<-c("Randomization process", "Deviations from intended interventions", "Mising outcome data", "Measurement of the outcome", "Selection of the reported result", "Overall Bias")
			 
	names(na)<-rownames(r)<-rownames(n)<-rownames(tr)<-rownames(rob)<-rownames(times)<-study.names.combined

	# Use Patency if reported, restenosis if not
	for(study.name in study.names.combined)
	{
		# Patency is reported
		if(!is.blank(bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na[study.name]))
		{
			na[study.name]<-bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na[study.name]
			r[study.name,1:na[study.name]]<-bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$r[study.name,1:na[study.name]]
			n[study.name,1:na[study.name]]<-bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$n[study.name,1:na[study.name]]
			times[study.name,1:na[study.name]]<-bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$times[study.name,1:na[study.name]]
			# Need to convert treatment codes from outcome to combined outcomes
			tr[study.name,1:na[study.name]]<-
				treatment.codes.combined["Combined codes",names(treatment.codes[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]])[bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$t[study.name,1:na[study.name]]]]
			# Risk of bias  (1="Low",2="Some concerns",3="High")
			rob[study.name,]<-bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$rob[study.name,]
		}
		# Patency is not reported but restenosis is
		if(is.blank(bugs.data[["Patency"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na[study.name]) & !is.blank(bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na[study.name]) )
		{
			na[study.name]<-bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$na[study.name]
			n[study.name,1:na[study.name]]<-bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$n[study.name,1:na[study.name]]
			r[study.name,1:na[study.name]]<-bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$n[study.name,1:na[study.name]]-
							bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$r[study.name,1:na[study.name]]
			times[study.name,1:na[study.name]]<-bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$times[study.name,1:na[study.name]]
			# Need to convert treatment codes from outcome to combined outcomes
			tr[study.name,1:na[study.name]]<-
				treatment.codes.combined["Combined codes",names(treatment.codes[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]])[bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$t[study.name,1:na[study.name]]]]
			# Risk of bias  (1="Low",2="Some concerns",3="High")
			rob[study.name,]<-bugs.data[["Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$rob[study.name,]
		}
	} # End loop over study names combined

	# Need to sort matrices by the order of tr[i,]
	for(i in 1:ns)
	{
		n[i,]<-n[i,order(tr[i,])]
		times[i,]<-times[i,order(tr[i,])]
		r[i,]<-r[i,order(tr[i,])]
		tr[i,]<-tr[i,order(tr[i,])]
	}

	# Ensure the data are numeric
	n<-matrix(as.numeric(n),nrow=ns)
	times<-matrix(as.numeric(times),nrow=ns)
	r<-matrix(as.numeric(r),nrow=ns)

	# Ensure the rows are named
	names(na)<-rownames(r)<-rownames(n)<-rownames(tr)<-rownames(times)<-rownames(rob)<-study.names.combined

	# Final number of treatments
	nt<-length(unique(c(tr[!is.na(tr)])))

	# Now store the data
	treatment.codes[["Patency or Restenosis"]][[lesion.locations[i.lesion.location]]][[i.time.pair]]<-treatment.codes.combined["Combined codes",]
	bugs.data[[outcome.names[i.outcome]]][[lesion.locations[i.lesion.location]]][[i.time.pair]]<-list(n=n,times=times, r=r, t=tr, na=na, ns=ns, nt=nt, rob=rob)

	} # End if there is data
	} # End loop over time pairs 
} # End loop over lesion locations



# Hacked on correction to switch patency to be "absence of patency" and combined outcomes
# to be "absence of patency or needing restenosis" and "absence of patency or needing restensosis or needing TLR"
for(i.outcome in c(1,4))
{
	for(i.lesion.location in 1:length(lesion.locations))
	{
		for(i.time.pair in 1:length(time.pairs))
		{
			bugs.data[[i.outcome]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$r<-bugs.data[[i.outcome]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$n-bugs.data[[i.outcome]][[lesion.locations[i.lesion.location]]][[i.time.pair]]$r
		}
	}
}
save(km.summary, km.trial.list, km.study.name.list, lesion.locations,outcome.names,time.pairs,treatment.codes,bugs.data,file=paste("results/BUGS datasets/pad.bugs.data.10.rda",sep=""))






