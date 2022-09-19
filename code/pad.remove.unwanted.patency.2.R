# Code to remove rows from the PAD data that report unwanted versions of patency
# Howard Thom 6-August-2019

# Hacked together so probably not reusable

# v2 updated to use dataset from 8-Aug-2019
# Note that many of the edits no longer have an impact as Vincent corrected them on the data side.

# Advice from Vincent 8-August-2019
# In terms of choices between these two outcomes, primary patency is always
# preferred (this applies to Lammer 2013, Rastan 2011, Brodmann 2011, 
# Gray 2018 and Laird 2018).
# If there are multiple measurement for the outcome, e.g. via angiography or 
# duplex Doppler ultrasound, we apply the worst-case scenario, i.e. bigger
# event results for binary restenosis and smaller event results for primary
# patency. Therefore, for studies, Zeller 2017, Tielbeek 1996 and Lammer 1992,
# we should take angiographic patency results.
# If there are different definitions, I would suggest to stay with the common 
# definitions (e.g. 50% or PVSR>2.4…etc). For example, for study Rand 2006, 
# please use PriP 50%.
# Some trials counted bailout stenting as failure (loss of patency) 
# immediately, which happened to  ID 770-Laird-2010 and ID 724-Dake-2011.
# This does not fit to common practice so post-hoc results should be used. 
# However, I need to double-check this with Rob/Graeme.


rows.to.remove<-c()

# Zeller 2017: Using PriP in outcome as this is via angiography.
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Zeller" & 
		pad.data[,"PubYear"]=="2017" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP"))

#Gandini 2016:Using categroy patency with "patency (angio)" in outcome
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Gandini" & 
		pad.data[,"PubYear"]=="2016" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="Patency (angio)"))


# Lammer 2013: PriP preferred over VR
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Lammer" & 
		pad.data[,"PubYear"]=="2013" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP"))

# Rastan 2011: PriP is preferred over VR or TLR
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Rastan" & 
		pad.data[,"PubYear"]=="2011" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP"))

# Brodmann 2011: Using "Primary patency" rather than "Re-obstruction (patency)"?
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Brodmann" & 
		pad.data[,"PubYear"]=="2011" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="Primary patency"))


# Laird 2010: Using "PriP (post-hoc)" not PriP
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Laird" & 
		pad.data[,"PubYear"]=="2010" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP (post-hoc)"))


# Rand 2006: Using PriP 50%
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Rand" & 
		pad.data[,"PubYear"]=="2006" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP 50%"))


# Tielbeek 1996: Using PriP (Angio) rather than "Time to primary patency" (this is more a statistical issue)
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Tielbeek" & 
		pad.data[,"PubYear"]=="1996" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP (Angio)"))


# Lammer 1992: Using PriP (Angio) rather than (ABI) or (clinical)
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Lammer" & 
		pad.data[,"PubYear"]=="1992" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="PriP (Angio)"))

# Gray 2018: Using "Primary patency" rather than "CD-TLR"
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Gray" & 
		pad.data[,"PubYear"]=="2018" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="Primary patency"))


# Laird 2018: Using "Primary patency" rather than "Freedom from TLR"
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Laird" & 
		pad.data[,"PubYear"]=="2018" & pad.data[,"Category"]=="Patency" 
		& pad.data[,"Outcome"]!="Primary patency"))


# Miura 2018: Using "Primary patency" rather than "Primary patency (ISR rate; PSVR > 2.0)" or "Primary patency (PSVR > 2.0)"
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Miura" & 
		pad.data[,"PubYear"]=="2018" & pad.data[,"Category"]=="Patency"
		& pad.data[,"Outcome"]!="Primary patency (PSVR > 2.0)"))


# Now remove the extraneous rows
rows.to.keep<-!is.element(1:dim(pad.data)[1],rows.to.remove)
pad.data<-pad.data[rows.to.keep,]





