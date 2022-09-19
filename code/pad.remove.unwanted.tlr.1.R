# Code to remove rows from the PAD data that report unwanted versions of TLR
# Howard Thom 12-August-2019

rows.to.remove<-c()

# Laird 2010. Use "TLR - freedom from" rather than "TLR - freedom from (post-hoc")
rows.to.remove<-c(rows.to.remove,
		which(pad.data[,"First.Author"]=="Lair" & 
		pad.data[,"PubYear"]=="2010" & pad.data[,"Category"]=="TLR"
		& pad.data[,"Outcome"]!="TLR - freedom from"))


# Now remove the extraneous rows
rows.to.keep<-!is.element(1:dim(pad.data)[1],rows.to.remove)
pad.data<-pad.data[rows.to.keep,]
