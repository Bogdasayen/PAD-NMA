# Utility function to identify favoured analysis


# Full list is
# > unique(pad.data[, "Analysis"])
preference.order <- c("time-to-event", "Time-to-event", "KM", 
                      "Time-to-event; ITT", "Time-to-event (mITT)","Time-to-event; mITT", 
                      "Time-to-event; PP",
                       "ITT",   "mITT",
                      "Per Protocol", "Per protocol", "PP",
             "Odd ratios",
             "Subgroup (by diabetes)", "Subgroup (by sex)",
             "Unclear")

# Returns an index of favoured analysis
which.favoured.analysis <- function(analysis.types, analysis.timepoints) {
  # Only one analysis type and timepoint
  if(length(analysis.types) == 1) {
    return(c(1))
  }
  
  
  # Find favoured analysis within each set of duplicate timepoints
  favoured.analysis.index <- c()
  for(analysis.timepoint in unique(analysis.timepoints)) {
    for(favoured.analysis in preference.order) {
      if(is.element(favoured.analysis, analysis.types[analysis.timepoints == analysis.timepoint])) {
        favoured.analysis.index <- c(favoured.analysis.index,
                                     which(analysis.types == favoured.analysis & 
                                           analysis.timepoints == analysis.timepoint))
        break
      }
    }
  }
  
  return(favoured.analysis.index)
}

