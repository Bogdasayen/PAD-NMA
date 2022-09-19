# Function to draw forest plots on odds ratio scale using output from
# NMA of log odds ratios

# Uses medians of the hazards ratios rather than means

# V2 corrected to handle plots with only one contrast. (Matrix vs vector indexing)
# V3 switches to exponentiating the log hazard ratios as bugs was crashing with the hr themselves

# Function to format mean and 95% credible interval with fewer digits
format.results<-function(x,n.digits=3)
{
	return(paste(format(x[1],digits=n.digits)," (",format(x[2],digits=n.digits),", ",format(x[3],digits=n.digits),")",sep=""))
}

format.effect.summary<-function(x,n.digits=2)
{
  return(paste0(format(x[1],digits=n.digits, nsmall = n.digits)," (",format(x[2],digits=n.digits, nsmall = n.digits),", ",format(x[3],digits=n.digits, nsmall = n.digits),")"))
}

draw.forest.plot<-function(b.object, t.names, b.data, analysis.name = "",
			reference.treatment = "PTA",
			piecewise.period = NULL,
			t.subset = NULL, effect.invert = FALSE,
			med = TRUE, n.digits = 2, log.scale = TRUE, 
			display.p.value = TRUE,
			cex.effect.label = 1.2,
			cex.effect.estimate = 1.2,
			x.min = NULL, x.max = NULL)
{
  # Extract the hr summaries needed for the plot
  effect.summary <- matrix(NA, nrow = b.data$nt, ncol = 4)
  colnames(effect.summary) <- c("mean or median", "L95CI", "U95CI", "pvalue")
  
  rownames(effect.summary) <- t.names
  
  # Need to use coda from sims.array to calculate HRs of interest
  if(is.null(piecewise.period)) {
    d.indices <- grep("d", rownames(b.object$summary))[1:(b.data$nt - 1)]  
  } else {
    d.indices <- grep("d", rownames(b.object$summary))[2*(1:(b.data$nt - 1)) - 2 + piecewise.period]  
  }
  
  
  # Need the treatment effects relative to the reference	
  ref.index <- d.indices[which(t.names==reference.treatment)] - 1
  for(i in 1:b.data$nt)
  {
    # Is the reference for the forest plot the reference for the overall NMA
    if(ref.index == 0) {
      if(log.scale) {
        # If treatment is not reference of overall NMA
        if(i!=1)effect.coda <- exp(b.object$sims.array[,,d.indices[i-1]]) 
        if(i==1)effect.coda<-matrix(1, nrow = dim(b.object$sims.array)[1], ncol = dim(b.object$sims.array)[2])
        
      } else {
        # If treatment is not reference of overall NMA
        if(i!=1)effect.coda <- b.object$sims.array[,,d.indices[i-1]]
        if(i==1)effect.coda<-matrix(0, nrow = dim(b.object$sims.array)[1], ncol = dim(b.object$sims.array)[2])
      } # End if log scale
      
    } else {
      if(log.scale) {
        # Is it reference treatment of overall NMA
        if(i!=1)effect.coda<-exp(b.object$sims.array[,,ref.index]-b.object$sims.array[,,d.indices[i-1]])
        if(i==1)effect.coda<-exp(b.object$sims.array[,,ref.index])
        
      } else {
        # Is it reference treatment of overall NMA
        if(i!=1)effect.coda<-b.object$sims.array[,,ref.index]-b.object$sims.array[,,d.indices[i-1]]
        if(i==1)effect.coda<-b.object$sims.array[,,ref.index]
      } # End if log scale
    }
    
    if(effect.invert) {
      if(log.scale) effect.coda <- 1/effect.coda
      if(!log.scale) effect.coda <- -effect.coda
    }
    
    # Save the summary statistics
    effect.summary[i, 1:3]<-c(mean(effect.coda),quantile(effect.coda,probs=c(0.025,0.975)))
    if(med)effect.summary[i, 1:3]<-c(median(effect.coda),quantile(effect.coda,probs=c(0.025,0.975)))
    
    # P value
    if(log.scale) effect.summary[i, 4] <- sum(effect.coda > 1) / length(effect.coda)
    if(!log.scale) effect.summary[i, 4] <- sum(effect.coda > 0) / length(effect.coda)
  } # End loop over treatments
  
  # Remove the reference treatment from effect.summary
  effect.summary<-effect.summary[-which(rownames(effect.summary)==reference.treatment),]
  

  # If a subset is passed, make sure the first is the reference
  if(!is.null(t.subset)){
    if(is.element(reference.treatment,t.subset))
    {
      t.subset<-t.subset[t.subset!=reference.treatment]
    }else{
      print("Warning. t.subset must include reference treatment")
    }
  }
  
  # Just in case subset is empty
  if(is.null(t.subset))
  {
    t.subset<-t.names[t.names!=reference.treatment]
  }
  
  # Number of treatments
  n.treat<-length(t.subset)
  
  
  # Ignore treatments with no data
  if(is.null(x.max)) {
    x.max<-1.5 * max(effect.summary[t.subset,3])  
  }
  if(is.null(x.min)) {
    x.min<-0.1 * min(effect.summary[t.subset,2])  
  }
  x.width<-x.max-x.min
  
  
  # Add space for treatment effects
  t.effects<-rep(NA,n.treat)
  names(t.effects)<-t.subset
  for(name.treat in t.subset)t.effects[name.treat]<-format.effect.summary(effect.summary[name.treat,],n.digits=n.digits)
  y.min=0; y.max<-n.treat-1 + 0.5
  
  # Store the old plot parameters
  old.mar <- par()$mar
  old.fig <- par()$fig
  old.mfrow <- par()$mfrow
  old.scipen <- options()$scipen
  # Turn off scientific notation
  options(scipen = 999)
  
  
  # Create 3 plots
  # One for treatment names, one for the forest plot, and one for effect summaries
  par(mfrow = c(1,3))
  par(fig = c(0, 0.2, 0, 1))
  par(mar=c(5.1,1,4.1,1))
  plot(c(0,0), xlab = "", col = 0, xlim = c(0, 4), ylim = c(y.min, y.max), ylab = "", axes = FALSE)
  
  # Plot the treatments (not the reference)
  for(i.treat in 1:n.treat)
  {
    # Write the treatment names on the left
    
    text(t.subset[i.treat],x=0,y=i.treat-1+0.25,pos=4,cex=1.2)#cex.effect.label)
  }
  
  par(fig = c(0.2, 0.7, 0, 1), new = TRUE)
  par(mar = c(5.1, 1, 4.1, 1))
  if(!effect.invert) analysis.label = paste(analysis.name, "comparator vs ", reference.treatment)
  if(effect.invert) analysis.label = paste(analysis.name, reference.treatment,"vs comparator")
  if(log.scale) {
    plot(c(0,0),col=0,xlim=c(x.min,x.max),ylim=c(y.min,y.max),ylab="",
         xlab=analysis.label,axes=FALSE, log = "x")
    
  } else {
    plot(c(0,0),col=0,xlim=c(x.min,x.max),ylim=c(y.min,y.max),ylab="",
         xlab=analysis.label,axes=FALSE)
  }
  axis(side=1)	
  
  if(log.scale==TRUE) {
    lines(y = c(y.min, y.max), x = c(1, 1)) # Include a vertical line at hazard ratio = 1
  } else {
    lines(y = c(y.min, y.max), x = c(0, 0)) # Include a vertical line at mean = 0
  }
  
  
  # Plot the treatments (not the reference)
  for(i.treat in 1:n.treat)
  {
    # All RCTs ('true' effects)
    points(y=i.treat-1+0.25,x=effect.summary[t.subset[i.treat],1])
    lines(y=rep(i.treat-1+0.25,2),x=effect.summary[t.subset[i.treat],c(2,3)])
  }
  
  par(fig = c(0.7, 1, 0, 1), new = TRUE)
  
  par(mar=c(5.1,1,4.1,1))
  plot(c(0, 0), xlab = "", col = 0, xlim = c(0, 4), ylim = c(y.min, y.max), ylab = "",
       axes=FALSE)
  
  # Plot the treatments (not the reference)
  for(i.treat in 1:n.treat)
  {
    # Write the treatment estimates on the right
    if(display.p.value) {
      text(format.effect.summary(effect.summary[t.subset[i.treat],],n.digits=n.digits),x=0,y=i.treat-1+0.45,pos=4,cex=cex.effect.estimate)
      text(paste0("p = ", format(effect.summary[t.subset[i.treat], 4],digits = 2)),x=0,y=i.treat-1,pos=4,cex=cex.effect.estimate)  
    } else {
      text(format.effect.summary(effect.summary[t.subset[i.treat],],n.digits=n.digits),x=0,y=i.treat - 0.75 ,pos=4,cex=cex.effect.estimate)
    }
    
  }
  
  # Reset parameters
  par(mar = old.mar)
  par(fig = old.fig)
  par(mfrow = old.mfrow)
  options(scipen = old.scipen)
  
}