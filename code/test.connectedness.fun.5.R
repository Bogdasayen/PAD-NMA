# Code for connectedness testing, step matrix generation, and collecting into connect sub-components

# Function to test whether an evidence network is connected and generate the distance matrix
# The input tr.matrix is same format as treatment matrix ‘t’ used in the NICE DSU TSD 2 WinBUGS code for network meta-analysis.
# An alternative to supplying tr.matrix is to supply vectors t1 and t2. These include one entry for each arm of the trials and t1 represents a list of trial IDs while t2 represents a list of treatments

require(expm)

test.connectedness.fun<-function(tr.matrix=NULL,t1=NULL,t2=NULL,nameoftreatments=NULL,sort.matrices=TRUE)
{
	# tr.matrix is a matrix of rows corresponding to trials and columns
	# corresponding to treatments
	if(is.null(tr.matrix) & (is.null(t1) | is.null(t2))){
		stop("Must provide tr.matrix or both t1 and t2 vectors")
	}

	# Vectors t1 and t2 include one entry for each arm of the trials
	# t1 represents a list of trial IDs
	# t2 represents a list of treatments
	# These will be constructed if not provided

	# nameoftreatments converts the numbers of t2 into treatment names

	# Construct the t1 and t2 vectors if only treatment matrix is provided
	if(is.null(t1) | is.null(t2))
	{
		x<-tr.matrix
		for(i in 1:dim(x)[1])
		{
			x[i,]<-i
		}
		t1<-c(x[!is.na(tr.matrix)])
		t2<-c(tr[!is.na(tr.matrix)])
	}
	
	# Build a matrix of 1s and 0s representing direct evidence connections
	n.trials<-length(unique(t1))
	n.treatments<-length(nameoftreatments)
	if(is.null(nameoftreatments))n.treatments<-length(unique(t2))
	adjacency.matrix<-matrix(0,nrow=n.treatments,ncol=n.treatments)
 	t2=as.numeric(as.factor(t2))
	for(trial.i in unique(t1))
	{
		adjacency.matrix[t2[t1==trial.i],t2[t1==trial.i]]<-1
	}
	# Set the diagonal to 0
	diag(adjacency.matrix)<-0
	# Sum the powers of the adjacency matrix to calculate the indirect connection matrix
	treatment.indirect.connection<-adjacency.matrix
	for(i in 2:(n.treatments-1))
	{
		treatment.indirect.connection<-treatment.indirect.connection+adjacency.matrix%^%i
	}
	# Set all non-zero elements to 1
	treatment.indirect.connection<-treatment.indirect.connection!=0
	# Convert to numeric
	treatment.indirect.connection<-matrix(as.numeric(treatment.indirect.connection),ncol=n.treatments)
	
	# Calculate the distance matrix
	distance.matrix<-adjacency.matrix
	for(i in 2:(n.treatments-1))
	{
		A.im1<-adjacency.matrix
		for(j in 1:(i-1))
		{
			A.im1<-A.im1+adjacency.matrix%^%j
		}
		A.i<-A.im1+adjacency.matrix%^%i
		# Set non-zero entries to 1
		A.i[A.i!=0]<-1
		A.im1[A.im1!=0]<-1
		distance.matrix<-distance.matrix+i*(A.i-A.im1)
	}
	# Set the diagonal to zero
	diag(distance.matrix)<-0

	# Name the rows and columns after the treatments
	if(!is.null(nameoftreatments)){
		rownames(distance.matrix)<-colnames(distance.matrix)<-rownames(adjacency.matrix)<-colnames(adjacency.matrix)<-rownames(treatment.indirect.connection)<-colnames(treatment.indirect.connection)<-nameoftreatments
	}else{
		warning("No nameoftreatments provided")
	}
	# Sort the matrices by treatment names?
	if(sort.matrices==TRUE)
	{
		# Sort the entries in the matrices
		adjacency.matrix<-adjacency.matrix[order(as.numeric(rownames(adjacency.matrix))),order(as.numeric(rownames(adjacency.matrix)))]
		treatment.indirect.connection<-treatment.indirect.connection[order(as.numeric(rownames(treatment.indirect.connection))),order(as.numeric(rownames(treatment.indirect.connection)))]
		distance.matrix<-distance.matrix[order(as.numeric(rownames(distance.matrix))),order(as.numeric(rownames(distance.matrix)))]
	}

	return(list("adjacency.matrix"=adjacency.matrix,"indirect.connection.matrix"=treatment.indirect.connection,"distance.matrix"=distance.matrix))

}

# Recursive function to collect/sort the indirect connection and distance matrices 
# into connected sub-components

# Simple function to collect matrices from 1st index
# Used recursively in collect.matrix
collect.submatrix<-function(collected.matrix)
{
	collected.treatments<-c(which(collected.matrix[1,]>0),which(collected.matrix[1,]==0))
	collected.matrix<-collected.matrix[collected.treatments,collected.treatments]
}

# Function to collect elements of a matrix into block-diagonal form
collect.matrix<-function(collected.matrix)
{
n.treatments<-dim(collected.matrix)[1]

# Slightly different for first treatment
collected.matrix<-collect.submatrix(collected.matrix)
	
for(j in 2:(dim(collected.matrix)[2]-1))
{
	if(collected.matrix[j,j-1]==0)
	{

		temp.collected.matrix<-collect.submatrix(collected.matrix[j:n.treatments,j:n.treatments])
		collected.matrix[j:n.treatments,j:n.treatments]<-temp.collected.matrix
		rownames(collected.matrix)[j:n.treatments]<-rownames(temp.collected.matrix)
		colnames(collected.matrix)[j:n.treatments]<-rownames(temp.collected.matrix)
	}
}
return(collected.matrix)
}
