num.iterations <- 1000;

# Function to standardize input values
zscore <- function(x, mean.val=NA) 
{
	if(is.matrix(x)) 
		return(apply(x, 2, zscore, mean.val=mean.val))
	if(is.data.frame(x)) 
		return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
	if(is.na(mean.val)) 
		mean.val <- mean(x)
		sd.val <- sd(x)
	if(all(sd.val == 0)) 
		return(x) # if all the values are the same
	(x - mean.val) / sd.val 
}

# Gradient descent function
grad <- function(x, y, theta) 
{
  gradient <- (1 / nrow(y)) * (t(x) %*% (as.matrix((1/(1 + exp(-as.matrix(x) %*% (t(theta)))) - y))));
  return(t(gradient))
}

gradient.descent <- function(x, y, alpha=0.0003, num.iterations=num.iterations, threshold=1e-5, output.path=FALSE) 
{

    # Add x_0 = 1 as the first column
    m <- if(is.vector(x)) 
			length(x) 
		else nrow(x)
    if(is.vector(x) || (!all(x[,1] == 1)))
 		x <- cbind(rep(1, m), x)
    if(is.vector(y)) 
		y <- matrix(y)
		x <- apply(x, 2, as.numeric)  # converts each column in x to be numeric
	
	int_data <- cbind(y,x);
	#int_data <- apply(data, 2, as.numeric)
	
	
    num.features <- ncol(x)
    
    # Initialize the parameters
    theta <- matrix(rep(0, num.features), nrow=1)  # First row of values is given to the first row

	# theta.path is a matrix that contains the values of the theta values for each variable
    # Look at the values over each iteration
    theta.path <- theta
	
	for (i in 1:num.iterations) 
	{
	  
	  # vector of thetas is updated using the grad(x,y,theta) function and stored momentarily in theta
	  theta <- theta - alpha * grad(x, y, theta)
	  
	  # if the calulcations for all values in theta vector gives NA then for loop is broken
      if(all(is.na(theta))) 
			break
	  
	  # binds the theta vector as a row to the end of the theta.path matrix
	  theta.path <- rbind(theta.path, theta)
      
	  # after initialization, if the change between thetas for ALL variables is less than the threshold, then for-loop
	  # is also borken
	  if(i > 2) 
		if(all(abs(theta - theta.path[i-1,]) < threshold)) 
			break 
	  if ( i %% 100 == 0)
		 print(i);
    }
    
	# output.path is true then return the theta.path
    if(output.path) 
		return(theta.path) 
	else 
		return(theta.path[nrow(theta.path),])
}

sto.gradient.descent <- function(x, y, alpha=0.0003, num.iterations=num.iterations, threshold=1e-2, output.path=FALSE) 
{

    # Add x_0 = 1 as the first column
    m <- if(is.vector(x)) 
			length(x) 
		else nrow(x)
    if(is.vector(x) || (!all(x[,1] == 1)))
 		x <- cbind(rep(1, m), x)
    if(is.vector(y)) 
		y <- matrix(y)
		x <- apply(x, 2, as.numeric)  # converts each column in x to be numeric
	
	int_data <- cbind(y,x);
	#int_data <- apply(data, 2, as.numeric)
	
	
    num.features <- ncol(x)
    
    # Initialize the parameters
    theta <- matrix(rep(0, num.features), nrow=1)  # First row of values is given to the first row

	# theta.path is a matrix that contains the values of the theta values for each variable
    # Look at the values over each iteration
    theta.path <- theta
	
	for (i in 1:num.iterations) 
	{
	  # Only use a sample each time, using sampling --> Stochastic gradient portion	
	  sample_iter <- int_data[sample(1:nrow(int_data), 1000, replace = FALSE),];
	  
	  x_red <- sample_iter[,-1];
	  y_red <- matrix(sample_iter[,1]);
	
	  # vector of thetas is updated using the grad(x,y,theta) function and stored momentarily in theta
	  theta <- theta - alpha * grad(x_red, y_red, theta)
	  
	  # if the calulcations for all values in theta vector gives NA then for loop is broken
      if(all(is.na(theta))) 
			break
	  
	  # binds the theta vector as a row to the end of the theta.path matrix
	  theta.path <- rbind(theta.path, theta)
      
	  # after initialization, if the change between thetas for ALL variables is less than the threshold, then for-loop
	  # is also borken
	  if(i > 2) 
		if(all(abs(theta - theta.path[i-1,]) < threshold)) 
			break 
	  if ( i %% 100 == 0)
		 print(i);
    }
    
	# output.path is true then return the theta.path
    if(output.path) 
		return(theta.path) 
	else 
		return(theta.path[nrow(theta.path),])
}