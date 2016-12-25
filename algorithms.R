#################################################################
#	Implemented algorithms for "solving" the TS problem:	#
#		1. Random connection (random(x, y))		#
#		2. Brute force methode (brute(x, y))		#
#		3. Simmulated annealing (sm(x, y, T, alpha))	#
#################################################################

library(combinat); #To generate all possible connections "combinat" is needed.
library(scatterplot3d); #Plotting 3D

# clean up open devices
cleandev <- function(d) {
  if(missing(d)) {
    while(dev.cur()>1) dev.off(dev.cur())
  } else for(i in 1:length(d)) dev.off(d[i])
}

#Function to calculate the distance of a given connection (idx (indices)) with city-coordinates x,y.
calc_distance <- function(x, y, idx) {
	N <- length(idx); #number of cities
	distance <- 0;
	#going through the indices of the given connection (idx[i])
	for(i in 1:(N-1)) {
                city_1 <- c(x[idx[i]], y[idx[i]]); #coordiante of i_th city
                city_2 <- c(x[idx[i+1]], y[idx[i+1]]); #coordinate of i+1_th city
                distance <- distance + sqrt(sum((city_2 - city_1)^2)); #distance between them
        }
	distance_2 <- sqrt((x[idx[N]]-x[idx[1]])^2 + (y[idx[N]]-y[idx[1]])^2); #calculate distance between the first and last point
	return(distance + distance_2);
}

#Plot the connection of the cities (points) with lines in between.
plot_ts <- function(x, y, idx, n, ...) {
	plot(x, y, pch=19, ...);
	for(i in 1:(n-1)) {
		lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
	}
	lines(x=c(x[idx[n]], x[idx[1]]), y=c(y[idx[n]], y[idx[1]]));
}

####################################################################################################

#Random connection between the cities.
random <- function(x, y) {
	n <- length(x);
	distance <- NULL;
	idx <- sample(n); #Generate a random connection.
	
	distance <- calc_distance(x, y, idx); #Calculate the distance of the chose connection.
	
	
	#Plot and Print the calculated distance
	plot_ts(x, y, idx, n, xlim = c(0,1000), ylim = c(0,1000));
	cat("Distance: ", sum(distance), "\n");
}


#Brute force methode to calculate the connection with the smallest distance.
brute <- function(x, y) {
	n <- length(x);
        dis_vec <- NULL;
	optional_conn <- permn(seq(1:n)); #Use permn from "combinat" to generate a list with all possible permuations.
	
	#Going through all possible connections and storing the	calculated distance in the vector dis_vec.
	for(j in 1:length(optional_conn)) {
		idx <- optional_conn[[j]];	
		dis_vec <- c(dis_vec, calc_distance(x, y, idx));
	}
	
	#Getting minimal distance and the corresponding connection and print them.
	optimal_dist = min(dis_vec); 
	optimal_conn = optional_conn[[which(dis_vec == min(dis_vec))[1]]];	
	cat("Opt. distance: ", optimal_dist, "\n");
	cat("City combination: ", optimal_conn, "\n");

	#Plot 
	plot_ts(x, y, optimal_conn, n, xlim = c(0,1000), ylim = c(0,1000));
}


#Simulated annealing methode to approximate global minimum of the TS problem.
sm <- function(x, y, temperature, alpha, N) {
	n <- length(x);
	
	iter <- 0; #counter of iteration
	iter_vec <- NULL; #vector for saving the iteration when temperature is switched
	dis_vec <- NULL; #vector for saving every single calculated distance
	idx_s <- sample(n); #starting value (random connection)
	dis_s <- calc_distance(x, y, idx_s); #distance of starting value
	
	#set current values to starting values
	idx_c <- idx_s;  
	dis_c <- dis_s;
	idx_t <- rep(0, n); #allocate vector for trail connection

	#Exit condition is some low temperature.
	while(temperature >= 1e-4) {
		i <- 0; #counter of switching process
		for(count in 1:N) {
			#Next lines switch the positions of two cities and reverse the direction in between.
			if(i < n) {
				i <- i + 1;
			}
			else i <- 1;
			j <- i;
			while(j == i) {
				j <- ceiling(runif(1, 0, n));
			}
			j_max <- max(c(i,j));
			i_min <- min(c(i,j));
			if((i-1) >= 1) {
				for(k in 1:(i-1)) {
					idx_t[k] <- idx_c[k];
				}
			}
			for(k in 0:(j_max-i_min)) {
				idx_t[i_min+k] <- idx_c[j_max-k];
			}
			if(n >= (j_max+1)) {
				for(k in (j_max+1):n) {
					idx_t[k] <- idx_c[k];
				}
			}
			dis_t <- calc_distance(x, y, idx_t); #calculate trail distance
			#Every 500th iteration the distance will be saved, to visiualize the process in the end.
			if((count %% 500) == 0) {
				dis_vec <- c(dis_vec, dis_t);
			}
			accept <- FALSE;
			if(dis_t < dis_c) accept <- TRUE; #accept if trail distance is better the the current one
			if(runif(1) < exp(-(dis_t - dis_c)/temperature)) accept <- TRUE; #if not, stil accept with some probability
			#If accepted, the current connection choice with distance dis_c will set to the trail one.
			if(accept) {
				idx_c <- idx_t;
				dis_c <- dis_t;
			}
			iter <- iter+1; #iteration counter
		}
		#The temperature will be lowered every iteration. Different choices how to lower are possible.  
		temperature <- alpha*temperature; 
		#temperature <- 1/log(k+1)*temperature;
		iter_vec <- c(iter_vec, iter); #Number of iterations after which the temperature is lowered wil be saved for printing corresponding vertical lines in the plot.
		cat("Temperature: ", temperature, "\n"); 
	}
	#Print calculated optimal choice.
	cat("Iterations: ",iter,"\n");
	cat("Opt. distance: ", dis_c,"\n");
	cat("City combination: ", idx_c,"\n");
	#Plot
	cleandev();
	par(mfcol = c(1,2));
	plot_ts(x, y, idx_c, n, xlim = c(0,1000), ylim = c(0,1000));
	plot(x=seq(1:length(dis_vec))*500, y=dis_vec, xlab="k / Iterations", ylab="d", type="l");
	for(i in 1:length(iter_vec)) {
		abline(v = iter_vec[i]);
	}
}
