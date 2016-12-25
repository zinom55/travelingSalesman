#################################################################
#	Implemented algorithms for "solving" the TS problem:	#
#		1. Random connection (random(n))		#
#		2. Brute force methode (brute(n))		#
#		3. Simmulated annealing (sm(n, T, alpha))	#
#################################################################

#Function to calculate the distance of a given connection (idx (indices)) with city-coordinates x,y.
calc_distance <- function(x, y, idx) {
	N <- length(idx); #number of cities
	distance <- 0;
	#going through the indices of the given connection (idx[i])
	for(i in 1:(N-1)) {
                city_1 <- c(x[idx[i]], y[idx[i]]); #coordiante of i_th city
                city_2 <- c(x[idx[i+1]], y[idx[i+1]]); #coordinate of i+1_th city
                distance <- distance + sqrt(sum((city_2 -city_1)^2)); #distance between them
        }
	return(distance);
}

#Random connection between the cities.
random <- function(n) {
	#Generate random x,y coordiantes to "place" the cities in a 100*100 square.
	x <- runif(n, 0, 100); 	
	y <- runif(n, 0, 100);
	distance <- NULL;
	idx <- sample(n); #Generate a random connection.
	
	distance <- calc_distance(x, y, idx); #Calculate the distance of the chose connection.
	
	#Plot the connection of the cities (points) with lines in between.
	plot(x, y, pch=19, xlim=c(0,100), ylim=c(0,100));
	for(i in 1:(n-1)) {
	lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
	}
	
	#print the calculated distance
	cat("Distance: ", sum(distance), "\n");
}


#Brute force methode to calculate the connection with the smallest distance.
brute <- function(n) {
	library(combinat); #To generate all possible connections "combinat" is needed.
	
	x <- runif(n, 0, 100);
        y <- runif(n, 0, 100);

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
	plot(x, y, pch=19, ylim=c(0,100), xlim=c(0,100));
	for(i in 1:(n-1)) {
	lines(x=c(x[optimal_conn[i]], x[optimal_conn[i+1]]), y=c(y[optimal_conn[i]], y[optimal_conn[i+1]]));
	}
}

#Simulated annealing methode to approximate global minimum of the TS problem.
sm <- function(n, temperature, alpha) {
	x <- runif(n, 0, 100);
        y <- runif(n, 0, 100);
	
	k <- 1; #counter of iteration
	idx_old <- sample(n); #starting value (random connection)
	dis_old <- calc_distance(x, y, idx_old); #distance of starting value
	
	#Exit condition is some low temperature.
	while(temperature > 10^(-5)) { 
		idx_new <- sample(n); #some new random chosen connection
		
		#A try just to swop two cities, but the result was poorly.
		#swop <- ceiling(runif(2,0,4));
		#temp <- c(idx_old[swop[1]], idx_old[swop[2]]);
		#idx_new <- idx_old;
		#idx_new[swop[1]] <- temp[2];
		#idx_new[swop[2]] <- temp[1];

		dis_new <- calc_distance(x, y, idx_new); #calculate new distance
		accept <- FALSE;
		if(dis_new < dis_old) accept <- TRUE; #accept if new distance is better
		if(runif(1) < exp(-(dis_new - dis_old)/temperature)) accept <- TRUE; #if not, stil accept with some probability
		#If accepted, the old connection choice with distance dis_old will set to the new one.
		if(accept) {
			idx_old <- idx_new;
			dis_old <- dis_new;
		}
		#The temperature will be lowered every iteration. Different choices how to lower are possible.  
		temperature <- alpha*temperature; 
		#temperature <- 1/log(k+1)*temperature;
		#cat(temperature, "\n");
		
		k <- k+1; #iteration counter
	}
	#Print calculated optimal choice.
	cat("Iterations: ",k-1,"\n");
	cat("Opt. distance: ", dis_old,"\n");
	cat("City combination: ", idx_old,"\n");

	#Plot
	plot(x, y, pch=19, xlim=c(0, 100), ylim=c(0, 100));
	for(i in 1:(n-1)) {
	lines(x=c(x[idx_old[i]], x[idx_old[i+1]]), y=c(y[idx_old[i]], y[idx_old[i+1]]));
	}
}

