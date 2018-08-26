library(combinat); # To generate all possible connections of the distances "combinat" is needed
library(scatterplot3d); # For plotting reason

cat("Implemented algorithms for solving the TS problem:\n");
cat("	1. Random connection (random(x, y))\n
	2. Brute force methode (brute(x, y))\n	
	3. Simulated annealing (sa(x, y, alpha = 0.8, N = 100, t_end = 10e-4, option_save = no, option_PlotTemp = no, option_mod = 500))\n
	4. Simulated annealing with a time component (sa_time <- function(x, y, alpha = 0.8, N = 100, t_end = 10e-4, time = 0,	stretch_f = 1, area = array(0,dim=c(2,2)), option_save = no, option_PlotTemp = no, option_mod = 500))\n\n"
);

# Load data
data <- read.table("data/beer127.tsp");
x <- data[,2]
y <- data[,3]

####################################### Helper functions ############################################

# Clean up open devices
cleandev <- function(d) {
  if(missing(d)) {
    while(dev.cur()>1) dev.off(dev.cur())
  } else for(i in 1:length(d)) dev.off(d[i])
}


# Function to calculate the matrix, which contains the distances between every city (point).
calc_distance_matrix <- function(x,y) {
	N <- length(x);
	M <- matrix(rep(0, N^2), ncol = N);
	# Loop over upper triangle part of the matrix. 
	for(i in 1:(N-1)) {
		for(j in (i+1):N) {
			city_1 <- c(x[i], y[i]); # Coordiante of the i_th city as a vector		
			city_2 <- c(x[j], y[j]); # Coordiante of the j_th city as a vector		
                	distance <- sqrt(sum((city_2 - city_1)^2)); # Distance between them is the scalar product
			# Use the symmetry distance(i,j) = distance(j,i) to fill the matrix
			M[i,j] <- distance; 
			M[j,i] <- distance; 
		}
	}
	return(M);
}


# Function to calculate the distance of a given connection (idx (indices)) with distance matrix M
calc_distance <- function(M, idx) {
	N <- length(idx); # Number of cities
	distance <- 0;
	# Going through the indices of the given connection (idx[i])
	for(i in 1:(N-1)) {
		distance <- distance + M[idx[i], idx[i+1]]; # Getting the distance between the connections from the matrix M
        }
	distance <- distance + M[idx[N], idx[1]]; # Calculate distance between the first and last point
	return(distance);
}


# Function to check if the connection between city1 and city2 lays in the defined area (2x2 array: area)
inside_region <- function(city1, city2, area) {
	if(city1[1] >= area[1,1] && city1[1] <= area[1,2]) {
		if(city1[2] >= area[2,1] && city1[2] <= area[2,2]) {
			if(city2[1] >= area[1,1] && city2[1] <= area[1,2]) {
				if(city2[2] >= area[2,1] && city2[2] <= area[2,2]) {
					p <- 1;
				}
				else p <- 0;
			}
			else p <- 0;
		}
		else p <- 0; 
	}
	else p <- 0;
	return(p);
}


# Function to calculate the distance of a given connection (idx (indices)) with distance matrix M with respect to the time component 
calc_distance_time <- function(x, y, idx, time, area, stretch_f) {
	N <- length(idx); #number of cities
	distance <- 0;
	for(i in 1:(N-1)) {
		city1 <- c(x[idx[i]], y[idx[i]]);
		city2 <- c(x[idx[i+1]], y[idx[i+1]]);
		if(inside_region(city1, city2, area)) {
			if(distance >= time) {
				distance <- distance + sqrt(sum((city2 - city1)^2))*stretch_f;
			}
			else {
				distance <- distance + sqrt(sum((city2 - city1)^2));
			}
		}
		else {
			distance <- distance + sqrt(sum((city2 - city1)^2));
		}
	}
	city1 <- c(x[idx[N]], y[idx[N]]);
	city2 <- c(x[idx[1]], y[idx[1]]);
	if(inside_region(city1, city2, area)) {
		if(distance >= time) {
			distance <- distance + sqrt(sum((city2 - city1)^2))*stretch_f;
		}
		else {
			distance <- distance + sqrt(sum((city2 - city1)^2));
		}
	}
	else {
		distance <- distance + sqrt(sum((city2 - city1)^2));
	}
	return(distance);
}


plot_ts_points <- function(x, y, ...) {
        par(oma=c(0,0,0,0), mar=c(5,6,2,2), mgp=c(0,1.5,0));
        plot(x, y, pch=19, ann=FALSE, cex.axis=2.5, cex=1.2, ...);
        mtext(side = 1, text = "X / arb. unit", line = 4, cex=2.5);
        mtext(side = 2, text = "Y / arb. unit", line = 4, cex = 2.5);	
}


# Plot the connection of the cities (points) with lines in between.
plot_ts <- function(x, y, idx, n, ...) {
	par(oma=c(0,0,0,0), mar=c(5,6,2,2), mgp=c(0,1.5,0));
        plot(x, y, pch=19, ann=FALSE, cex.axis=2.5, cex=1.2, ...);
        mtext(side = 1, text = "X / arb. unit", line = 4, cex=2.5);
        mtext(side = 2, text = "Y / arb. unit", line = 4, cex = 2.5);
	for(i in 1:(n-1)) {
		if(i == 1) {
			lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
		}
		else {
			lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
		}
	}
	lines(x=c(x[idx[n]], x[idx[1]]), y=c(y[idx[n]], y[idx[1]]));
	
}


# Function for switch the positions of two cities and reverse the direction in between
new_connection <-function(i, idx_c, n) {
	idx_t <- rep(0, n); #allocate vector for trail connection
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
	return(c(i, idx_t));
}


# Function two define default areas 1, 2 or 3.
define_area <- function(option) {
	if(option == 1) {
		area <- array(c(5000, 15000, 5000, 17000), dim = c(2,2));
	}
	else if(option == 2) {
		area <- array(c(7500, 12500, 7500, 15000), dim = c(2,2));
	}	
	else if(option == 3) {
		area <- array(c(0, 20000, 0, 20000), dim = c(2,2));

	}
}


# Function to define the start temperature
start_temperature <- function(idx_c, dis_s, n, M, N) {
	dis_r_vec <- NULL; #vector for saving the random walk if T=0 is chosen
	idx_r <- idx_c;
	dis_r_old <- dis_s;
	i <- 0;
	for(r in 1:N) {
		temp <- new_connection(i, idx_r, n); 			
		i <- temp[1];
		idx_r <- temp[2:length(temp)];
		dis_r <- calc_distance(M, idx_r); #calculate trail distance
		dis_r_vec <- c(dis_r_vec, dis_r-dis_r_old);
		dis_r_old <- dis_r;
	}
	return(10*max(dis_r_vec));

}


########################################### Functions ###############################################

# Random connection between the cities
random <- function(x, y) {
	n <- length(x);
	M <- calc_distance_matrix(x, y); # Calcuate the matrix, containing the distances between all points
	distance <- NULL;
	idx <- sample(n); # Generate a random connection
	
	distance <- calc_distance(M, idx); # Calculate the distance of the chose connection
	
	# Plot and Print the calculated distance
	plot_ts(x, y, idx, n, xlim = c(0,20000), ylim = c(0,20000));
	cat("Distance: ", sum(distance), "\n");
}


# Brute force methode to calculate the connection with the smallest distance.
brute <- function(x, y) {
	n <- length(x);
	M <- calc_distance_matrix(x, y); # Calcuate the matrix, containing the distances between all points
        dis_vec <- NULL;
	optional_conn <- permn(seq(1:n)); # Use permn from "combinat" to generate a list with all possible permuations
	
	# Going through all possible connections and storing the calculated distance in the vector dis_vec
	for(j in 1:length(optional_conn)) {
		idx <- optional_conn[[j]];	
		dis_vec <- c(dis_vec, calc_distance(M, idx));
	}
	
	# Getting minimal distance and the corresponding connection and print them
	optimal_dist = min(dis_vec); 
	optimal_conn = optional_conn[[which(dis_vec == min(dis_vec))[1]]];	
	cat("Opt. distance: ", optimal_dist, "\n");
	cat("City combination: ", optimal_conn, "\n");

	# Plot 
	plot_ts(x, y, optimal_conn, n, xlim = c(0,20000), ylim = c(0,20000));
}
 

# Simulated annealing methode to approximate global minimum of the TS problem
sa <- function(x, y, alpha = 0.8, N = 100, t_end = 10e-4, option_save = "no", option_PlotTemp = "no", option_mod = 500) {
	start_time <- proc.time();
	n <- length(x);
	M <- calc_distance_matrix(x, y); # Calcuate the matrix, containing the distances between all points
	
	iter <- 0; # Counter of iteration
	iter_vec <- NULL; # Vector for saving the iteration when temperature is switched
	dis_vec <- NULL;
	dis_vec_i <- NULL; # Vector for saving distance against iteration
	dis_vec_t <- NULL; # Vector for saving distance against temperature
	C_vec_t <- NULL; # Vector for saving specific_heat against temperature
	t_vec <-NULL; # Vector for saving the used temperatures
	idx_s <- sample(n); # Starting value (random connection)
	dis_s <- calc_distance(M, idx_s); # Distance of starting value
	count_pic <- 1;
		
	# Set current values to starting values
	idx_c <- idx_s;  
	dis_c <- dis_s;
	
	# The starting temperature will be calculated by T=10*max{DeltaH}, using a random walk
	temperature = start_temperature(idx_c, dis_s, n, M, N);	

	# Exit condition is some low temperature.
	while(temperature >= t_end) {
		cat("Temperature: ", temperature, "\n"); 
		i <- 0; # Counter of switching process
		for(count in 1:N) {
			# Switch the positions of two cities and reverse the direction in between
			temp <- new_connection(i, idx_c, n); 			
			i <- temp[1];
			idx_t <- temp[2:length(temp)];
			dis_t <- calc_distance(M, idx_t); # Calculate trail distance
			# Every option_mod-th iteration the distance will be saved, to visiualize the process in the end
			if((count %% option_mod) == 0) {
				dis_vec_i <- c(dis_vec_i, dis_t);
			}
			accept <- FALSE;
			if(dis_t < dis_c) accept <- TRUE; # Accept if trail distance is better the the current one
			if(runif(1) < exp(-(dis_t - dis_c)/temperature)) accept <- TRUE; # If not, stil accept with some probability
			# If accepted, the current connection choice with distance dis_c will set to the trail one
			if(accept) {
				idx_c <- idx_t;
				dis_c <- dis_t;
				# The accepted distances are saved.
				dis_vec <- c(dis_vec, dis_t);
				# If option_save is chosen, a picture of every accepted connection is saved, to build a video later on
				if(option_save == "yes") {
					jpeg(file=paste("pictures/", count_pic,".jpg", sep=""));
					plot_ts(x, y, idx_c, n, xlim = c(0,20000), ylim = c(0,20000), main=paste(temperature));
					dev.off();
					count_pic <- count_pic + 1;
				}
			}
			iter <- iter+1; # Iteration counter	
		
		}
		t_vec <- c(t_vec, temperature); # Vector of temperatures T
		length_dis_vec <- length(dis_vec); # Number of accepted moves this T
		if(length_dis_vec != 0) {
			#dis_vec_t <- c(dis_vec_t, mean(dis_vec[ceiling(length_dis_vec/2):length_dis_vec]));
			#C_vec_t <- c(C_vec_t, var(dis_vec[ceiling(length_dis_vec/2):length_dis_vec])/temperature^2);
			# Calculate the mean and the specific heat for this temerature-run
			dis_vec_t <- c(dis_vec_t, mean(dis_vec));
			C_vec_t <- c(C_vec_t, var(dis_vec)/temperature^2);
			#cleandev();
			#dev.new();
			#plot(dis_vec, xlab="k / Acc. Moves", ylab="d", type="l");
			cat("Mean energy: ", mean(dis_vec), "\n");
			cat("C_specific: ", var(dis_vec)/temperature^2, "\n");
		}
		else {
			dis_vec_t <- c(dis_vec_t, 0);
			C_vec_t <- c(C_vec_t, 0);
		}

		cat("Nr. of accepted moves: ", length_dis_vec, "\n\n");
		dis_vec <- NULL;
		# The temperature will be lowered every iteration  
		temperature <- alpha*temperature; 
		iter_vec <- c(iter_vec, iter); # Number of iterations after which the temperature is lowered wil be saved for printing corresponding vertical lines in the plot.
	}
	stop_time <- proc.time();
	
	# Print calculated optimal choice.
	cat("Iterations: ",iter,"\n");
	cat("Opt. distance: ", dis_c,"\n");
	cat("City combination: ", idx_c,"\n");
	cat("Time: ", stop_time - start_time,"s\n");
	# Plot
	cleandev();
	plot_ts(x, y, idx_c, n, xlim = c(0,20000), ylim = c(0,20000));
	dev.new();
	par(mfcol = c(1,2));
	plot(x=t_vec, y=dis_vec_t, xlab="temperature", ylab="Delta d", type="l", log="x");
	plot(x=t_vec, y=C_vec_t, xlab="temperature", ylab="C", type="l", log="x");
	dev.new();
	plot(x=seq(1:length(dis_vec_i))*option_mod, y=dis_vec_i, xlab="k / Iterations", ylab="d", type="l");
	if(option_PlotTemp == "yes") {
		for(i in 1:length(iter_vec)) {
			abline(v = iter_vec[i]);
		}
	}
}

# Simulated annealing methode to approximate global minimum of the TS problem plus time component. It will be assumed that the overall time is 24h and that after time "time" the distance to travel takes the factor "stretch_f" longer.
sa_time <- function(x, y, alpha = 0.8, N = 100, t_end = 10e-4, time = 0, stretch_f = 1, area = array(0,dim=c(2,2)), option_save = "no", option_PlotTemp = "no", option_mod = 500) {
	n <- length(x);
	solution <- 118293.52;
	time <- time * solution / 24;

	iter <- 0; # Counter of iteration
	iter_vec <- NULL; # Vector for saving the iteration when temperature is switched
	dis_vec <- NULL; # Vector for saving the last 1000 calculated distances
	dis_vec_i <- NULL; # Vector for saving distance against iteration
	dis_vec_t <- NULL; # Vector for saving distance against temperature
	C_vec_t <- NULL; # Vector for saving specific_heat against temperature
	t_vec <-NULL; # Vector for saving the used temperatures
	dis_r_vec <- NULL; # Vector for saving the random walk if T=0 is chosen
	idx_s <- sample(n); # Starting value (random connection)
	dis_s <- calc_distance_time(x, y, idx_s, time, area, stretch_f); # Distance of starting value
	count_pic <- 1;
		
	# Set current values to starting values
	idx_c <- idx_s;  
	dis_c <- dis_s;
	# If temperature == 0 is chosen, the starting temperature will be calculated by T=10*max{DeltaH}, with a random walk
	if(temperature == 0) {
		idx_r <- idx_c;
		dis_r_old <- dis_s;
		i <- 0;
		for(r in 1:10e3) {
			temp <- new_connection(i, idx_r, n); 			
			i <- temp[1];
			idx_r <- temp[2:length(temp)];
			dis_r <- calc_distance_time(x, y, idx_r, time, area, stretch_f); # Calculate trail distance
			dis_r_vec <- c(dis_r_vec, dis_r-dis_r_old);
			dis_r_old <- dis_r;
		}
		temperature <- 10*max(dis_r_vec);	
	}

	# Exit condition is some low temperature
	while(temperature >= t_end) {
		cat("Temperature: ", temperature, "\n"); 
		i <- 0; #counter of switching process
		for(count in 1:N) {
			# Switch the positions of two cities and reverse the direction in between
			temp <- new_connection(i, idx_c, n); 			
			i <- temp[1];
			idx_t <- temp[2:length(temp)];
			dis_t <- calc_distance_time(x, y, idx_t, time, area, stretch_f); # Calculate trail distance
			# Every option_mod-th iteration the distance will be saved, to visiualize the process in the end
			if((count %% option_mod) == 0) {
				dis_vec_i <- c(dis_vec_i, dis_t);
			}
			accept <- FALSE;
			if(dis_t < dis_c) accept <- TRUE; # Accept if trail distance is better the the current one
			if(runif(1) < exp(-(dis_t - dis_c)/temperature)) accept <- TRUE; # If not, stil accept with some probability
			# If accepted, the current connection choice with distance dis_c will set to the trail one
			if(accept) {
				idx_c <- idx_t;
				dis_c <- dis_t;
				# If option_save is chosen, a picture of every accepted connection is saved 
				dis_vec <- c(dis_vec, dis_t);
				if(option_save == "yes") {
					jpeg(file=paste("pictures/", count_pic,".jpg", sep=""));
					plot_ts(x, y, idx_c, n, xlim = c(0,20000), ylim = c(0,20000), main=paste(temperature));
					dev.off();
					count_pic <- count_pic + 1;
				}
			}
			iter <- iter+1; # Iteration counter	
		}

		t_vec <- c(t_vec, temperature);
		length_dis_vec <- length(dis_vec);
		if(length_dis_vec != 0) {
			dis_vec_t <- c(dis_vec_t, mean(dis_vec[ceiling(length_dis_vec/2):length_dis_vec]));
			C_vec_t <- c(C_vec_t, var(dis_vec[ceiling(length_dis_vec/2):length_dis_vec])/temperature^2);
		}
		else {
			dis_vec_t <- c(dis_vec_t, 0);
			C_vec_t <- c(C_vec_t, 0);
		}
		cat("Nr. of accepted moves: ", length_dis_vec, "\n\n");
		dis_vec <- NULL;

		# The temperature will be lowered every iteration  
		temperature <- alpha*temperature; 
		iter_vec <- c(iter_vec, iter); # Number of iterations after which the temperature is lowered wil be saved for printing corresponding vertical lines in the plot
	}

	# Print calculated optimal choice.
	cat("Iterations: ",iter,"\n");
	cat("Opt. distance: ", dis_c,"\n");
	cat("City combination: ", idx_c,"\n");
	
	# Plot
	cleandev();
	plot_ts(x, y, idx_c, n, xlim = c(0,20000), ylim = c(0,20000));
	lines(x=c(area[1,1], area[1,2]), y=c(area[2,1], area[2,1]));
	lines(x=c(area[1,1], area[1,2]), y=c(area[2,2], area[2,2]));
	lines(x=c(area[1,1], area[1,1]), y=c(area[2,1], area[2,2]));
	lines(x=c(area[1,2], area[1,2]), y=c(area[2,1], area[2,2]));
	dev.new();
	par(mfcol = c(1,2));
	plot(x=t_vec, y=dis_vec_t, xlab="temperature", ylab="Delta d", type="l", log="x");
	plot(x=t_vec, y=C_vec_t, xlab="temperature", ylab="C", type="l", log="x");
	dev.new();
	plot(x=seq(1:length(dis_vec_i))*option_mod, y=dis_vec_i, xlab="k / Iterations", ylab="d", type="l");
	if(option_PlotTemp == "yes") {
		for(i in 1:length(iter_vec)) {
			abline(v = iter_vec[i]);
		}
	}
}
