#################################################################################################################################################
#	Implemented algorithms for "solving" the TS problem:											#
#		1. Random connection (random(x, y))												#
#		2. Brute force methode (brute(x, y))												#
#		3. Simulated annealing (sm(x, y, T, alpha, N, t_end = 10e-4, option_save="no",option_PlotTemp = "no", option_mod = 500))	#
#################################################################################################################################################

library(combinat); #To generate all possible connections "combinat" is needed.
library(scatterplot3d); #Plotting 3D

cat("Implemented algorithms for solving the TS problem:\n");
cat("	1. Random connection (random(x, y))\n
	2. Brute force methode (brute(x, y))\n	
	3. Simulated annealing (sm(x, y, T, alpha, N, t_end = 10e-4, option_save = no,option_PlotTemp = no, option_mod = 500))\n\n"
);


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


#Function for switch the positions of two cities and reverse the direction in between.
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

#Simulated annealing methode to approximate global minimum of the TS problem.
sm <- function(x, y, temperature, alpha, N, t_end = 10e-4, option_save = "no", option_PlotTemp = "no", option_mod = 500) {
	n <- length(x);
	
	iter <- 0; #counter of iteration
	iter_vec <- NULL; #vector for saving the iteration when temperature is switched
	dis_vec <- NULL; #vector for saving the last 1000 calculated distances
	dis_vec_i <- NULL; #vector for saving distance against iteration
	dis_vec_t <- NULL; #vector for saving distance against temperature
	C_vec_t <- NULL; #vector for saving specific_heat against temperature
	t_vec <-NULL; #vector for saving the used temperatures
	dis_r_vec <- NULL; #vector for saving the random walk if T=0 is chosen
	idx_s <- sample(n); #starting value (random connection)
	dis_s <- calc_distance(x, y, idx_s); #distance of starting value
	count_pic <- 1;
		
	#set current values to starting values
	idx_c <- idx_s;  
	dis_c <- dis_s;
	#If temperature == 0 is chosen, the starting temperature will be calculated by T=10*max{DeltaH}
	if(temperature == 0) {
		idx_r <- idx_c;
		dis_r_old <- dis_s;
		i <- 0;
		for(r in 1:10e3) {
			temp <- new_connection(i, idx_r, n); 			
			i <- temp[1];
			idx_r <- temp[2:length(temp)];
			dis_r <- calc_distance(x, y, idx_r); #calculate trail distance
			dis_r_vec <- c(dis_r_vec, dis_r-dis_r_old);
			dis_r_old <- dis_r;
		}
		temperature <- 10*max(dis_r_vec);	
	}

	#Exit condition is some low temperature.
	while(temperature >= t_end) {
		cat("Temperature: ", temperature, "\n"); 
		i <- 0; #counter of switching process
		for(count in 1:N) {
			#Switch the positions of two cities and reverse the direction in between.
			temp <- new_connection(i, idx_c, n); 			
			i <- temp[1];
			idx_t <- temp[2:length(temp)];
			dis_t <- calc_distance(x, y, idx_t); #calculate trail distance
			#Every option_modth iteration the distance will be saved, to visiualize the process in the end.
			if((count %% option_mod) == 0) {
				dis_vec_i <- c(dis_vec_i, dis_t);
			}
			accept <- FALSE;
			if(dis_t < dis_c) accept <- TRUE; #accept if trail distance is better the the current one
			if(runif(1) < exp(-(dis_t - dis_c)/temperature)) accept <- TRUE; #if not, stil accept with some probability
			#If accepted, the current connection choice with distance dis_c will set to the trail one.
			if(accept) {
				idx_c <- idx_t;
				dis_c <- dis_t;
				#If option_save is chosen, a picture of every accepted connection is saved. 
				dis_vec <- c(dis_vec, dis_t);
				if(option_save == "yes") {
					jpeg(file=paste("pictures/", count_pic,".jpg", sep=""));
					plot_ts(x, y, idx_c, n, xlim = c(0,20000), ylim = c(0,20000), main=paste(temperature));
					dev.off();
					count_pic <- count_pic + 1;
				}
			}
			iter <- iter+1; #iteration counter	
		
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
		#The temperature will be lowered every iteration. Different choices how to lower are possible.  
		temperature <- alpha*temperature; 
		#temperature <- 1/log(k+1)*temperature;
		iter_vec <- c(iter_vec, iter); #Number of iterations after which the temperature is lowered wil be saved for printing corresponding vertical lines in the plot.
	}
	#Print calculated optimal choice.
	cat("Iterations: ",iter,"\n");
	cat("Opt. distance: ", dis_c,"\n");
	cat("City combination: ", idx_c,"\n");
	#Plot
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
