#################################################################
#       Implemented algorithms for "solving" the TS problem:    #
#               1. Random connection (random(x, y))             #
#               2. Brute force methode (brute(x, y))            #
#               3. Simmulated annealing (sm(x, y, T, alpha))    #
#################################################################

library(combinat); #To generate all possible connections "combinat" is needed.
library(plot3D); #Plotting 3D

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

#Function to visualize the potential given with the chosen cities.
#vis_pot <- function(x, y) {

	x <- runif(5,0,100);
	y <- runif(5,0,100);

        n <- length(x);
        dis_vec <- NULL;
        optional_conn <- permn(seq(1:n)); #Use permn from "combinat" to generate a list with all possible permuations.

        #Going through all possible connections and storing the calculated distance in the vector dis_vec.
        for(j in 1:length(optional_conn)) {
                idx <- optional_conn[[j]];
                dis_vec <- c(dis_vec, calc_distance(x, y, idx));
        }

        z_plot <- matrix(dis_vec, sqrt(length(optional_conn)), sqrt(length(optional_conn)));
        x_plot <- seq(1:sqrt(length(optional_conn)));
        y_plot <- seq(1:sqrt(length(optional_conn)));
        persp3D(z=z_plot);
#}

