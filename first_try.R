#random connection
random <- function(n) {
	x <- runif(n, 0, 100);
	y <- runif(n, 0, 100);
	
	N <- length(x);
	distance <- NULL;
	idx <- sample(N);
	
	for(i in 1:(N-1)) {
		city_1 <- c(x[idx[i]], y[idx[i]]); 
		city_2 <- c(x[idx[i+1]], y[idx[i+1]]);
		distance <- c(distance, sqrt(sum((city_2 -city_1)^2))); 
	}
	
	plot(x, y, pch=19);
	for(i in 1:(N-1)) {
	lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
	}
	
	cat("Distance: ", sum(distance), "\n");
}


#brute force-methode
brute <- function(n) {
	library(combinat);
	x <- runif(n, 0, 100);
        y <- runif(n, 0, 100);

        N <- length(x);
        distance <- 0;
        dis_vec <- NULL;
	optional_conn <- permn(seq(1:n));
	
	for(j in 1:length(optional_conn)) {
		idx <- optional_conn[[j]];	
		for(i in 1:(N-1)) {
        		city_1 <- c(x[idx[i]], y[idx[i]]);
                	city_2 <- c(x[idx[i+1]], y[idx[i+1]]);
                	distance <- distance + sqrt(sum((city_2 -city_1)^2));
        	}
		dis_vec <- c(dis_vec, distance);
		distance <- 0;
	}
	optimal_dist = min(dis_vec);
	optimal_conn = optional_conn[[which(dis_vec == min(dis_vec))[1]]];	
	cat("Opt. distance: ", optimal_dist, "\n");
	cat("City combination: ", optimal_conn, "\n");

	plot(x, y, pch=19);
	for(i in 1:(N-1)) {
	lines(x=c(x[optimal_conn[i]], x[optimal_conn[i+1]]), y=c(y[optimal_conn[i]], y[optimal_conn[i+1]]));
	}
}

calc_distance <- function(x, y, idx) {
	N <- length(idx);
	distance <- 0;
	for(i in 1:(N-1)) {
                city_1 <- c(x[idx[i]], y[idx[i]]);
                city_2 <- c(x[idx[i+1]], y[idx[i+1]]);
                distance <- distance + sqrt(sum((city_2 -city_1)^2));
        }
	return(distance);
}

#simulated annealing
sm <- function(n, temperature, alpha) {
	x <- runif(n, 0, 100);
        y <- runif(n, 0, 100);
	
        N <- length(x);
	k <- 1;
	idx_old <- sample(N);
	dis_old <- calc_distance(x,y,idx_old);

	while(temperature > 10^(-5)) {
		idx_new <- sample(N);
		#swop <- ceiling(runif(2,0,4));
		#temp <- c(idx_old[swop[1]], idx_old[swop[2]]);
		#idx_new <- idx_old;
		#idx_new[swop[1]] <- temp[2];
		#idx_new[swop[2]] <- temp[1];
		dis_new <- calc_distance(x,y,idx_new);
		accept <- FALSE;
		if(dis_new < dis_old) accept <- TRUE;
		if(runif(1) < exp(-(dis_new - dis_old)/temperature)) accept <- TRUE;
		if(accept) {
			idx_old <- idx_new;
			dis_old <- dis_new;
		}
		temperature <- alpha*temperature;
		#temperature <- 1/log(k+1)*temperature;
		#cat(temperature, "\n");
		k <- k+1;
	}
	cat("Steps: ",k-1,"\n");
	cat("Opt. distance: ", dis_old,"\n");
	cat("City combination: ", idx_old,"\n");

	plot(x, y, pch=19);
	for(i in 1:(N-1)) {
	lines(x=c(x[idx_old[i]], x[idx_old[i+1]]), y=c(y[idx_old[i]], y[idx_old[i+1]]));
	}
}

