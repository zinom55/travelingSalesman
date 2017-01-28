read_conn <- function(file) {
	data <- read.table(file);
	return(data);
}

read_data <- function(file) {
	data <- read.table(file);
	return(data);
}


#Plot the connection of the cities (points) with lines in between.
plot_ts <- function(file, file_conn, ...) {
       	idx <- read_conn(file_conn);
	
	idx <- idx[,1] + 1;
	data <- read_data(file);
	x <- data[,2];
	y <- data[,3];
	n <- length(x);
	plot(x, y, pch=19, ...);
        for(i in 1:(n-1)) {
                if(i == 1) {
                        lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]), col="red");
                }
                else {
                        lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
                }
        }
        lines(x=c(x[idx[n]], x[idx[1]]), y=c(y[idx[n]], y[idx[1]]), col="blue");

}


file <- "data/dsj1000.tsp";
file_conn <- "data/dsj1000_result/dsj1000_result_conn_1e5.txt";

plot_ts(file, file_conn);


