# clean up open devices
cleandev <- function(d) {
  if(missing(d)) {
    while(dev.cur()>1) dev.off(dev.cur())
  } else for(i in 1:length(d)) dev.off(d[i])
}


read_data <- function(file, ...) {
	data <- read.table(file, ...);
	return(data);
}

#Plot the connection of the cities as points
plot_ts_points <- function(file, option_save = 0, ...) {
	data <- read_data(file);
	x <- data[,2];
	y <- data[,3];
	n <- length(x);
	if(option_save == 1) {
		pdf("pictures_latex/bier127_points.pdf", width=15, height=10);
	}
	par(oma=c(0,0,0,0), mar=c(5,6,2,2), mgp=c(0,1.5,0));
	plot(x[1:n], y[1:n], pch=19, ann=FALSE, cex.axis=2.5, cex=1.2, ...);
	mtext(side = 1, text = "X / arb. unit", line = 4, cex=2.5);
	mtext(side = 2, text = "Y / arb. unit", line = 4, cex = 2.5);
}

#Plot the connection of the cities (points) with lines in between.
plot_ts <- function(file, file_conn, option = 0, option_save = 0, ...) {
       	if(option == 1) {
		idx_temp <- read_data(file_conn);
		idx <- read_data(file_conn, skip = 2);
		distance <- idx_temp[,1][2];
	}
	else {
		#idx <- read_data(file_conn, skip = 2);
		idx <- read_data(file_conn);
	}
	
	idx <- idx[,1] + 1;
	data <- read_data(file);
	x <- data[,2];
	y <- data[,3];
	n <- length(x);
	if(option_save == 1) {
		pdf("pictures_latex/bier127_BestPar.pdf", width=15, height=12);
	}

	if(option == 1) {
		par(oma=c(0,0,1,0), mar=c(5,6,2,2), mgp=c(0,1.5,0));
	}
	else {
		par(oma=c(0,0,0,0), mar=c(5,6,2,2), mgp=c(0,1.5,0));
	}
	plot(x[1:n], y[1:n], pch=19, ann=FALSE, cex.axis=2.5, cex=1.2, ...);
	mtext(side = 1, text = "X / arb. unit", line = 4, cex=2.5);
	mtext(side = 2, text = "Y / arb. unit", line = 4, cex = 2.5);
	if(option == 1) {
		distance <- round(distance, 2);
		mtext(side = 3, text = paste("Distance:", distance), line = 1, cex=2.5, col = "red");
	}
	points(x[idx[1]], y[idx[1]], col = "red", pch = 19, cex=2);
        for(i in 1:(n-1)) {
                if(i <= 10) {
                        lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]), col="red");
                }
                else {
                        lines(x=c(x[idx[i]], x[idx[i+1]]), y=c(y[idx[i]], y[idx[i+1]]));
                }
        }
        lines(x=c(x[idx[n]], x[idx[1]]), y=c(y[idx[n]], y[idx[1]]));
}

plot_SpecificHeat <- function(file_data, option_save = 0, ...) {
	data <- read_data(file_data);
	temperature <- data[,1];
	C <- data[,3];
	if(option_save == 1) {
		pdf("pictures_latex/bier127_specific.pdf", width=15, height=10);
	}
	par(oma=c(0,0,0,0), mar=c(5,6,2,2), mgp=c(0,1.5,0));
	plot(temperature, C, type = "l", ann=FALSE, cex.axis=2.5, cex=1.2, log = "x", ...);
	points(temperature, C, pch=19, cex=0.8);
	mtext(side = 1, text = "T / arb. unit", line = 4, cex=2.5);
	mtext(side = 2, text = "C / arb. unit", line = 4, cex = 2.5);
}

plot_MeanEnergy <- function(file_data, option_save = 0, ...) {
	data <- read_data(file_data);
	temperature <- data[,1];
	C <- data[,2];
	if(option_save == 1) {
		pdf("pictures_latex/bier127_mean.pdf", width=15, height=10);
	}
	par(oma=c(0,0,0,0), mar=c(5,6.5,2,2), mgp=c(0,1.5,0));
	plot(temperature, C, type = "l", ann=FALSE, cex.axis=2.5, cex=1.2, log = "x", ...);
	points(temperature, C, pch=19, cex=0.8);
	mtext(side = 1, text = "T / arb. unit", line = 4, cex=2.5);
	mtext(side = 2, text = expression(paste(bar(H)," / arb. unit")), line = 4, cex = 2.5);
}
	
#Plot the specific heat and mean energy of the TSP
plot_data <- function(file_data, file_conn = NULL, option = 0, ...) {
	data <- read_data(file_data);
	temperature <- data[,1];
	start_temperature <- max(temperature);
	if(option == 1) {
		data_temp <- read_data(file_conn);
		temp <- data_temp[,1][1];
		distance <- data_temp[,1][2];
		idx <- which(temperature >= temp);
		temperature <- data[,1][idx];
		H <- data[,2][idx];
		C <- data[,3][idx];
		screen(3);
		par(oma=c(0,0,1,0), mar=c(5,6,2,2));
		plot(temperature, H, type = "l", ann=FALSE, log="x", xlim = c(1,start_temperature), ylim = c(100000,700000), cex.axis=2, main = paste(distance), ...);
		mtext(side = 2, text = expression(paste(Delta,"H / arb. unit")), line = 3, cex=2);
		mtext(side = 1, text = "T / arb. unit", line = 3, cex = 2);
		temp <- round(temp, 2);
		mtext(side = 3, text = paste("Temperature:",temp), line = 1, cex=2, col = "red");
		screen(4);
		par(oma=c(0,0,0,0), mar=c(5,6,2,2));
		plot(temperature, C, type = "l", ann=FALSE, log="x", cex.axis=2, xlim = c(1,start_temperature), ylim = c(0,200), ...);
		mtext(side = 2, text = "C / arb. unit", line = 3, cex=2);
		mtext(side = 1, text = "T / arb. unit", line = 3, cex = 2);
	}
	else {
		H <- data[,2];
		C <- data[,3];
		par(oma=c(0,0,0,0), mar=c(5,6,2,2), mfcol = c(1,2));
		plot(temperature, H, type = "l", ann=FALSE, log="x", cex.axis=2);
		mtext(side = 2, text = expression(paste(Delta,"H / arb. unit")), line = 3, cex=2);
		mtext(side = 1, text = "T / arb. unit", line = 3, cex = 2);
		plot(temperature, C, type = "l", ann=FALSE, log="x", cex.axis=2, ...);
		mtext(side = 2, text = "C / arb. unit", line = 3, cex=2);
		mtext(side = 1, text = "T / arb. unit", line = 3, cex = 2);
	}
}

CreatePictures <- function(N, file, file_data) {
	for(i in 0:N) {
		file_conn <- paste(c("data/bier127_result/c_txt/",i,".txt"), collapse="");
		jpeg(file=paste("data/bier127_result/pictures/", i,".jpg", sep=""), width = 1400, height = 700);
		split.screen(figs=c(1,2));
		split.screen(figs=c(2,1), screen = 2);
		screen(1);
		plot_ts(file, file_conn, 1);
		plot_data(file_data, file_conn, 1);
		close.screen(all=TRUE);
		dev.off();	
	}
}



cleandev();
file <- "data/bier127.tsp";
#file_conn_end <- "data/bier127_result/bier127_result_conn_test.txt";
file_data <- "data/bier127_result/bier127_result_40e6.txt";
file_conn <- "data/bier127_result/bier127_result_conn_1e4.txt";


#plot_ts(file, file_conn);
#dev.new();
#plot_data(file_data);
#plot_ts(file,file_conn,0,1);
#CreatePictures(1240, file, file_data);


plot_MeanEnergy(file_data,1);
