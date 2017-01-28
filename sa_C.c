//Simulated annealing algorithm analog to the one in algorithms.R, but now in C to gain speed

#include "stdio.h"
#include "stdbool.h"
#include "stdlib.h"
#include "math.h"
#include "time.h"
#include "string.h"

//Read the data for TS-problem.
void read_data(int *x, int *y, const char *file) {
	FILE *f = fopen(file, "r");
	unsigned int i = 0;
	unsigned int number = 0;
	if(f == NULL) {printf("Error! File does not exist."); exit(-1);}
	while(fscanf(f, "%i %i %i",&number, &x[i], &y[i]) == 3) {
		//printf("\n%i \t %i", x[i], y[i]);
		++i;
	}
	fclose(f);
}

double** allocate_matrix(unsigned int rows, unsigned int cols) {
	double** M = (double**)malloc(rows * sizeof(double*));
	for(int i = 0; i < rows; ++i) {
		M[i] = (double*)malloc(cols * sizeof(double));
	}
	return(M);
}

void free_matrix(double **M, unsigned int rows) {
	for(int i = 0; i < rows; ++i) {
		free(M[i]);
	}
	free(M);
}

double** distance_matrix(int *x, int *y, unsigned int length_x) {
	double** M = allocate_matrix(length_x, length_x);
	int city_1[2] = {0};
	int city_2[2] = {0};
	double distance = 0;
	for(unsigned int i = 0; i < (length_x-1); ++i) {
		for(unsigned int j = (i+1); j < length_x; ++j) {
			city_1[1] = x[i];
			city_1[2] = y[i];
			city_2[1] = x[j];
			city_2[2] = y[j];
			distance = sqrt(pow((city_2[1]-city_1[1]),2) + pow((city_2[2]-city_1[2]),2));
			M[i][j] = distance;
			M[j][i] = distance;
		}
		M[i][i] = 0;
	}
	M[length_x-1][length_x-1] = 0;
	return(M);
}

double CalcDistance(double** M, unsigned int length, unsigned int *idx) {
	double distance = 0;
	for(unsigned int i = 0; i < (length-1); ++i) {
		distance += M[idx[i]][idx[i+1]];
	}
	distance += M[idx[length-1]][idx[0]];
	return(distance);
}

double RandMtoN(double M, double N) {
	return(M + (rand() / (RAND_MAX / (N-M))));  
}

unsigned int GetConnection(unsigned int i, unsigned int *idx, unsigned int length, unsigned int *idx_new) {
	unsigned int j = 0, j_max = 0, i_min = 0; 
	j = i;
	while(j == i) {
		j = (int)RandMtoN(0, length-1);
	}
	//printf("j = %i\n", j);
	if(i > j) {
		j_max = i;		
		i_min = j;
	}		
	else {
		j_max = j;		
		i_min = i;
	}
	if(i > 0) {
		for(unsigned int k = 0; k <= (i-1); ++k) {
			idx_new[k] = idx[k];
		}
	}
	for(unsigned int k = 0; k <= j_max-i_min; ++k) {
		idx_new[i_min + k] = idx[j_max - k];
	}
	if((length-1) > (j_max)) {
                for(unsigned int k = j_max+1; k < length; ++k) {
                        idx_new[k] = idx[k];
                }
        }	
	if(i < (length-1)) {
		i = i + 1;
	}
	else i = 0;
	return(i);	
}

double GetStartTemperature(unsigned int iterations, double **M, unsigned int length, unsigned int *idx) {
	double *distance_array = (double*)malloc(iterations * sizeof(double));
	unsigned int *idx_t = (unsigned int*)malloc(length * sizeof(unsigned int));
	double distance_old = CalcDistance(M, length, idx);
	double distance_new = 0, max = 0;
	unsigned int i = 0;
	for(unsigned r = 0; r < iterations; ++r) {
		i = GetConnection(i, idx, length, idx_t);
		distance_new = CalcDistance(M, length, idx_t);
		distance_array[r] = fabs(distance_new - distance_old);
		distance_old = distance_new;
		if(r >= 1) {
			if(distance_array[r] > distance_array[r-1]) {
				max = distance_array[r]; 
			}
		}
		else max = distance_array[0];
	}
	free(distance_array);
	free(idx_t);
	return(50*max);
}

void SA(int *x, int *y, unsigned int length, double alpha, unsigned int iter, double t_end) {
	time_t tstart;
	time(&tstart);
	
	FILE *f_result = fopen("data/bier127_result_1e6.txt", "w");
	FILE *f_result_conn = fopen("data/bier127_result_conn_1e6.txt", "w");
	FILE *f_result_data = fopen("data/bier127_result_data_1e6.txt", "w");
	double **M = distance_matrix(x, y, length);
	unsigned int *idx_c = (unsigned int*)malloc(length * sizeof(unsigned int));
	unsigned int *idx_t = (unsigned int*)malloc(length * sizeof(unsigned int));
	unsigned int *temp_c = idx_c;
	for(unsigned int i = 0; i < length; ++i) {
		idx_c[i] = i;
	}
	double dis_c = CalcDistance(M, length, idx_c);
	double dis_t = 0;
	bool accept = 0;
	unsigned int count_accept = 0, t_steps = 0;
	double mean = 0, mean_sq = 0, C = 0;
	double temperature = GetStartTemperature(iter, M, length, idx_c);
	double temperature_start = temperature;
	double *t_array = (double*)malloc(((int)(log(t_end/temperature)/(log(alpha)))) * sizeof(double));
	double *C_array = (double*)malloc(((int)(log(t_end/temperature)/(log(alpha)))) * sizeof(double));
	double *mean_array = (double*)malloc(((int)(log(t_end/temperature)/(log(alpha)))) * sizeof(double));
	unsigned int i = 0;
	printf("Start Connection:\n");
	for(unsigned int p = 0; p < length; ++p) {
		printf("%i ", idx_c[p]);
	}
	printf("\n\n");
	while(temperature >= t_end) {
		printf("Temperature: %f\n", temperature);
		i = 0;
		count_accept = 0;
		mean = 0;
		mean_sq = 0;
		C = 0;
		for(unsigned count = 0; count < iter; ++count) {
			i = GetConnection(i, idx_c, length, idx_t);
			dis_t = CalcDistance(M, length, idx_t);
			accept = 0;	
			if(dis_t < dis_c) accept = 1;
			else if(RandMtoN(0, 1) < exp(-(dis_t - dis_c)/temperature)) accept = 1;
			if(accept) {
				idx_c = idx_t;
				idx_t = temp_c;
				temp_c = idx_c;
				dis_c = dis_t;
				++count_accept;
				mean += dis_t;
				mean_sq += dis_t*dis_t;
			}
		}
		if(count_accept != 0) {
			mean = mean/count_accept;
			mean_sq = mean_sq/count_accept;
			C = (mean_sq - pow(mean, 2)) / pow(temperature, 2);
		}
		else {
			mean = 0;
			C = 0;
		}
		C_array[t_steps] = C;
		t_array[t_steps] = temperature;
		printf("Accepted: %i \nDistance: %f\n", count_accept, dis_c);
		printf("H: %f \nC: %f\n\n", mean, C);
		mean_array[t_steps] = mean;
		temperature *= alpha;
		++t_steps;
	}
	for(unsigned t = 0; t < t_steps; t++) {
		fprintf(f_result, "%f \t %f \t %f\n", t_array[t], mean_array[t], C_array[t]);
	}
	printf("\nOpt. Combination:\n");
	for(unsigned t = 0; t < length; t++) {
		fprintf(f_result_conn, "%i\n", idx_c[t]);
		printf("%i ", idx_c[t]);
	}
	printf("\n\n");
	fprintf(f_result_data, "Opt. distance = %f\n", dis_c);
	fprintf(f_result_data, "Starting Temperature = %f\n", temperature_start);
	fprintf(f_result_data, "Iterations per Temperature = %i\n", iter);
	time_t tend;
	time(&tend);
	fprintf(f_result_data, "Time = %lus\n", tend-tstart);
	

	printf("Opt. Distance = %f\ns", dis_c);	
	printf("Time = %lus\n", tend-tstart);
	free_matrix(M, length);
	fclose(f_result);
}	


int main() {
	time_t t;
	srand((unsigned) time(&t));
	int x[127] = {0};	
	int y[127] = {0};	
	read_data(x, y, "data/bier127.tsp");
	SA(x,y,127,0.9,1e6,1);	
	return(0);
}
