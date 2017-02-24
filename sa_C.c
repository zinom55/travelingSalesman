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
	unsigned int *idx_temp_t = (unsigned int*)malloc(length * sizeof(unsigned int));
	double distance_old = CalcDistance(M, length, idx);
	double distance_new = 0, max = 0;
	unsigned int i = 0;
	for(unsigned r = 0; r < iterations; ++r) {
		i = GetConnection(i, idx, length, idx_temp_t);
		distance_new = CalcDistance(M, length, idx_temp_t);
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
	free(idx_temp_t);
	return(50*max);
}

void WriteInFiles(unsigned int count_pictures, unsigned int *idx, unsigned int length, double distance, double temperature) {
	char file_save_name[30];
	sprintf(file_save_name, "data/bier127_result/c_txt/%i.txt", count_pictures);
	printf("File: %s\n", file_save_name);
	FILE *f_picture = fopen(file_save_name, "w");
	fprintf(f_picture, "%f\n", temperature);
	fprintf(f_picture, "%f\n", distance);
	for(unsigned t = 0; t < length; t++) {
		fprintf(f_picture, "%i\n", idx[t]);
	}
	fclose(f_picture);
	if(count_pictures > 10e4) {
		printf("Too many pictures (>10e4).");
		return;
	}
}

double SA(int *x, int *y, unsigned int length, double alpha, unsigned int iter, double t_end, double t_start, int modulus, bool save_pictures, bool save_data) {
	time_t tstart;
	time(&tstart);
	FILE *f_result = NULL;
	FILE *f_result_conn = NULL;
	FILE *f_result_data = NULL;
	if(save_data == 1) {
		f_result = fopen("data/bier127_result/bier127_result_40e6.txt", "w");
		f_result_conn = fopen("data/bier127_result/bier127_result_conn_40e6.txt", "w");
		f_result_data = fopen("data/bier127_result/bier127_result_data_40e6.txt", "w");
	}
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
	int count_accept = 0;
	unsigned int count_pictures = 0, t_steps = 0;
	double mean = 0, mean_sq = 0, C = 0;
	double temperature = 0;
	if(t_start == 0) {
		temperature = GetStartTemperature(iter, M, length, idx_c);
	}
	else temperature = t_start;
	double temperature_start = temperature;
	double *t_array = (double*)malloc(((int)(log(t_end/temperature)/(log(alpha))+1)) * sizeof(double));
	double *C_array = (double*)malloc(((int)(log(t_end/temperature)/(log(alpha))+1)) * sizeof(double));
	double *mean_array = (double*)malloc(((int)(log(t_end/temperature)/(log(alpha))+1)) * sizeof(double));
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
			if(((count % modulus) == 0) && save_pictures == 1) {
				WriteInFiles(count_pictures, idx_t, length, dis_t, temperature);
               			++count_pictures;
			}
			accept = 0;	
			if(dis_t < dis_c) accept = 1;
			else if(RandMtoN(0, 1) < exp(-(dis_t - dis_c)/temperature)) accept = 1;
			if(accept) {
				idx_c = idx_t;
				idx_t = temp_c;
				temp_c = idx_c;
				dis_c = dis_t;
				mean += dis_t;
				mean_sq += dis_t*dis_t;
				++count_accept;
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
	if(save_pictures == 1) {
		WriteInFiles(count_pictures, idx_c, length, dis_c, temperature);
	}
	if(save_data == 1) {
		for(unsigned t = 0; t < t_steps; t++) {
			fprintf(f_result, "%f \t %f \t %f\n", t_array[t], mean_array[t], C_array[t]);
		}
	}
	printf("\nOpt. Combination:\n");
	for(unsigned t = 0; t < length; t++) {
		if(save_data == 1) {
			fprintf(f_result_conn, "%i\n", idx_c[t]);
		}
		printf("%i ", idx_c[t]);
	}
	printf("\n\n");
	
	time_t tend;
	time(&tend);
	if(save_data == 1) {
		fprintf(f_result_data, "Opt. distance = %f\n", dis_c);
		fprintf(f_result_data, "Starting Temperature = %f\n", temperature_start);
		fprintf(f_result_data, "Iterations per Temperature = %i\n", iter);
		fprintf(f_result_data, "Time = %lus\n", tend-tstart);
	}
	printf("Opt. Distance = %f\n", dis_c);	
	printf("Time = %lus\n", tend-tstart);
	free_matrix(M, length);
	free(C_array);	
	free(mean_array);	
	free(t_array);
	free(idx_c);
	free(idx_t);
	if(save_data == 1) {
		fclose(f_result);
	}
	return(dis_c);
}

void GetTemperatureDependence(time_t t, int *x, int *y, double i_max, double i_min, double step, unsigned int iter) {
	unsigned int count = 0;
	double *distance = (double*) malloc((int)(((i_max-i_min)/step)+1) * sizeof(double));
	for(unsigned int k = 0; k < iter; ++k) {
		printf("\nIteration: %i from %i\n", k+1, iter);
		for(double i = i_max; i >= i_min; i -= step) {
			char f_temperature_name[50];
			sprintf(f_temperature_name, "data/bier127_result/t_dependence/alpha_%f.txt", i);
			FILE *f_temperature = fopen(f_temperature_name, "a");

			srand((unsigned) time(&t));
			distance[count] = SA(x,y,127,i,1e5,1,500000,40e5,0,0);
			printf("Alpha: %f \t Distance: %f \n", i, distance[count]);
			fprintf(f_temperature, "%f \t %f \n", i, distance[count]);
			fclose(f_temperature);
			count++;
		}
		for(unsigned int i = 0; i < count; ++i) {
			printf("\n Distance: %f", distance[i]);
		}
		count = 0;
	}
}

void GetIterationDependence(time_t t, int *x, int *y, unsigned int iter) {
	unsigned int count = 0;
	unsigned int step = 0;
	unsigned int steps_array[9] = {1, 1e2, 1e3, 1e4, 1e5, 5e5, 1e6, 2e6, 4e6};
	double *distance = (double*) malloc(9 * sizeof(double));
	for(unsigned int k = 0; k < iter; ++k) {
		printf("\nIteration: %i from %i\n", k+1, iter);
		for(unsigned int i = 0; i < 9; ++i) {
			step = steps_array[i];
			char f_N_name[50];
			sprintf(f_N_name, "data/bier127_result/N_dependence/N_%i.txt", step);
			FILE *f_N = fopen(f_N_name, "a");

			srand((unsigned) time(&t));
			distance[count] = SA(x,y,127,0.8,step,1,500000,40e5,0,0);
			printf("N: %u \t Distance: %f \n", step, distance[count]);
			fprintf(f_N, "%u \t %f \n", step, distance[count]);
			fclose(f_N);
			count++;
		}
		for(unsigned int i = 0; i < count; ++i) {
			printf("\n Distance: %f", distance[i]);
		}
		count = 0;
	}
}


int main() {
	time_t t;
	srand((unsigned) time(&t));
	int x[127] = {0};	
	int y[127] = {0};	
	read_data(x, y, "data/bier127.tsp");
	//GetTemperatureDependence(t, x, y, 0.9, 0.2, 0.1, 2000);
	GetIterationDependence(t, x, y, 2);
	return(0);
}
