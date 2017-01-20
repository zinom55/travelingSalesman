//Simulated annealing algorithm analog to the one in algorithms.R, but now in C to gain speed

#include "stdio.h"
#include "stdlib.h"
#include "math.h"

//Read the data for TS-problem of the 127 Biergarten in Augsburg
void read_data(int *x, int *y) {
	FILE *f_biergarten = fopen("data/bier127.tsp", "r");
	unsigned int i = 0;
	unsigned int number = 0;
	if(f_biergarten == NULL) {printf("Error! File does not exist."); exit(-1);}
	while(fscanf(f_biergarten, "%i %i %i",&number, &x[i], &y[i]) == 3) {
		//printf("\n%i \t %i", x[i], y[i]);
		++i;
	}
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
	printf("\n %f", M[125][126]);
	printf("\n %f", M[126][125]);
	printf("\n %f", M[126][126]);
	for(unsigned int i = 0; i < (length_x-1); ++i) {
		for(unsigned int j = (i+1); j < length_x; ++j) {
			city_1[1] = x[i];
			city_1[2] = y[i];
			city_2[1] = x[j];
			city_2[2] = y[j];
			distance = sqrt(pow((city_2[1]-city_1[1]),2) + pow((city_2[2]-city_1[2]),2));
			printf("\n%i \t %i \t %f", i, j, distance);
			M[i][j] = distance;
			M[j][i] = distance;
		}
		M[i][i] = 0;
	}
	M[length_x-1][length_x-1] = 0;
	return(M);
}

int main() {
	int x[127] = {0};	
	int y[127] = {0};	
	read_data(x, y);
	double **M = distance_matrix(x, y, 127);

	
	free_matrix(M, 127);
	return(0);
}
