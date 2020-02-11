#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <time.h>
#include <complex.h>
#include <math.h>

long int upper_bound = 10000000000;
double lower_bound = 0.001;
double range = 2.0;
pthread_mutex_t row_done_mutex = PTHREAD_MUTEX_INITIALIZER;
int image_size, n_threads, degree;
double *points;
char **attractors;
char **convergences;
char *row_done;
double complex *roots;
struct Result {
    int attr;
    int conv;
};

char *colors[] = {
    "0 0 0 ",
    "9 0 0 ",
    "0 9 0 ",
    "0 0 9 ",
    "9 9 0 ",
    "0 9 9 ",
    "9 4 0 ",
    "4 0 4 ",
    "4 9 0 ",
    "0 9 4 "
};

char *gray_values[] = {"00 00 00 ", "01 01 01 ", "02 02 02 ", "03 03 03 ", "04 04 04 ", "05 05 05 ", "06 06 06 ", "07 07 07 ", "08 08 08 ", "09 09 09 ",
                       "10 10 10 ", "11 11 11 ", "12 12 12 ", "13 13 13 ", "14 14 14 ", "15 15 15 ", "16 16 16 ", "17 17 17 ", "18 18 18 ", "19 19 19 ",
                       "20 20 20 ", "21 21 21 ", "22 22 22 ", "23 23 23 ", "24 24 24 ", "25 25 25 ", "26 26 26 ", "27 27 27 ", "28 28 28 ", "29 29 29 ",
                       "30 30 30 ", "31 31 31 ", "32 32 32 ", "33 33 33 ", "34 34 34 ", "35 35 35 ", "36 36 36 ", "37 37 37 ", "38 38 38 ", "39 39 39 ",
                       "40 40 40 ", "41 41 41 ", "42 42 42 ", "43 43 43 ", "44 44 44 ", "45 45 45 ", "46 46 46 ", "47 47 47 ", "48 48 48 ", "49 49 49 ",
                       "50 50 50 ", "51 51 51 ", "52 52 52 ", "53 53 53 ", "54 54 54 ", "55 55 55 ", "56 56 56 ", "57 57 57 ", "58 58 58 ", "59 59 59 ",
                       "60 60 60 ", "61 61 61 ", "62 62 62 ", "63 63 63 ", "64 64 64 ", "65 65 65 ", "66 66 66 ", "67 67 67 ", "68 68 68 ", "69 69 69 ",
                       "70 70 70 ", "71 71 71 ", "72 72 72 ", "73 73 73 ", "74 74 74 ", "75 75 75 ", "76 76 76 ", "77 77 77 ", "78 78 78 ", "79 79 79 ",
                       "80 80 80 ", "81 81 81 ", "82 82 82 ", "83 83 83 ", "84 84 84 ", "85 85 85 ", "86 86 86 ", "87 87 87 ", "88 88 88 ", "89 89 89 ",
                       "90 90 90 ", "91 91 91 ", "92 92 92 ", "93 93 93 ", "94 94 94 ", "95 95 95 ", "96 96 96 ", "97 97 97 ", "98 98 98 ", "99 99 99 "};

struct Result newton(double re, double im) {
	double complex z = re + im * I;
    double complex diff, root;
    double diff_re, diff_im;
    int conv, attr;
    for (conv = 0, attr = -1; ; conv++) {
        if (re > upper_bound || re < -upper_bound || im > upper_bound || im < -upper_bound) {
            // Too far away
            attr = 0;
            break;
        }
        if (re * re + im * im < lower_bound * lower_bound) {
            // Too close to origin
            attr = 0;
            break;
        }
        for (int i = 1; i <= degree; i++) {
        	// roots[0] = 0
            root = roots[i];
            diff = z - root;
            diff_re = creal(diff);
            diff_im = cimag(diff);
            if (diff_re * diff_re + diff_im * diff_im < lower_bound * lower_bound) {
            	attr = i;
            	break;
            }
        }
        if (attr != -1) {
          break;
        }
        if (conv > 99) {
        	conv = 99;
        }
        switch (degree) {
            case 1: z = 1.0; break;
            case 2: z = (1 * z) / 2 + 1 / (2 * z); break;
            case 3: z = (2 * z) / 3 + 1 / (3 * z * z); break;
            case 4: z = (3 * z) / 4 + 1 / (4 * z * z * z); break;
            case 5: z = (4 * z) / 5 + 1 / (5 * z * z * z * z); break;
            case 6: z = (5 * z) / 6 + 1 / (6 * z * z * z * z * z); break;
            case 7: z = (6 * z) / 7 + 1 / (7 * z * z * z * z * z * z); break;
            case 8: z = (7 * z) / 8 + 1 / (8 * z * z * z * z * z * z * z); break;
            case 9: z = (8 * z) / 9 + 1 / (9 * z * z * z * z * z * z * z * z); break;
            default:
                fprintf(stderr, "Unexptected degree\n");
                exit(1);
        }
        re = creal(z);
        im = cimag(z);
    }
    struct Result r = {attr, conv};
    return r;
}

void *compute_main(void *arg) {
	int offset = *((int *) arg);
	free(arg);
	double re, im;
	int attr, conv;
	char *temp_attractors, *temp_convergences;
    struct Result r;
    for (int i = offset; i < image_size; i += n_threads) {
        // i is a row index
        temp_attractors = malloc(6 * sizeof(char) * image_size);
        temp_convergences = malloc(9 * sizeof(char) * image_size);
        im = points[(image_size - 1) - i];
        for (int j = 0; j < image_size; j++) {
            // j loops over all columns
            re = points[j];
            r = newton(re, im);
            memcpy(temp_attractors + 6 * j, colors[r.attr], 6);
  			memcpy(temp_convergences + 9 * j, gray_values[r.conv], 9);
        }
        attractors[i] = temp_attractors;
        convergences[i] = temp_convergences;
        pthread_mutex_lock(&row_done_mutex);
        row_done[i] = 1;
        pthread_mutex_unlock(&row_done_mutex);
    }
	return NULL;
}

void *write_main() {
    char file_1[50], file_2[50];

    sprintf(file_1, "%s%d%s", "newton_attractors_x", degree, ".ppm");
    sprintf(file_2, "%s%d%s", "newton_convergence_x", degree, ".ppm");
    
    FILE *attractor_fp = fopen(file_1, "w");
    FILE *convergence_fp = fopen(file_2, "w");

    fprintf(attractor_fp, "P3\n%d %d\n10\n", image_size, image_size);
    fprintf(convergence_fp, "P3\n%d %d\n100\n", image_size, image_size);
    
    char *row_done_loc = calloc(image_size, sizeof(char));
	struct timespec sleep_timespec = {0, 10000};

    for (int i = 0; i < image_size; ) {
        pthread_mutex_lock(&row_done_mutex);
        if (row_done[i] != 0) {
            memcpy(row_done_loc, row_done, image_size * sizeof(char));
        }
        pthread_mutex_unlock(&row_done_mutex);
        if (row_done_loc[i] == 0) {
            nanosleep(&sleep_timespec, NULL);
            continue;
        }
        for (; i < image_size && row_done_loc[i] != 0; i++) {
            fwrite(attractors[i], sizeof(char), 6 * image_size, attractor_fp);
            fwrite(convergences[i], sizeof(char), 9 * image_size, convergence_fp);
            fwrite("\n", sizeof(char), 1, attractor_fp);
            fwrite("\n", sizeof(char), 1, convergence_fp);
        }
    }

    fclose(attractor_fp);
    fclose(convergence_fp);

	free(row_done_loc);

	return NULL;
}

int main(int argc, char *argv[]) {
    // Checking input argumnets
	if (argc != 4) {
		printf("%s", "The program requires three inputs.\n");
		exit(1);
	}
 
    // Parsing command line arguments
    char *a1 = argv[1];
    char *a2 = argv[2];
    degree = atoi(argv[3]);

    char *size, *threads;
    if (a1[1] == 't') {
        threads = a1;
        size = a2;
    } else {
        threads = a2;
        size = a1;
    }

    char* SIZE = malloc(sizeof(char) * (strlen(size) - 2));
    char* THREADS = malloc(sizeof(char) * (strlen(threads) - 2));

    strncpy(SIZE, size + 2, strlen(size) - 2);
    strncpy(THREADS, threads + 2, strlen(threads) - 2);

    image_size = atoi(SIZE);
    n_threads = atoi(THREADS);

    free(SIZE);
    free(THREADS);

    // Calculating the roots
    double theta;
    roots = malloc(sizeof(double complex) * (degree + 1));
    for (int i = 0; i < degree; i++) {
    	theta = (2 * M_PI / degree) * (double) i;
    	roots[i+1] = cos(theta) + I * sin(theta);
    }
    roots[0] = 0;

    // Initialization of data structures
    points = malloc(sizeof(double) * image_size);
    attractors = malloc(sizeof(char *) * image_size);
    convergences = malloc(sizeof(char *) * image_size);
    row_done = malloc(sizeof(char) * image_size);
    for (int i = 0; i < image_size; i++) {
        points[i] = -range + i * 2.0 * range / (image_size - 1);
        row_done[i] = 0;
    }

    // Synchronization of threads
    //pthread_mutex_init(&row_done_mutex, NULL);
    pthread_t *compute_threads = malloc(sizeof(pthread_t) * n_threads);
    pthread_t *write_thread = malloc(sizeof(pthread_t));

    // Creating threads
    for (int i = 0; i < n_threads; i++) {
        int *arg = malloc(sizeof(int));
        *arg = i;
        pthread_create(&compute_threads[i], NULL, compute_main, arg);
    }
    pthread_create(write_thread, NULL, write_main, NULL);

    // Joining threads
    for (int i = 0; i < n_threads; i++) {
        pthread_join(compute_threads[i], NULL);
    }
    pthread_join(*write_thread, NULL);

    // Freeing memory
    pthread_mutex_destroy(&row_done_mutex);
    free(compute_threads);
    free(write_thread);
    free(roots);   
    free(points);
    for (int i; i < image_size; i++) {
        free(attractors[i]);
        free(convergences[i]);
    }
    free(attractors);
    free(convergences);
    free(row_done);

    return 0;
}

