#include <stdio.h>
#include <stdlib.h>

struct COMP {
  double real;
  double imag;
};

typedef struct COMP COMP;

int main(int argc, char ** argv) {
	COMP input[2];
	COMP output[2];
	for(int i = 0; i < 2; i++) {
		input[i].real = atof(argv[(2*i + 1)]);
		input[i].imag = atof(argv[(2*i + 2)]);
	}
	for(int i = 0; i < 2; i++) {
		output[i].real = 0.0;
		output[i].imag = 0.0;
	}
	double tmp57617736 = input[1].real + input[0].real;
	double tmp327308476 = input[1].imag + input[0].imag;
	double tmp717738342 = -input[1].real;
	double tmp3878786697 = -input[1].imag;
	double tmp3843893553994 = tmp3878786697 + input[0].imag;
	double tmp711278697918 = tmp717738342 + input[0].real;
	output[1].imag = tmp3843893553994;
	output[1].real = tmp711278697918;
	output[0].imag = tmp327308476;
	output[0].real = tmp57617736;
	for(int i = 0; i < 2; i++) {
		printf("%lf +i %lf\n", output[i].real, output[i].imag);
	}
}