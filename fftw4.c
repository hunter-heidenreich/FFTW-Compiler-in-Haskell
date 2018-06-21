#include <stdio.h>
#include <stdlib.h>

struct COMP {
  double real;
  double imag;
};

typedef struct COMP COMP;

int main(int argc, char ** argv) {
	COMP input[4];
	COMP output[4];
	for(int i = 0; i < 4; i++) {
		input[i].real = atof(argv[(2*i + 1)]);
		input[i].imag = atof(argv[(2*i + 2)]);
	}
	for(int i = 0; i < 4; i++) {
		output[i].real = 0.0;
		output[i].imag = 0.0;
	}
	double tmp115233485 = input[2].real + input[0].real;
	double tmp230464983 = input[1].real + input[3].real;
	double tmp1435464297 = -input[2].real;
	double tmp2153190252 = -input[3].real;
	double tmp47269433507 = input[2].imag + input[0].imag;
	double tmp47860301401 = input[1].imag + input[3].imag;
	double tmp294420361587 = -input[1].imag;
	double tmp298100615952 = -input[2].imag;
	double tmp1422545119323 = tmp1435464297 + input[0].real;
	double tmp2133869156477 = tmp2153190252 + input[1].real;
	double tmp2845090215177 = -tmp230464983;
	double tmp291794803917367 = tmp294420361587 + input[3].imag;
	double tmp295441049691241 = tmp298100615952 + input[0].imag;
	double tmp590835420795387 = -tmp47860301401;
	double tmp3602206854359895657 = -tmp291794803917367;
	double tmp26342614736708607 = -tmp2133869156477;
	double tmp7951569210612697757 = tmp26342614736708607 + tmp295441049691241;
	double tmp585564746016833959 = tmp590835420795387 + tmp47269433507;
	double tmp294896744578088543 = tmp2133869156477 + tmp295441049691241;
	double tmp290578392895359795 = tmp291794803917367 + tmp1422545119323;
	double tmp2819598599624047 = tmp2845090215177 + tmp115233485;
	double tmp94616154475621 = tmp230464983 + tmp47860301401 + tmp115233485 + tmp47269433507;
	double tmp_8879947886783168319 = tmp3602206854359895657 + tmp1422545119323;
	output[1].imag = tmp294896744578088543;
	output[2].imag = tmp585564746016833959;
	output[2].real = tmp2819598599624047;
	output[3].real = tmp_8879947886783168319;
	output[0].real = tmp94616154475621;
	output[1].real = tmp290578392895359795;
	output[3].imag = tmp7951569210612697757;
	for(int i = 0; i < 4; i++) {
		printf("%lf +i %lf\n", output[i].real, output[i].imag);
	}
}