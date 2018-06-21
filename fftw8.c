#include <stdio.h>
#include <stdlib.h>

struct COMP {
  double real;
  double imag;
};

typedef struct COMP COMP;

int main(int argc, char ** argv) {
	COMP input[8];
	COMP output[8];
	for(int i = 0; i < 8; i++) {
		input[i].real = atof(argv[(2*i + 1)]);
		input[i].imag = atof(argv[(2*i + 2)]);
	}
	for(int i = 0; i < 8; i++) {
		output[i].real = 0.0;
		output[i].imag = 0.0;
	}
	double tmp230464983 = input[4].real + input[0].real;
	double tmp345696481 = input[5].real + input[1].real;
	double tmp460927979 = input[2].real + input[6].real;
	double tmp576159477 = input[3].real + input[7].real;
	double tmp2870916207 = -input[4].real;
	double tmp3588642162 = -input[5].real;
	double tmp4306368117 = -input[6].real;
	double tmp5024094072 = -input[7].real;
	double tmp47860301401 = input[0].imag + input[4].imag;
	double tmp48451169295 = input[1].imag + input[5].imag;
	double tmp49042037189 = input[2].imag + input[6].imag;
	double tmp49632905083 = input[7].imag + input[3].imag;
	double tmp298100615952 = -input[2].imag;
	double tmp305461124682 = -input[4].imag;
	double tmp309141379047 = -input[5].imag;
	double tmp316501887777 = -input[7].imag;
	double tmp685170425347 = tmp460927979 + tmp230464983;
	double tmp913559254383 = tmp345696481 + tmp576159477;
	double tmp2845077962133 = tmp2870916207 + input[0].real;
	double tmp3556401999287 = tmp3588642162 + input[1].real;
	double tmp4267726036441 = input[2].real + tmp4306368117;
	double tmp4979050073595 = input[3].real + tmp5024094072;
	double tmp5690155900797 = -tmp460927979;
	double tmp7112688743607 = -tmp576159477;
	double tmp96030217542695 = tmp49042037189 + tmp47860301401;
	double tmp97201317708603 = tmp48451169295 + tmp49632905083;
	double tmp295442822294923 = tmp298100615952 + input[6].imag;
	double tmp302735313842671 = tmp305461124682 + input[0].imag;
	double tmp306382741352333 = tmp309141379047 + input[1].imag;
	double tmp313677596371657 = tmp316501887777 + input[3].imag;
	double tmp598129684946817 = -tmp48451169295;
	double tmp605423949098247 = -tmp49042037189;
	double tmp52685077919864187 = -tmp4267726036441;
	double tmp61466373158530317 = -tmp4979050073595;
	double tmp3647241641230824477 = -tmp295442822294923;
	double tmp3782294941994550927 = -tmp306382741352333;
	double tmp3872349927208105707 = -tmp313677596371657;
	double tmp5112650900475904330 = 0.7071067811865476;
	double tmp5112650900475887611 = 0.7071067811865475;
	double tmp7981570351484211134 = tmp5112650900475904330 * tmp313677596371657;
	double tmp7981570350958983749 = tmp5112650900475887611 * tmp313677596371657;
	double tmp7752402481052147674 = tmp5112650900475904330 * tmp306382741352333;
	double tmp7752402480526920289 = tmp5112650900475887611 * tmp306382741352333;
	double tmp_1716194480469406596 = tmp5112650900475904330 * tmp4979050073595;
	double tmp_1716194480994633981 = tmp5112650900475887611 * tmp4979050073595;
	double tmp_1760886969723792416 = tmp5112650900475904330 * tmp3556401999287;
	double tmp_1760886970249019801 = tmp5112650900475887611 * tmp3556401999287;
	double tmp8888072813518311381 = -tmp_1716194480994633981;
	double tmp8425891389871268216 = -tmp7981570351484211134;
	double tmp8425884905939200391 = -tmp7981570350958983749;
	double tmp1700367699677183939 = -tmp7752402480526920289;
	double tmp_2259720528575898204 = tmp7752402481052147674 + tmp_1760886970249019801;
	double tmp8475345105127069090 = tmp_1716194480469406596 + tmp8425884905939200391;
	double tmp2666537009190377752 = tmp8425891389871268216 + tmp8888072813518311381;
	double tmp1199950267112704077 = -tmp97201317708603;
	double tmp600022563115051173 = tmp605423949098247 + tmp47860301401;
	double tmp592795703991232905 = tmp598129684946817 + tmp49632905083;
	double tmp304240012520199997 = tmp4267726036441 + tmp302735313842671;
	double tmp295603309154742501 = tmp295442822294923 + tmp2845077962133;
	double tmp11277888995358177 = -tmp913559254383;
	double tmp7049017130127213 = tmp7112688743607 + tmp345696481;
	double tmp5639172888487985 = tmp5690155900797 + tmp230464983;
	double tmp_1142552515064586221 = tmp3647241641230824477 + tmp2845077962133;
	double tmp_2829309306525158565 = tmp52685077919864187 + tmp302735313842671;
	double tmp_4634364395060345854 = tmp_1760886969723792416 + tmp1700367699677183939;
	double tmp_4772885820621284946 = -tmp_2259720528575898204;
	double tmp8654259936863247633 = tmp1199950267112704077 + tmp96030217542695;
	double tmp5237932416074082219 = tmp2666537009190377752 + tmp_4634364395060345854;
	double tmp2753152629097750283 = tmp592795703991232905 + tmp5639172888487985;
	double tmp193076792546648753 = tmp913559254383 + tmp97201317708603 + tmp685170425347 + tmp96030217542695;
	double tmp_1528565256779891713 = tmp_2259720528575898204 + tmp8475345105127069090;
	double tmp_1764919722968684875 = tmp_4772885820621284946 + tmp8475345105127069090;
	double tmp_5213603897127313553 = -tmp7049017130127213;
	double tmp_5294431490921779285 = -tmp592795703991232905;
	double tmp_7134618409443422797 = tmp7049017130127213 + tmp600022563115051173;
	double tmp_7269677075418079327 = tmp11277888995358177 + tmp685170425347;
	double tmp_9038793116336286078 = -tmp2666537009190377752;
	double tmp8257800382458193153 = tmp_9038793116336286078 + tmp_4634364395060345854;
	double tmp6437698082566579517 = -tmp5237932416074082219;
	double tmp5050913810028467573 = tmp5237932416074082219 + tmp295603309154742501;
	double tmp4178791803475916105 = tmp_1528565256779891713 + tmp304240012520199997;
	double tmp2733428273816782193 = tmp_5213603897127313553 + tmp600022563115051173;
	double tmp881092457108106225 = -tmp_1528565256779891713;
	double tmp_2317870237479019351 = tmp_5294431490921779285 + tmp5639172888487985;
	double tmp_2329228997434323337 = -tmp_1764919722968684875;
	double tmp_3612912392281604035 = tmp_1764919722968684875 + tmp_1142552515064586221;
	output[4].imag = tmp8654259936863247633;
	output[2].imag = tmp_7134618409443422797;
	output[2].real = tmp2753152629097750283;
	output[4].real = tmp_7269677075418079327;
	output[0].real = tmp193076792546648753;
	double tmp9005662897266780219 = tmp_2329228997434323337 + tmp_1142552515064586221;
	double tmp5837970127412243811 = -tmp8257800382458193153;
	double tmp_5019675487027565149 = tmp6437698082566579517 + tmp295603309154742501;
	double tmp_5927143315759837417 = tmp881092457108106225 + tmp304240012520199997;
	double tmp_6814613273551795159 = tmp8257800382458193153 + tmp_2829309306525158565;
	output[6].imag = tmp2733428273816782193;
	output[6].real = tmp_2317870237479019351;
	output[1].imag = tmp4178791803475916105;
	output[1].real = tmp5050913810028467573;
	output[3].real = tmp_3612912392281604035;
	double tmp_6789666441845883001 = tmp5837970127412243811 + tmp_2829309306525158565;
	output[5].real = tmp_5019675487027565149;
	output[7].real = tmp9005662897266780219;
	output[3].imag = tmp_6814613273551795159;
	output[5].imag = tmp_5927143315759837417;
	output[7].imag = tmp_6789666441845883001;
	for(int i = 0; i < 8; i++) {
		printf("%lf +i %lf\n", output[i].real, output[i].imag);
	}
}