#include<stdio.h>
#include"pnorm.h"

void main(){
	double x;
	printf(" x: ");
	scanf("%lf", &x);
	printf(" pnorm(%2.2f) = %.20e\n\n",x,pnorm(x));
}
