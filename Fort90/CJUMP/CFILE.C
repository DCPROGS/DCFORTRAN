#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <string.h>
void cfile(double [],long *);
	void cfile(double data[],long *n)
	{
		FILE *fptr;
		long m=*n;
		int i;
		fptr= fopen("textc.dat","w");
		for(i=0;i<m;i++)
				fprintf(fptr,"%lf",data[i]);
		fclose(fptr);
	}
