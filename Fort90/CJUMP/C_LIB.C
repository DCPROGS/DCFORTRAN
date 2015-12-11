
#include<malloc.h>
#include<stdio.h>
#include<dos.h>
#include<conio.h>
#include<string.h>
#include"myhead.h"
void wpixel(int *,int *,int *);

#pragma Data(Common,"_lc_mydata");
	float arx[4];
	float	ary[4];
#pragma Data;
typedef unsigned char boolean;

/*-------------------------------------------------------------------------

function to divide a number by 5

--------------------------------------------------------------------------*/

double divide_by_5 (double *number)
{
	return(*number/5.0);
} /*end of divide_by_5*/


/*-------------------------------------------------------------------------

function to add two numbers, return result in c

--------------------------------------------------------------------------*/
void added_1(float *a,float *b,float *c)
{
	*c = *a + *b;
	return;
}/*end of added_1*/

/*-------------------------------------------------------------------------

 function to add two numbers, return result in x

 -------------------------------------------------------------------------*/

void added_2(int *x,int *y)
{
		*x = *x+*y;
		return;
}/*end of added_2*/


/*-------------------------------------------------------------------------

 function to add two numbers, return result in x

--------------------------------------------------------------------------*/

void added_3(int *result, int *x, int *y)
{
		*result = *x+*y;
		return;
}/*end of added_3*/


/*------------------------------------------------------------------------*/

/*function to set a pixel on the screen;calls assembly function: newpixel */

/*------------------------------------------------------------------------*/
void	cwpixel(int *xc,int *yc, int *colc)
{
		int *x,*y,*c;
		/*newpixel(xc,yc,colc);*/
  	wpixel(xc,yc,colc);
  	wpixel(x,y,c);
}/*end of cwpixel*/

/*------------------------------------------------------------------------*/

/*function to sort in ascending order 'n' numbers contained in an array */

/*------------------------------------------------------------------------*/
void Sort_array_1(int Tab[],int *n)
{
   	boolean Swap;
   	int Temp,I,Last;
	 	Last=*n;
	 	do
	 		{
      	Swap = 0;
      	for (I = 0; I<Last; I++)
	 			if (Tab[I] > Tab[I+1])
	 				{
	    			Temp = Tab[I];
	    			Tab[I] = Tab[I+1];
	    			Tab[I+1] = Temp;
	    			Swap = 1;
	    		}
      }
   	while (Swap);
}/*end of Sort_array_1*/


/*------------------------------------------------------------------------*/

/* function to test COMMON */

/*------------------------------------------------------------------------*/
void test_common(float arx[],float ary[])
{
	int i;

	for (i = 0;i<4; i++)
			{
				arx[i]=arx[i]+10;
				ary[i]=ary[i]-10;
			}
}/*end of test_commom*/

/*------------------------------------------------------------------------*/

/* function to add two matrices*/

/*------------------------------------------------------------------------*/
void add_matrix(float a[2][3], float b[2][3], float c[2][3])
{
	int i,j;

	for (i=0;i<2;i++)
			{
					for (j=0;j<3;j++)
							c[i][j]=a[i][j]+b[i][j];
			}
}/*end of add_matrix*/

/*------------------------------------------------------------------------*/

/* function to print an array of strings */

/*------------------------------------------------------------------------*/

 void write_text(char *text[])
 {
			printf(" \n");
			printf("%s\n",*text);
			printf(" \n");
		  puts(*text);
 }/*end of write_text*/


/*------------------------------------------------------------------------*/

void wt_string(char *intext[])
{
	int i;
	char *names[4];

	names[0] = "IONESCU";
	names[1] = "Popa   ";
	names[2] = "MIHAILY";
	names[3] = "CRISTEA";
  for (i=0; i<4; i++)
			printf("%s\n",names[i]);
	names[0] = intext[0];

}

/**************************************************************************/
