
#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <string.h>
#include "ivlib.h"


void tryline(int x1,int y1,int x2,int y2,int col)
{
/*		printf("%d %d %d %d %d\n",x1,y1,x2,y2,col);
		x1=100;
		x2=400;
		y1=100;
		y2=400;
		col=8; */
		printf("inside c");
		printf("%d %d %d %d\n",x1,y1,x2,y2);
		linedraw(x1,y1,x2,y2,col);
		printf("%d %d %d %d\n",x1,y1,x2,y2);
}


