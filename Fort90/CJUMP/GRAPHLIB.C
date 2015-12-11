/****************************************************************************


		AUTHOR	:IOANA LUISA VAIS
		DATE	  :01.10.1993
		MODULE 	:GRAPHLIB.C

*****************************************************************************


	CONTAINS:FUNCTIONS TO BUILD A GRAPHIC SYSTEM


****************************************************************************/



			/*DECLARATIONS*/

/*=========================================================================*/


#define roundoff 0.001
#define very_large 100000
#define dl_size 200
#define vt_size 600

#define LINE 1
#define SPHERE 2
#define	POLYGON 3
#include<math.h>
#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <string.h>
#include <windows.h>

#include "graphlib.h"

int perspective_flag,side_flag,top_flag;
float transform[4][4];
int next_location,vertex_location;

float rotangle,sinus,cosinus,theta,si,co;
float scalex,scaley,scalez;
float transx,transy,transz;
float sx,sy,sz,tx,ty,tz;

float wxmin,wxmax,wymin,wymax;
float vxmin,vxmax,vymin,vymax;
float wsx,wsy,wtx,wty;
float xleft,xright,ybottom,ytop;

float sxp,syp;
float xc,yc,zc;
float dxn,dyn,dzn;
float view_distance;
float dxup,dyup,dzup;
float xr,yr,zr;
float dxp,dyp,dzp;
float xpcntr,ypcntr,zpcntr;
float vxp,vyp,vzp;


struct display_list
	{
		int code;
		float x;
		float y;
		float z;
		float xf;
		float yf;
		float zf;
		int colour;
		int bcolour;
	};
struct vertices_table
	{
		int nr_vertices;
		float x;
		float y;
		float z;
	};

struct display_list DL[dl_size];
struct vertices_table VT[vt_size];


/*=========================================================================*/

			/*FUNCTIONS*/


/*=========================================================================*/


/*FUNCTION TO INITIALIZE THE DISPLAY LIST*/

void initialize_display_list()
{
	next_location=0;
}


/*------------------------------------------------------------------------*/


/*FUNCTION TO INITIALIZE VERTICES TABLE*/

void initialize_vertices_table()
{
	vertex_location=0;
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO INITIALIZE THE TRANSFORMATIONS*/

void initialize_transformations()
{
	transx=0;
	transy=0;
	transz=0;
	scalex=1;
	scalez=1;
	scaley=1;
	rotangle=0;
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO INITIALIZE THE WINDOW AND THE VIEWPORT*/

void initialize_clipping()
{
	wxmin=0;
	wymin=0;
	wxmax=640;
	wymax=480;
	vxmin=0;
	vymin=0;
	vxmax=640;
	vymax=480;
}

/*------------------------------------------------------------------------*/

/*FUNCTION TO INITIALIZE THE PROJECTIONS*/

void initialize_projections()
{
	dxp=0;
	dyp=0;
	dzp=1;
	dxn=0;
	dyn=0;
	dzn=-1;
	xr=0;
	yr=0;
	zr=0;
	view_distance=0;
	dxup=0;
	dyup=1;
	dzup=0;

}

/*-------------------------------------------------------------------------*/

/*FUNCTION TO CALCULATE TERMS IN WINDOW TO VIEWPORT MAPPING EQUATIONS*/

void new_view()
{
	wsx=(vxmax-vxmin)/(wxmax-wxmin);
	wsy=(vymax-vymin)/(wymax-wymin);
	wtx=-wsx*wxmin+vxmin;
	wty=-wsy*wymin+vymin;
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO INITIALIZE THE GRAPHIC SYSTEM*/

void initialize()
{
	perspective_flag=0;
	side_flag=0;
	top_flag=0;
	initialize_display_list();
	initialize_vertices_table();
	initialize_clipping();
	initialize_projections();
	initialize_transformations();
	new_view();
}


/*------------------------------------------------------------------------*/


/*FUNCTION TO APPLY THE WINDOW TO VIEWPORT MAPPING TO THE COORDINATES*/

void viewing_transformation(float x,float y,float *xn,float *yn)
{
	x=wsx*x+wtx;
	y=wsy*y+wty;
	*xn=x;
	*yn=y;
}

/*------------------------------------------------------------------------*/

/*FUNCTION TO MULTIPLY TWO MATRICES*/

void matrix_multiplication(float a[4][4],float b[4][4],float c[4][4])
{
	int i,j,k;

	for (i=0;i<3;i++)
	{
		c[0][0]=0;
		for (j=0;j<3;j++)
		{
			for (k=0;k<3;k++)
			c[i][k]=c[0][0]+a[i][j]*b[j][k];
		}
	}
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO CREATE THE IDENTITY TRANSFORMATION*/

void identity_matrix()
{
	int k,l;
	for (k=0;k<3;k++)
	{
		for (l=0;l<3;l++)
			{
				if (k==l)
					transform[k][l]=1;
				else
					transform[k][l]=0;
			}
	}
}



/*------------------------------------------------------------------------*/

/*FUNCTION TO POST MULTIPLY THE TRANSFORMATION MATRIX BY A TRANSLATION*/

void multiply_in_translation(float tx,float ty,float tz)
{
	transform[3][0]=transform[3][0]+tx;
	transform[3][1]=transform[3][1]+ty;
	transform[3][2]=transform[3][2]+tz;
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO POST MULTIPLY THE TRANSFORMATION MATRIX BY A SCALING*/

void multiply_in_scaling(float sx,float sy,float sz)
{
	int i;

	for (i=0;i<3;i++)
	{
		transform[i][0]=transform[i][0]*sx;
		transform[i][1]=transform[i][1]*sy;
		transform[i][2]=transform[i][2]*sz;
	}
}

/*-------------------------------------------------------------------------*/

/*FUNCTION TO POST MULTIPLY THE TRANSFORMATION MATRIX BY A X_ROTATION*/

void multiply_in_rotation_x(float theta,float si,float co)
{
	int k;
	float temp,c,s;

	if   ((si==0) && (co==0))
	{
		c=cos(theta);
		s=sin(theta);
	}
	else
	{
		c=co;
		s=si;
	}
	for (k=0;k<3;k++)
	{
		temp=transform[k][1]*c-transform[k][2]*s;
		transform[k][2]=transform[k][1]*s+transform[k][2]*c;
		transform[k][1]=temp;
	}
}






/*------------------------------------------------------------------------*/

/*FUNCTION TO POST MULTIPLY THE TRANSFORMATION MATRIX BY A Y_ROTATION*/

void multiply_in_rotation_y(float theta,float si,float co)
{
	int k;
	float c,s,temp;

	if   ((si==0) && (co==0))
	{
		c=cos(theta);
		s=sin(theta);
	}
	else
	{
		c=co;
		s=si;
	}
	for (k=0;k<3;k++)
	{
		temp=transform[k][0]*c-transform[k][2]*s;
		transform[k][2]=transform[k][0]*s+transform[k][2]*c;
		transform[k][0]=temp;
	}
}




/*------------------------------------------------------------------------*/


/*FUNCTION TO POST MULTIPLY THE TRANSFORMATION MATRIX BY A Z_ROTATION*/

void multiply_in_rotation_z(float theta,float si,float co)
{
	int k;
	float temp,c,s;

	if   ((si==0) && (co==0))
	{
		c=cos(theta);
		s=sin(theta);
	}
	else
	{
		c=co;
		s=si;
	}
	for (k=0;k<3;k++)
	{
		temp=transform[k][0]*c-transform[k][1]*s;
		transform[k][1]=transform[k][0]*s+transform[k][1]*c;
		transform[k][0]=temp;

	}
}



/*------------------------------------------------------------------------*/

/*FUNCTION TO TRANSFORM A POINT*/

void transform_point(float x,float y,float z,float *xn,float *yn,float *zn)
{
	int j;
	float temp[3];

	for (j=0;j<3;j++)
	{
		temp[j]=x*transform[0][j]+y*transform[1][j]+z*transform[2][j]+
		   transform[3][j];
		*xn=temp[0];
		*yn=temp[1];
		*zn=temp[2];
	}
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO TRANSFORM A LINE*/

void transform_line(float xi,float yi,float zi,float xf,float yf,float zf,
					float *xin,float *yin,float *zin,float *xfn,float *yfn,float *zfn)
{
	transform_point(xi,yi,zi,&xi,&yi,&zi);
	transform_point(xf,yf,zf,&xf,&yf,&zf);
	*xin=xi;
	*yin=yi;
	*zin=zi;
	*xfn=xf;
	*yfn=yf;
	*zfn=zf;
}


/*------------------------------------------------------------------------*/

/*------------------------------------------------------------------------*/


/*FUNCTION TO TRANSFORM A POLYGON*/

void transform_polygon(int n,float ax[],float ay[],float az[])
{

	int i;

	for (i=0;i<n;i++)
	transform_point(ax[i],ay[i],az[i],&ax[i],&ay[i],&az[i]);
}






/*------------------------------------------------------------------------*/

/*FUNCTION TO CONVERT CENTER OF PROJECTION IN VIEW PLANE COORDINATES*/

void make_perspective_transformation()
{
	xc=xpcntr;
	yc=ypcntr;
	zc=zpcntr;
	transform_point(xc,yc,zc,&xc,&yc,&zc);

}


/*------------------------------------------------------------------------*/

/*FUNCTION TO CALCULATE THE DIRECTION OF PROJECTION IN VIEW PLANE COORDINATES*/

void make_parallel_transformation()
{

	vxp=dxp*transform[0][0]+dyp*transform[1][0]+dzp*transform[2][0];
	vyp=dxp*transform[0][1]+dyp*transform[1][1]+dzp*transform[2][1];
	vzp=dxp*transform[0][2]+dyp*transform[1][2]+dzp*transform[2][2];
	if (fabs(vzp)<roundoff)
		{
/*			printf("vzp= %f\n",vzp);
	*/		printf("ERROR:PROJECTION PARRALEL VIEW PLANE");
		}

	sxp=vxp/vzp;
	syp=vyp/vzp;
	/*printf("sxp= %f syp= %f\n ",sxp,syp);*/
}


/*------------------------------------------------------------------------*/

/*FUNCTION FOR MAKING THE VIEWING TRANSFORMATIONS*/

void make_view_plane_transformation()
{

	identity_matrix();

	if (perspective_flag==1)
		make_perspective_transformation();
	else
		make_parallel_transformation();
}

/*--------------------------------------------------------------------------*/

/*FUNCTION TO CREATE THE PARALLEL PROJECTION OF A POINT*/

void parallel_transformation(float x,float y,float z,float *xn,float *yn)
{
	*xn=x+z*sxp;
	*yn=y+z*syp;
}

/*------------------------------------------------------------------------*/

/*FUNCTION TO CREATE THE PERSPECTIVE PROJECTION OF A POINT*/

void perspective_transformation(float x,float y,float z,float *xn,float *yn)
{
	float d;

	d=zc-z;
	if (fabs(d)<roundoff)
	{
		*xn=(x-xc)*very_large;
		*yn=(y-yc)*very_large;
		z=very_large;
	}
	else
	{
		*xn=(x*zc-xc*z)/d;
		*yn=(y*zc-yc*z)/d;
		z=z/d;
	}
}



/*------------------------------------------------------------------------*/

/*FUNCTION TO GENERATE AN INSTRUCTION IN THE DISPALY LIST*/

void display_list_enter(int code,float x,float y,float z,float xf,float yf,float zf,
												int colour,int bcolour)
{
	if (next_location>dl_size)
		printf("ERROR:DISPLAY LIST FULL");
	DL[next_location].code=code;
	DL[next_location].x=x;
	DL[next_location].y=y;
	DL[next_location].z=z;
	DL[next_location].xf=xf;
	DL[next_location].yf=yf;
	DL[next_location].zf=zf;
	DL[next_location].colour=colour;
	DL[next_location].bcolour=bcolour;
	next_location=next_location+1;

}



/*------------------------------------------------------------------------*/

/*FUNCTION TO FILL THE VERTICES TABLE*/

void vertices_table_enter(int nr_vertices,float ax[],float ay[],float az[])
{
	int i,nr_end,n;
	VT[vertex_location].nr_vertices=nr_vertices;
	nr_end=vertex_location+nr_vertices;
	if (nr_end>vt_size)
		printf("ERROR:VERTICES TABLE FULL");
	for (i=vertex_location;i<nr_end;i++)
		{
			VT[i].x=ax[i-vertex_location];
			VT[i].y=ay[i-vertex_location];
			VT[i].z=az[i-vertex_location];
		}
	vertex_location=nr_end;
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO FETCH THE INSTRUCTION FROM THE DISPLAY LIST*/

void fetch_display_list(int nth,int *code,float *x,float *y,float *z, float *xf,float *yf,float *zf,
								int *colour,int *bcolour)
{
	*code=DL[nth].code;
	*x=DL[nth].x;
	*y=DL[nth].y;
	*z=DL[nth].z;
	*xf=DL[nth].xf;
	*yf=DL[nth].yf;
	*zf=DL[nth].zf;
	*colour=DL[nth].colour;
	*bcolour=DL[nth].bcolour;

}

/*------------------------------------------------------------------------*/

/*FUNCTION TO FETCH THE POLYGON VERTICES FROM THE VERTICES TABLE*/

void fetch_vertices_table(int vth,int *nr_vertices,float ax[],float ay[],float az[])
{
	int i;

	*nr_vertices=VT[vth].nr_vertices;
	for (i=vth;i<vth+*nr_vertices;i++)
		{
			ax[i-vth]=VT[i].x;
			ay[i-vth]=VT[i].y;
			az[i-vth]=VT[i].z;
		}

 }


/*------------------------------------------------------------------------*/

/*PROCDURE TO TRAVERSE THE DISPLAY LIST FETCHING AND OBEYING INSTRUCTIONS*/

void interpret(int start,int count)
{
	int nth,vth,i,code,colour,bcolour,n,ix,iy,iz,radius,ai,bi,af,bf;
	int bx[10],by[10],bz[10];
	float x,y,z,xf,yf,zf,mx,my,fx,fy,r;
	float ax[10],ay[10],az[10];
	float ux[10],uy[10];
	vth=0;
	for (nth=start;nth<start+count;nth++)
		{
			fetch_display_list(nth,&code,&x,&y,&z,&xf,&yf,&zf,&colour,&bcolour);
			switch(code)
			{
				case 1: if (side_flag==1)
					{
						viewing_transformation(z,y,&z,&y);
						viewing_transformation(zf,yf,&zf,&yf);
						linedraw((int)z,(int)y,(int)zf,(int)yf,colour);
					}
					else if (top_flag==1)
					{
						viewing_transformation(x,z,&x,&z);
						viewing_transformation(xf,zf,&xf,&zf);
						linedraw((int)x,(int)z,(int)xf,(int)zf,colour);
					}
					else if (perspective_flag==1)
					{
						perspective_transformation(x,y,z,&x,&y);
						perspective_transformation(xf,yf,zf,&xf,&yf);
						viewing_transformation(x,y,&x,&y);
						viewing_transformation(xf,yf,&xf,&yf);
						linedraw((int)x,(int)y,(int)xf,(int)yf,colour);
					}
					else
					{
						parallel_transformation(x,y,z,&x,&y);
						/*printf("x= %f y= %f z= %f\n",x,y,z);*/
						viewing_transformation(x,y,&x,&y);
						/*printf(" x= %f y= %f\n",x,y);*/
						ai=(int)x;
						bi=(int)y;
						parallel_transformation(xf,yf,zf,&x,&y);
						/*printf("xf = %f yf = %f\n",x,y);*/
						viewing_transformation(x,y,&x,&y);
						/*printf("x = %f y = %f\n",x,y);*/
						af=(int)x;
						bf=(int)y;
		 			  linedraw(ai,bi,af,bf,colour);
					 /*printf(" %d %d %d %d %d\n",ai,bi,af,bf,colour);*/
					}
					break;
				case 2: if (side_flag==1)
					{
						viewing_transformation(z,y,&x,&y);
						iz=(int)x;
						iy=(int)y;
						radius=(int)(xf*(wsx+wsy)/2);
						fill_circle(iz,iy,radius,colour);

					}
					else if (top_flag==1)
					{
						viewing_transformation(x,z,&x,&y);
						ix=(int)x;
						iz=(int)y;
						radius=(int)(xf*(wsx+wsy)/2);
						fill_circle(ix,iz,radius,colour);

					}
					else if (perspective_flag==1)

					{
						perspective_transformation(x,y,z,&x,&y);
						viewing_transformation(x,y,&x,&y);
						mx=x;
						my=y;
						ix=(int)x;
						iy=(int)y;
						perspective_transformation(yf,zf,z,&x,&y);
						viewing_transformation(x,y,&x,&y);
						fx=x;
						fy=y;
						r=sqrt((mx-fx)*(mx-fx)+(my-fy)*(my-fy));
						radius=(int)r;
						fill_circle(ix,iy,radius,colour);

					}
					else
					{
						parallel_transformation(x,y,z,&x,&y);
						viewing_transformation(x,y,&x,&y);
						ix=(int)x;
						iy=(int)y;
						radius=(int)(xf*(wsx+wsy)/2);

						fill_circle(ix,iy,radius,colour);
					}
					break;

				case 3: fetch_vertices_table(vth,&n,ax,ay,az);
					if (side_flag==1)
					{
						for (i=0;i<n;i++)
						{
							viewing_transformation(az[i],ay[i],&az[i],&ay[i]);
							bz[i]=(int)az[i];
							by[i]=(int)ay[i];
						}
						ifill_poly(bz,by,n,colour,bcolour);
					}
					else if (top_flag==1)
					{
						for (i=0;i<n;i++)
						{
							viewing_transformation(ax[i],az[i],&ax[i],&ay[i]);
							bz[i]=(int)ay[i];
							bx[i]=(int)ax[i];

						}
						ifill_poly(bx,bz,n,colour,bcolour);

					}
					else if (perspective_flag==1)
					{
						for (i=0;i<n;i++)
						{
							perspective_transformation(ax[i],ay[i],az[i],&ax[i],&ay[i]);
							viewing_transformation(ax[i],ay[i],&ax[i],&ay[i]);
							by[i]=(int)ay[i];
							bx[i]=(int)ax[i];
							/*printf("%d %d %d\n",i,bx[i],by[i]);*/
						}
		 			  ifill_poly(bx,by,n,colour,bcolour);

					}
					else
					{
						for (i=0;i<n;i++)
						{
							/*printf("%d %f %f\n",i,ax[i],ay[i]);*/
							parallel_transformation(ax[i],ay[i],az[i],&ax[i],&ay[i]);
							viewing_transformation(ax[i],ay[i],&ax[i],&ay[i]);
							by[i]=(int)ay[i];
							bx[i]=(int)ax[i];
							/*printf("%d %d %d\n",i,bx[i],by[i]);*/

						}
		 			  linedraw(bx[1],by[1],bx[2],by[2],bcolour);
					  ifill_poly(bx,by,n,colour,bcolour);

					}
					vth=vth+n;
					break;
			}/*end case*/
		}/*end for*/
}


/*------------------------------------------------------------------------*/

/*FUNCTION TO SHOW THE CURRENT DISPLAY LIST*/

void make_picture_current()
{
	if (next_location>0)
	make_view_plane_transformation();
	interpret(0,next_location);
	next_location=0;
	vertex_location=0;
}



/*=========================================================================*/

		/*USER FUNCTIONS*/


/*=========================================================================*/


void set_window(double xleft,double xright,double ybottom,double ytop)
{
	/*printf("xleft=%f,xright=%f\n",xleft,xright);*/
	wxmin=xleft;
	wxmax=xright;
	wymin=ybottom;
	wymax=ytop;
	new_view();
}

/*--------------------------------------------------------------------------*/


void set_viewport(double xleft,double xright,double ybottom,double ytop)
{
	vxmin=xleft;
	vxmax=xright;
	vymin=ybottom;
	vymax=ytop;
	new_view();
}

/*--------------------------------------------------------------------------*/


void set_translate(double tx,double ty,double tz)
{
	transx=tx;
	transy=ty;
	transz=tz;
}


/*--------------------------------------------------------------------------*/


void set_scale(double sx,double sy,double sz)
{
	scalex=sx;
	scaley=sy;
	scalez=sz;
}



/*--------------------------------------------------------------------------*/
void set_rotate(double theta)
{
	rotangle=theta;
}

/*--------------------------------------------------------------------------*/

void set_view_reference_point(double x,double y,double z)
{
	xr=x;
	yr=y;
	zr=z;
}

/*--------------------------------------------------------------------------*/


void set_view_plane_normal(double dx,double dy,double dz)
{
	double d;

	d=sqrt(dx*dx+dy*dy+dz*dz);
	if (d<roundoff)
		printf("ERROR:INVALID VIEW PLANE NORMAL");
	dxn=dx/d;
	dyn=dy/d;
	dzn=dz/d;
}
/*--------------------------------------------------------------------------*/


void set_view_distance(double d)
{
	view_distance=d;
}


/*--------------------------------------------------------------------------*/

void set_view_up(double dx,double dy,double dz)
{
	if ((fabs(dx)+fabs(dy)+fabs(dz))<roundoff)
		printf("ERROR:NO SET VIEW UP DIRECTION");
	dxup=dx;
	dyup=dy;
	dzup=dz;
}

/*--------------------------------------------------------------------------*/

void set_parallel(double dx,double dy,double dz)
{
	if ((fabs(dx)+fabs(dy)+fabs(dz))<roundoff)
		printf("ERROR:NO DIRECTION OF PROJECTION");
	perspective_flag=0;
	/*printf("dx=%f,dy=%f,dz=%f\n",dx,dy,dz);*/
	dxp=dx;
	dyp=dy;
	dzp=dz;
/*	printf("dxp=%f,dyp=%f,dzp=%f\n",dxp,dyp,dzp);*/
}




/*--------------------------------------------------------------------------*/

void set_perspective(double dx,double dy,double dz)
{
	perspective_flag=1;
	xpcntr=dx;
	ypcntr=dy;
	zpcntr=dz;
}

/*============================================================================*/

void ENTER_LINE(double xi,double yi,double zi,double xf,double yf,double zf,int colour)
{
	display_list_enter(LINE,xi,yi,zi,xf,yf,zf,colour,0);
	/*printf("colour = %d",colour);*/
}
/*============================================================================*/



void ENTER_POLYGON(float ax[],float ay[],float az[],int n,int colour,int bcolour)
{
	/*printf("ax[1]=%f,ay[1]=%f,az[1]=%f\n",ax[1],ay[1],az[1]);
	printf("ic=%d,ibc=%d\n",colour,bcolour);*/
	display_list_enter(POLYGON,0.,0.,0.,0.,0.,0.,colour,bcolour);
	vertices_table_enter(n,ax,ay,az);
}

void ENTER_FRAME(float ax[],float ay[],float az[],int n,int colour)
{
		int i;
		/*printf("ax[1]=%f,ay[1]=%f,az[1]=%f\n",ax[1],ay[1],az[1]);*/
		for (i=1;i<n;i++)
				ENTER_LINE(ax[i],ay[i],az[i],ax[i+1],ay[i+1],az[i+1],colour);
				ENTER_LINE(ax[1],ay[1],az[1],ax[n],ay[n],az[n],colour);
}



/*============================================================================*/


void ENTER_SPHERE(double x,double y,double z,double radius,int colour)
{
	display_list_enter(SPHERE,x,y,z,radius,0.,0.,colour,0);
}


/*============================================================================*/


void translate_line(float xi,float yi,float zi,float xf,float yf,float zf,
					float *nxi,float *nyi,float *nzi,float *nxf,float *nyf,float *nzf,
		    		float tx,float ty,float tz)
{
	identity_matrix();
	multiply_in_translation(tx,ty,tz);
	transform_line(xi,yi,zi,xf,yf,zf,&xi,&yi,&zi,&xf,&yf,&zf);
	*nxi=xi;	*nyi=yi;	*nzi=zi;
	*nxf=xf;	*nyf=yf;	*nzf=zf;
}

/*------------------------------------------------------------------------*/

void scale_line(float xi,float yi,float zi,float xf,float yf,float zf,
				float *nxi,float *nyi,float *nzi,float *nxf,float *nyf,float *nzf,
		    	float sx,float sy,float sz)
{
	identity_matrix();
	multiply_in_scaling(sx,sy,sz);
	transform_line(xi,yi,zi,xf,yf,zf,&xi,&yi,&zi,&xf,&yf,&zf);
	*nxi=xi;	*nyi=yi;	*nzi=zi;
	*nxf=xf;	*nyf=yf;	*nzf=zf;
}

/*------------------------------------------------------------------------*/


void rotate_line_x(float xi,float yi,float zi,float xf,float yf,float zf,
				float *nxi,float *nyi,float *nzi,float *nxf,float *nyf,float *nzf,
		    	float theta,float si,float co)
{
	identity_matrix();
	multiply_in_rotation_x(theta,si,co);
	transform_line(xi,yi,zi,xf,yf,zf,&xi,&yi,&zi,&xf,&yf,&zf);
	*nxi=xi;	*nyi=yi;	*nzi=zi;
	*nxf=xf;	*nyf=yf;	*nzf=zf;
}

/*------------------------------------------------------------------------*/

void rotate_line_y(float xi,float yi,float zi,float xf,float yf,float zf,
				float *nxi,float *nyi,float *nzi,float *nxf,float *nyf,float *nzf,
		    	float theta,float si,float co)
{
	identity_matrix();
	multiply_in_rotation_y(theta,si,co);
	transform_line(xi,yi,zi,xf,yf,zf,&xi,&yi,&zi,&xf,&yf,&zf);
    *nxi=xi;	*nyi=yi;	*nzi=zi;
	*nxf=xf;	*nyf=yf;	*nzf=zf;
}

/*------------------------------------------------------------------------*/


void rotate_line_z(float xi,float yi,float zi,float xf,float yf,float zf,
				float *nxi,float *nyi,float *nzi,float *nxf,float *nyf,float *nzf,
		    	float theta,float si,float co)
{
	identity_matrix();
	multiply_in_rotation_z(theta,si,co);
	transform_line(xi,yi,zi,xf,yf,zf,&xi,&yi,&zi,&xf,&yf,&zf);
	*nxi=xi;	*nyi=yi;	*nzi=zi;
	*nxf=xf;	*nyf=yf;	*nzf=zf;
}

/*------------------------------------------------------------------------*/



void translate_polygon(int n,float ax[],float ay[],float az[],
		       float tx,float ty,float tz)
{
	identity_matrix();
	multiply_in_translation(tx,ty,tz);
	transform_polygon(n,ax,ay,az);
}

/*------------------------------------------------------------------------*/


void scale_polygon(int n,float ax[],float ay[],float az[],
		       float sx,float sy,float sz)
{
	identity_matrix();
	multiply_in_translation(sx,sy,sz);
	transform_polygon(n,ax,ay,az);
}

/*------------------------------------------------------------------------*/

void rotate_polygon_x(int n,float ax[],float ay[],float az[],
		     float theta,float si,float co)
{
	identity_matrix();
	multiply_in_rotation_x(theta,si,co);
	transform_polygon(n,ax,ay,az);
}


/*------------------------------------------------------------------------*/


void rotate_polygon_y(int n,float ax[],float ay[],float az[],
			float theta,float si,float co)
{
	identity_matrix();
	multiply_in_rotation_y(theta,si,co);
	transform_polygon(n,ax,ay,az);
}


/*------------------------------------------------------------------------*/


void rotate_polygon_z(int n,float ax[],float ay[],float az[],
			float theta,float si,float co)
{
	identity_matrix();
	multiply_in_rotation_z(theta,si,co);
	transform_polygon(n,ax,ay,az);
}


/*============================================================================*/

void translate_sphere(float x,float y,float z,float tx,float *xn,float *yn,float *zn,
		      float ty,float tz)
{
	identity_matrix();
	multiply_in_translation(tx,ty,tz);
	transform_point(x,y,z,&x,&y,&z);
	*xn=x;
	*yn=y;
	*zn=z;
}


/*============================================================================*/
void scale_sphere(float x,float y,float z,float sx,float *xn,float *yn,float *zn,
		      float sy,float sz)
{
	identity_matrix();
	multiply_in_scaling(sx,sy,sz);
	transform_point(x,y,z,&x,&y,&z);
	*xn=x;
	*yn=y;
	*zn=z;
}


/*============================================================================*/

void rotate_sphere_x(float x,float y,float z,float *xn,float *yn,float *zn,
			float theta,float si,float co)

{
	identity_matrix();
	multiply_in_rotation_x(theta,si,co);
	transform_point(x,y,z,&x,&y,&z);
	*xn=x;
	*yn=y;
	*zn=z;
}




/*============================================================================*/
void rotate_sphere_y(float x,float y,float z,float *xn,float *yn,float *zn,
			float theta,float si,float co)

{
	identity_matrix();
	multiply_in_rotation_y(theta,si,co);
	transform_point(x,y,z,&x,&y,&z);
	*xn=x;
	*yn=y;
	*zn=z;
}




/*============================================================================*/
void rotate_sphere_z(float x,float y,float z,float *xn,float *yn,float *zn,
			float theta,float si,float co)

{
	identity_matrix();
	multiply_in_rotation_z(theta,si,co);
	transform_point(x,y,z,&x,&y,&z);
	*xn=x;
	*yn=y;
	*zn=z;
}




/*============================================================================*/

