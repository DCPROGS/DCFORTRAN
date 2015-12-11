#include<alloc.h>
#include<bios.h>
#include<process.h>
#include<graphics.h>
#include<conio.h>
#include<dos.h>
#include<stdio.h>
#include<math.h>
#define video 0x10
#include "luisa\bioslib.h"
#include "luisa\mikelib.h"
#include "luisa\smallib.h"
void scene1(int);
void chess_table(float,float,float,float,int,int,int,int);
void avion(int,int);
void enter_data();
void planar_movement(float,float,float,float,float,float,float,float,
                     float,float,float,float,float,float,float,float,
					 float,float,float,int,int,int);
void planar_movement_3D(float,float,float,float,float,float,float,float,
                     float,float,float,float,float,float,float,float,
					 float,float,float,int,int,int);


void chess_table(float xmin,float xmax,float ymin,float ymax,int nx,int ny,int colour1,int colour2)
{
	float cx[4],cy[4];
	int icx[4],icy[4];
	int i;
	float x,y,rx,ry;


	rx=(xmax-xmin)/nx;
	ry=(ymax-ymin)/ny;
	icx[0]=(int)xmin;	icy[0]=(int)ymin;
	icx[1]=(int)xmin;	icy[1]=(int)ymax;
	icx[2]=(int)xmax;	icy[2]=(int)ymax;
	icx[3]=(int)xmax;	icy[3]=(int)ymin;
    fill_poly(4,icx,icy,colour1);
	for (x=xmin+rx/2;x<xmax;x+=2*rx)
		{
			for (y=ymin+ry/2;y<ymax;y+=2*ry)
				{
					cx[0]=x-rx/2;	cy[0]=y-ry/2;
					cx[1]=x-rx/2;	cy[1]=y+ry/2;
					cx[2]=x+rx/2;	cy[2]=y+ry/2;
					cx[3]=x+rx/2;	cy[3]=y-ry/2;
                    for (i=0;i<4;i++)
						{
							icx[i]=(int)cx[i];
							icy[i]=(int)cy[i];
						}
					fill_poly(4,icx,icy,colour2);
				}
		}
	for (x=xmin+3*rx/2;x<xmax;x+=2*rx)
		{
			for (y=ymin+3*ry/2;y<ymax;y+=2*ry)
				{
					cx[0]=x-rx/2;	cy[0]=y-ry/2;
					cx[1]=x-rx/2;	cy[1]=y+ry/2;
					cx[2]=x+rx/2;	cy[2]=y+ry/2;
					cx[3]=x+rx/2;	cy[3]=y-ry/2;
					for (i=0;i<4;i++)
						{
							icx[i]=(int)cx[i];
							icy[i]=(int)cy[i];
						}
					fill_poly(4,icx,icy,colour2);
				}
		}
  }

void scene1(int colour)
{
	int x;
	int ax[4],ay[4];

ax[0]=1;	ay[0]=1;
ax[1]=1; 	ay[1]=120;
ax[2]=319;	ay[2]=120;
ax[3]=319; 	ay[3]=1;
fill_poly(4,ax,ay,1);

	set_background(colour);
    fill_circle(40,40,20,9);
	fill_circle(35,45,8,1);
	fill_circle(35,45,3,3);
	fill_circle(250,150,30,10);
    fill_circle(250,150,30,10);
	fill_circle(250,180,15,10);
	fill_circle(220,150,15,10);
	fill_circle(235,165,15,10);
    fill_circle(270,175,15,10);
	fill_circle(280,135,15,10);
	for (x=245;x<257;x+=1)

    linedraw(x,120,x,80,6);


}

void avion(int x,int colour)
{
 int ax[4],ay[4],bx[3],by[3],cx[3],cy[3];

ax[0]=10+x;	ay[0]=170;
ax[1]=10+x; ay[1]=180;
ax[2]=50+x;	ay[2]=180;
ax[3]=50+x; ay[3]=170;

bx[0]=20+x;	by[0]=160;
bx[1]=20+x; by[1]=190;
bx[2]=40+x;	by[2]=175;

cx[0]=50+x;	cy[0]=170;
cx[1]=50+x; cy[1]=180;
cx[2]=60+x;	cy[2]=175;
fill_poly(4,ax,ay,colour);
fill_poly(3,bx,by,colour);
fill_poly(3,cx,cy,colour);

}

void enter_data()
{
	float x01,y01,z01,x02,y02,z02;
	float m1,m2;
	float alpha1,alpha2,betha1,betha2,theta;
	float e,v1,v2;

	printf("ENTER M1:");
    scanf("%f\n",&m1);

	printf("ENTER V1:");
    scanf("%f\n",&v1);

	printf("ENTER x01:");
    scanf("%f\n",&x01);

	printf("ENTER y01:");
    scanf("%f\n",&y01);


	printf("ENTER z01:");
    scanf("%f\n",&z01);


	printf("ENTER alpha1:");
    scanf("%f\n",&alpha1);


	printf("ENTER betha1:");
    scanf("%f\n",&betha1);

	printf("ENTER M1:");
    scanf("%f\n",&m1);

	printf("ENTER V1:");
    scanf("%f\n",&v1);

	printf("ENTER x01:");
    scanf("%f\n",&x01);

	printf("ENTER y01:");
    scanf("%f\n",&y01);


	printf("ENTER z01:");
    scanf("%f\n",&z01);


	printf("ENTER alpha1:");
    scanf("%f\n",&alpha1);


	printf("ENTER betha1:");
    scanf("%f\n",&betha1);



	printf("ENTER E:");
    scanf("%f\n",&e);

}


void planar_movement(float x01,float y01,float z01,float x02,float y02,float z02,
					 float m1,float m2,float ra,float rb,
					 float alpha1,float alpha2,float betha1,float betha2,
					 float tmax1,float tmax2,
					 float e,float v1,float v2,
					 int coloura,int colourb,int backcolour)

{
	float xca,yca,zca,xcb,ycb,zcb,theta;
	float vxa,vya,vza,vxb,vyb,vzb;
	float van,vat,vbn,vbt,v11n,v11t,v22n,v22t;
	float xa,xb,ya,yb,za,zb,x11,x22,y11,y22,z11,z22;
    int ti;
    double d;
	vxa=v1*cos(alpha1)*cos(betha1);
	vya=v1*sin(alpha1)*cos(betha1);
	vza=v1*sin(betha1);

	vxb=v2*cos(alpha2)*cos(betha2);
	vyb=v2*sin(alpha2)*cos(betha2);
	vzb=v2*sin(betha2);
	zca=z01;
	zcb=z02;


	for (ti=0;ti<tmax1;ti++)
	{
		xa=x01+vxa*ti;
		ya=y01+vya*ti;
		za=z01+vza*ti;

    	xb=x02+vxb*ti;
		yb=y02+vyb*ti;
		zb=z02+vzb*ti;

		d=sqrt((xa-xb)*(xa-xb)+(ya-yb)*(ya-yb));
    	if (d>(ra+rb))
			{
				fill_circle(xa,ya,ra,coloura);
				fill_circle(xb,yb,rb,colourb);

    		}

		if (d<(ra+rb))
			{
				xca=xa;
				yca=ya;
				xcb=xb;
				ycb=yb;
				theta=acos((ycb-yca)/d);

				van=v1*cos(betha1)*cos(alpha1-theta);
    			vat=v1*cos(betha1)*sin(alpha1-theta);
				vbn=v2*cos(betha2)*cos(alpha2-theta);
				vbt=v2*cos(betha2)*sin(alpha2-theta);

				v11n=van*(m1-e*m2)/(m1+m2)+vbn*m2*(1+e)/(m1+m2);
    			v11t=vat;

				v22n=vbn*(m2-e*m1)/(m1+m2)+van*m1*(1+e)/(m1+m2);
    			v22t=vbt;

				for (ti=1;ti<tmax2;ti++)
					{
					/*	x11=(xca+v11n*ti)*cos(theta)-(yca+v11t*ti)*sin(theta);
    					y11=(xca+v11n*ti)*sin(theta)+(yca+v11t*ti)*cos(theta);
					*/
						x11=xca+(v11n*cos(theta)+v11t*sin(theta))*ti;
						y11=yca+(v11n*sin(theta)+v11t*cos(theta))*ti;
						z11=zca+vza*ti;

					/*	x22=(xcb+v22n*ti)*cos(theta)-(ycb+v22t*ti)*sin(theta);
						y22=(xcb+v22n*ti)*sin(theta)+(ycb+v22t*ti)*cos(theta);
					*/	x22=xcb+(v22n*cos(theta)+v22t*sin(theta))*ti;
						y22=ycb+(v22n*sin(theta)+v22t*cos(theta))*ti;
						z22=zcb+vzb*ti;
						if (x11<ra || x11>(320-ra) || y11<ra || y11>(200-ra))
							break;
						fill_circle(x11,y11,ra,coloura);
						fill_circle(x22,y22,rb,colourb);
						swap_buffers();
						cls(backcolour);

					}
			break;
		}
		swap_buffers();
		cls(backcolour);
   }

}


void planar_movement_3D(float x01,float y01,float z01,float x02,float y02,float z02,
					 float m1,float m2,float ra,float rb,
					 float alpha1,float alpha2,float betha1,float betha2,
					 float tmax1,float tmax2,
					 float e,float v1,float v2,
					 int coloura,int colourb,int backcolour)

{
	float bx[4],by[4],bz[4];
	float xca,yca,zca,xcb,ycb,zcb,theta;
	float vxa,vya,vza,vxb,vyb,vzb;
	float van,vat,vbn,vbt,v11n,v11t,v22n,v22t;
	float xa,xb,ya,yb,za,zb,x11,x22,y11,y22,z11,z22;
    int ti;
	double d;


	bx[0]=0;	bz[0]=0;	by[0]=0;
	bx[1]=0;	bz[1]=0;	by[1]=100;
	bx[2]=120;	bz[2]=0;	by[2]=100;
	bx[3]=120;	bz[3]=0;	by[3]=0;

	vxa=v1*cos(alpha1)*cos(betha1);
	vya=v1*sin(alpha1)*cos(betha1);
	vza=v1*sin(betha1);

	vxb=v2*cos(alpha2)*cos(betha2);
	vyb=v2*sin(alpha2)*cos(betha2);
	vzb=v2*sin(betha2);
	zca=z01;
	zcb=z02;


	for (ti=0;ti<tmax1;ti++)
	{
		xa=x01+vxa*ti;
		ya=y01+vya*ti;
		za=z01+vza*ti;

    	xb=x02+vxb*ti;
		yb=y02+vyb*ti;
		zb=z02+vzb*ti;

		d=sqrt((xa-xb)*(xa-xb)+(ya-yb)*(ya-yb));
    	if (d>(ra+rb))
			{
				scene1(13);
				ENTER_POLYGON(4,bx,bz,by,5);
				ENTER_SPHERE(xa,za,ya,ra,coloura);
				ENTER_SPHERE(xb,zb,yb,rb,colourb);
				make_picture_current();
    		}

		if (d<(ra+rb))
			{
				xca=xa;
				yca=ya;
				xcb=xb;
				ycb=yb;
				theta=acos((ycb-yca)/d);

				van=v1*cos(betha1)*cos(alpha1-theta);
    			vat=v1*cos(betha1)*sin(alpha1-theta);
				vbn=v2*cos(betha2)*cos(alpha2-theta);
				vbt=v2*cos(betha2)*sin(alpha2-theta);

				v11n=van*(m1-e*m2)/(m1+m2)+vbn*m2*(1+e)/(m1+m2);
    			v11t=vat;

				v22n=vbn*(m2-e*m1)/(m1+m2)+van*m1*(1+e)/(m1+m2);
    			v22t=vbt;

				for (ti=1;ti<tmax2;ti++)
					{
					/*	x11=(xca+v11n*ti)*cos(theta)-(yca+v11t*ti)*sin(theta);
    					y11=(xca+v11n*ti)*sin(theta)+(yca+v11t*ti)*cos(theta);
					*/
						x11=xca+(v11n*cos(theta)+v11t*sin(theta))*ti;
						y11=yca+(v11n*sin(theta)+v11t*cos(theta))*ti;
						z11=zca+vza*ti;

					/*	x22=(xcb+v22n*ti)*cos(theta)-(ycb+v22t*ti)*sin(theta);
						y22=(xcb+v22n*ti)*sin(theta)+(ycb+v22t*ti)*cos(theta);
					*/	x22=xcb+(v22n*cos(theta)+v22t*sin(theta))*ti;
						y22=ycb+(v22n*sin(theta)+v22t*cos(theta))*ti;
						z22=zcb+vzb*ti;
						/*if (x11<ra || x11>(320-ra) || y11<ra || y11>(200-ra))
							break;
									   */
						scene1(13);
						ENTER_POLYGON(4,bx,bz,by,5);
						ENTER_SPHERE(x11,z11,y11,ra,coloura);
						ENTER_SPHERE(x22,z22,y22,rb,colourb);
						make_picture_current();
						swap_buffers();
						cls(backcolour);

					}
			break;
		}
		swap_buffers();
		cls(backcolour);
   }

}

