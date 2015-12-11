


/*************************************************************************


		TITLE	:RIGID BODY MOTION
		AUTHOR	:IOAN LUISA VAIS
		COURSE	:MSC IN COMPUTER GRAPHICS
		MODULE	:MIKELIB.H
		DATE	:01.05.1992

***************************************************************************


	CONTAINS:DECLARATIONS OF MIKE DAY PRIMITIVE DRAWING FUNCTIONS


***************************************************************************/

extern void double_buffer(int truth_value);
extern void cls(int colour);
extern void linedraw(int x0, int y0, int x1, int y1, int colour);
extern void fill_circle(int xc, int yc, int radius, int colour);
extern void fill_poly(int n, int *ax, int *ay, int colour);
extern void swap_buffers(void);

/*=========================================================================*/


		/*END OF MODULE MIKELIB.H*/


/*=========================================================================*/
