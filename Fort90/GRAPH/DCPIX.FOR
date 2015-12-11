	subroutine DCPIX(x,y,n,iact)
	real*4 x(n),y(n)
	integer iact(0:15)
	integer icol(2048)	!for solution #1 to prblem described below
c Displays x(i),y(i) ,i=1,...,n
c
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c [NB could also have simpler version (as LGRFDC) call DCPIX(x,y,n,ipen,ic)
c	 which draws a pixel, in colour #ipen, ONLY if that pixel already has
c	colour #ic so call DCPIX(x,y,n,14,0) will draw yellow points on blank
c	screen (col=0) only, and so will not overwrite any existing points.  This
c	trace could then be erased selectively by call DCPIX(x,y,n,0,14) which
c	erases ONLY yellow pixels]
c
c THIS VERSION -more general
c If the pixel to be set (or erased if colour=background) is already set
c to colour #ic (ic=0,...,15) then the action taken is as follows
c   if iact(ic)=ic1 (=0,...,15) -then the pixel is drawn in colour #ic1 (rather
c					    than colour #ipen)
c   if iact(ic)=-1              -then the pixel is not drawn at all
c
c Thus to draw a red (#12) trace on blank screen call DCPIX(x,y,n,iact)
c	with iact(0)=12.
c To draw regardless of what is on screen call with all iact(0,...,15)=12
c To superimpose a light blue (#11) trace without overwriting any red points
c	then call DCPIX(x1,y1,n1,iact) with iact(12)=-1, all other iact(i)=11
c Or, to avoid overwriting ANYTHING that is on screen already, ie write new
c	pixel ONLY if old one has colour=0, then call DCPIX(x1,y1,n1,iact)
c	with iact(0)=11 and all other iact(1,...,15)=-1
c To draw the light blue (#11) trace on top of the red (#12) one, but change
c	all pixels that are common to both to purple (#13) then
c	call DCPIX(x1,y1,n1,iact) with iact(12)=13 (change red to purple) and
c	iact(0)=11 (draw blue if pixel not set)
c To ERASE the light blue (#11) trace only call DCPIX(x1,y1,n1,iact) with
c	iact(11)=0, and all other iact(i)=-1
c To ERASE both light blue and purple points call DCPIX(x1,y1,n1,iact)
c	with iact(11)=0, iact(13)=0, and all other iact(i)=-1
c
c N.B. There will be a PROBLEM with some of these options when successive x,y
c values are close enough that both point to the same pixel (e.g. when
c displaying 2048 points in VGA).  E.g. if the loop with x(1),y(1) changes
c changes the colour of the pixel, then on next cycle when drawing x(2),y(2),
c which address the same pixel, the colour of this pixel reported by GETPIX is
c the changed value, not the expected original value! The following code
c suffers from this problem.
c  SOLUTIONS:
c (1) Could go through all the data and record colour of each pixel,
c   e.g. in integer array, without any SETPIX calls, then go through again
c   to do all the SETPIX calls
c OR
c (2) a better solution, in terms of both storage (not generally a problem
c   with Lahey 5.0!), and perhaps speed too, would be to check the actual pixel
c   coordinates each time, and if they are the same as those for the pixel
c   addressed on the previous cycle round the loop then do nothing (this pixel
c   has already been set properly).  This solution is not so easy in Fortran,
c   because the pixel coordinates for a given x,y are not easily available. The
c   manuals do not seem to state precisely how x,y coordinates are converted
c   to pixel coordinates!
c
c SIMPLE CODE (has above problem)
c	do 1 i=1,n
c	   call GETPIX(x(i),y(i),ic)			!get present colour (=#ic)
c	   ic1=iact(ic)
c	   if(ic1.ne.-1) then
c		call SETPIX(x(i),y(i),ic1)		!set to colour #iact(ic)
c	   endif
c1	continue
c
c Solution number 1 for the problem (the hamfisted method)
c NB GETPIX returns rubbish for ic if x is out of range (0-11)
c   First record original pixel colours in icol()
	do 1 i=1,n
	   if((x(i).lt.0.).or.(x(i).gt.11.)) goto 1
	   call GETPIX(x(i),y(i),ic)			!get present colour (=#ic)
	   icol(i)=ic
1	continue
c Then draw, according to what original colours were
	do 2 i=1,n
	   if((x(i).lt.0.).or.(x(i).gt.11.)) goto 2
	   ic1=iact(icol(i))
	   if(ic1.ne.-1) then
		call SETPIX(x(i),y(i),ic1)		!set to colour #iact(ic)
	   endif
2	continue
	RETURN
	end

