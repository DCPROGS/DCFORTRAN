	subroutine IDCPIX3(ix,iy,n,iact,icol,imode,setcol)
	integer*2 ix(n),iy(n)
	integer*1 icol(n)
	integer iact(0:15)
	logical setcol
c
c Displays ix(i),iy(i) ,i=1,...,n (pixel units)
c
c IDCPIX is version of DCPIX with integer*2 data (VGA scale) using Ioana's
c routines WPIXEL,RPIXEL in place of SETPIX,GETPIX
c
c IDCPIX1 is version in which array ICOL is made a parameter (and
c	also made integer*1 to save space) so
c  (1) it can be allocated to correct size before calling IDCPIX1
c  (2) after drawing points, icol() is set to the colour of each point
c	so ready to use at next call, but this unlikely to be useful because
c	the points drawn at next call will be different; we want icol() to
c	contain the colours of each pixel to be drawn at the present call
c	so normally (imode=0) have to look at each point with RPIXEL as
c	before. If imode=1 this is omitted and icol() defined by last
c	call is used.
c
c IDCPIX2 is version without imode argument (always like imode=1)
c
c IDCPIX3 03/06/97 05:30pm
c has extra argument, setcol=.false. if icol() is to be unchanged by call,
c and isetcol=.true, if to function as before (icol(i) changed to colour in
c which x(i),y(i) is actually drawn (but now cannot set icol()=-1)
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
	if(imode.eq.1) goto 3		!use input icol()
	do 1 i=1,n
	   if((ix(i).lt.0).or.(ix(i).gt.639)) goto 1
	   call RPIXEL(ix(i),iy(i),ic)			!get present colour (=#ic)
	   icol(i)=ic
1	continue
c Then draw, according to what original colours were
c Modif 03/06/97 02:25pm so that icol() never set to -1
3	continue
	do 2 i=1,n
	   if((ix(i).lt.0).or.(ix(i).gt.639)) goto 2
	   ic=int4(icol(i))
	   ic1=iact(ic)
	   if(ic1.ne.-1) then
		call WPIXEL(ix(i),iy(i),ic1)		!set to colour #iact(ic)
	      if(setcol) icol(i)=ic1		!record for output
	   endif
2	continue
	RETURN
	end

