	program TGRAF
	integer idat(2048)
	real*4 xdisp(10240),ydisp(10240)
	real*4 xbox(4),ybox(4)
c
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
	print 11
11	format(' Start increment, ndisp = ')
	read 12,inc,ndisp
12	format(2i8)
c	ndisp=512
c	inc=20
c
c Start Lahey
c	i2=0			!i2=1 faster, but does not work
c	imode=18
c	call PLOTS(0,i2,imode)		!start Lahey graphics
	call PLOTS(0,1,0)		!start Lahey graphics
c Try box
	xbox(1)=0.
	ybox(1)=0.0
	xbox(2)=11.
	ybox(2)=0.0
	xbox(3)=11.
	ybox(3)=0.5
	xbox(4)=0.
	ybox(4)=0.5
	call NEWPEN(1)
	call FILL(4,xbox,ybox)
	call NEWPEN(14)
	call GTEXT(28,1,'1.TEST BOX')
	call GTEXT(28,12,'2.TEST BOX')
	call GTEXT(28,23,'3.TEST BOX')
	call GTEXT(28,34,'4.TEST BOX')
c	call NEWPEN(12)
c	call GTEXT(28,1,'TEST BOX')
c End of box -not very satisfactory as GTEXT has course positioning
c
c Draw 'single channel data' in red
	call NEWPEN(12)
	call PLOT(0.,6.,3)
	call PLOT(3.,6.,2)
	call PLOT(3.,4.,2)
	call PLOT(7.,4.,2)
	call PLOT(7.,6.,2)
	call PLOT(11.,6.,2)
c
c
	dx=11.0/float(ndisp)
	do 1 i=1,ndisp
	xdisp(i)=float(i)*dx
1	continue
	do 2 i=1,10240
	fi=float(i)/50.
	ydisp(i)=2.0*sin(fi)+4.25
2	continue
c
c NB to avoid erasing fixed data trace (in red) might be faster, rather than
c using many calls to GETPIX, to record for each XDISP(), eg in integer array,
c which pixels are set in the data trace, and then skip both erase and write
c at these elements -but need Y pixel too.  Needs thought!
c TIMING:
c (1) about 12ms/512 points to write and erase
c (2) 26ms/512 with 2 calls to GETPIX per 512 points
c (3) idat() method = 19ms/512 points (1 GETPIX per cycle)
	n=1
	jfst=1
	jlast=ndisp
4	continue
	i=0
	do 3 j=jfst,jlast
	i=i+1
	if(jfst.gt.1) then
	   j1=j-inc
c	   call GETPIX(xdisp(i),ydisp(j1),ic)
c	   if(ic.ne.12) call SETPIX(xdisp(i),ydisp(j1),0)	!erase old
	   if(idat(i).ne.0) call SETPIX(xdisp(i),ydisp(j1),0)	!erase old
c=	   call SETPIX(xdisp(i),ydisp(j1),0)	!erase old
	endif
	call GETPIX(xdisp(i),ydisp(j),ic)
c	if(ic.ne.12) call SETPIX(xdisp(i),ydisp(j),11)	!draw new
	if(ic.ne.12) then
	   call SETPIX(xdisp(i),ydisp(j),11)	!draw new
	   idat(i)=1
	else
	   idat(i)=0	!already red -don't draw
	endif
3	continue
	i=0
c	do 31 j=jfst,jlast
c	i=i+1
c	call SETPIX(xdisp(i),ydisp(j),0)	!erase
31	continue
	n=n+1
	jfst=jfst+inc
	jlast=jfst+ndisp-1
	if(jlast.le.10240) goto 4
	pause
	call PLOT(0.,0.,999)
	print 10,n
10	format(' Number of displays = ',i8)
	end

