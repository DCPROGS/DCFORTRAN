	program TGRAF1
	integer idat(2048)
	real*4 xdisp(2048),ydisp(10240)
	logical first
c
c TGRAF1=version of TGRAF that uses DCPIX1
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	print 11
11	format(' Start increment, ndisp = ')
	read 12,inc,ndisp
12	format(2i8)
c
c Start Lahey
	call PLOTS(0,1,0)		!start Lahey graphics
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
c NB for scrolling displays could make special version that uses integer
c array to avoid one GETPIX call per point
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
	ndim=10240		!dimension of the long array
	ipen=11
	first=.true.
	n=1
	jfst=1
4	continue
	 call DCPIX1(xdisp,ydisp,ndim,ndisp,jfst,inc,ipen,idat,first)
	 n=n+1
	 jfst=jfst+inc
	 jlast=jfst+ndisp-1
	 if(jlast.le.10240) goto 4
c
	pause
	call PLOT(0.,0.,999)
	print 10,n
10	format(' Number of displays = ',i8)
	end


