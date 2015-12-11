	program TGRAF2
	integer iact(0:15)
	real*4 xdisp1(2048),ydisp1(2048)
	real*4 xdisp2(2048),ydisp2(2048)
	real*4 xdisp3(2048),ydisp3(2048)
c	integer iarray(9)		!for GRINFO
c	real*4 rarray(7)
c	character*40 string
c
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c
c Define 'single channel' data #1 in form of x,y arrays with ndisp points
	ipen1=12		!red
	ndisp1=1024
	dx1=11.0/float(ndisp1)
	do 1 i=1,ndisp1
	xdisp1(i)=float(i)*dx1
	if(xdisp1(i).lt.3.) ydisp1(i)=6.
	if(xdisp1(i).ge.3..and.xdisp1(i).lt.7.) ydisp1(i)=4.
	if(xdisp1(i).ge.7.) ydisp1(i)=6.
1	continue
c
c Define 'single channel' data #2 in form of x,y arrays with ndisp points
	ipen2=14		!yellow
	ndisp2=1024
	dx2=11.0/float(ndisp2)
	do 2 i=1,ndisp2
	xdisp2(i)=float(i)*dx2
	if(xdisp2(i).lt.4.) ydisp2(i)=6.
	if(xdisp2(i).ge.4..and.xdisp2(i).lt.8.) ydisp2(i)=4.
	if(xdisp2(i).ge.8.) ydisp2(i)=6.
2	continue
c
cc Array #3=sine wave
c	ipen3=11		!light blue
c	ndisp3=740
c	dx3=11.0/float(ndisp3)
c	do 3 i=1,ndisp3
c	xdisp3(i)=float(i)*dx3
c	fi=float(i)/50.
c	ydisp3(i)=2.0*sin(fi)+4.25
c3	continue
c
c Define array 3 = square wave displaced again
	ipen3=11		!light blue
	ndisp3=1024
	dx3=11.0/float(ndisp3)
	do 3 i=1,ndisp3
	xdisp3(i)=float(i)*dx3
	if(xdisp3(i).lt.5.) ydisp3(i)=6.
	if(xdisp3(i).ge.5..and.xdisp3(i).lt.9.) ydisp3(i)=4.
	if(xdisp3(i).ge.9.) ydisp3(i)=6.
3	continue
c
c Start Lahey
c	i2=0			!i2=1 faster, but does not work
c	imode=18
c	call PLOTS(0,i2,imode)		!start Lahey graphics
	call PLOTS(0,1,0)		!start Lahey graphics
c
	call SETACT(iact,ipen1)		!draw in col ipen1 whatever
	call DCPIX(xdisp1,ydisp1,ndisp1,iact)
	pause
c
	call SETACT(iact,ipen2)	!draw in col ipen2=yellow
	iact(ipen1)=13		!unless already ipen1=red in which case purple
	call DCPIX(xdisp2,ydisp2,ndisp2,iact)
	pause
c
c	call GRINFO(iarray,rarray,string)
c	do 30 i=1,9
c	  print 31,i,iarray(i)
c31	  format(' iarray(',i2,') = ',i8)
c30	continue
c	do 32 i=1,7
c	  print 33,i,rarray(i)
c33	  format(' rarray(',i2,') = ',g13.6)
c32	continue
c	print 34,string
c34	format(a40)
c	pause
c
	call SETACT(iact,-1)		!do not draw
	iact(0)=ipen3			!unless pixel not set yet
	call DCPIX(xdisp3,ydisp3,ndisp3,iact)
	pause
c
c Now try selective erasing
c Erase trace #2 whether colour is ipen2=14=yellow, or purple (13)
	call SETACT(iact,-1)		!do not erase
	iact(ipen2)=0			!unless ipen2=yellow
	iact(13)=0				!or purple
	call DCPIX(xdisp2,ydisp2,ndisp2,iact)
	pause
c
	call PLOT(0.,0.,999)
c
	end

