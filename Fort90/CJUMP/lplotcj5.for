	subroutine LPLOTCJ5(naver,navc,iADC,avcur,avcurcon,nsamp,nAc,dnAc,
     & title1,ijd,itsamp,nAv,dnAv,dA,ncjump,ilenc,igapc,vjump,control,
     & nvjump,ilenv,igapv,record,kmax,abort,mkeep,ikeep1,kstep,
     & jmask,nsamp1,recscn)
c
c LPLOTCJ4.for has kmax2 removed -all arrays  now kmax in size
c
c Modif 07/15/91 03:55pm so recscn is param (so message put up when 'record
c	screen' used (when RECORD=false)
c
c Modif 07/12/91 11:58am so dimension of AVCUR now kmax2, and YDISP, YAV,
c Xdisp now 20481 (if memory, or speed problems display only every nth point)
c
c 03/17/91 12:44pm ilenc etc now in integer*4 microsec
c=====skip scale after 1st time, and call PLOTS()!
c Modified 11/03/90 06:11pm to accept dnAv=number of microseconds following
c iADC(nAc) at which C-jump starts ( and dnAv similarly for V-jump).
c
c Plots current vs time; current sweep (green), and, if naver (or navc) >1
c then also plots average (red). Plots AVCUR for average unless CONTROL=true
c in which case current average of controls, AVCURCON, is plotted
c GRAPHL routines (in GRAPHL.LIB = Lahey GRAPH3.LIB except
c that all routines renamed with 'L' on end of name)- this does NOT work
c- still have got 'plot' internally in GRAPHL, so homemade LGRFDC.LIB
c (from LGRFDC.FOR), which uses only wpixel)
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	ALLOCATABLE::xdisp,ydisp,yav
	integer*2 iADC(kmax)
	real*4 avcur(kmax),avcurcon(kmax)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*4 ikeep1(4,2)
      integer*2 kstep(5)
      integer*1 jmask(kmax)
	real*4 xdisp(:),ydisp(:),yav(:)  !may need nsamp+1
	character title1*79,cijd*11
	logical record,vjump,control,abort,recscn
	logical debug,caplock,mono
	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
	common/lgrf/xpos,ypos,ipen		!ditto
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c
	debug()=caplock()

	ALLOCATE(xdisp(nsamp+1),ydisp(nsamp1+1),yav(nsamp1+1))
	call INITLGRF
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 units/volt
c See TLAHGRF and library (LGRFDC.FOR) for details re scaling
	xmin=0.0
	xmax=float(itsamp)/1000.		!msec
	idx=int(xmax/10.)
	dx=(xmax-xmin)/float(nsamp)
c Make ymin smaller than any actual data, to leave margin at bottom
c for the Drawbox boxes, and narrower margin at top for messages
	ymin=-6.0		!volts
	ymax=+5.5		!volts
	acal=6553.6	!adc units/volt
c use mouse or pots or keys to move cursor?

	call gSCALE(xmin,xmax,ymin,ymax,is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)

	ipen=9
	call lincols(ipen,0)
	call gramov(xmin,0.)
	call gralin(xmax,0.)     !blue line at zero current
	call gramov(xmin,ymin)
	call gralin(xmin,ymax)     !blue line at zero current
	y=-6
	do i=1,12
	   call gramov(xmin,y)
	   call gralin(0.005*xmax,y)     !blue line at zero current
	   y=y+1
	enddo
	do i=1,idx
	   call gramov(10.*float(i),0.1)
	   call gralin(10.*float(i),-0.1)     !blue line at zero current
	enddo
c
	if(abort) then
	   call BELL(2)
	   call NEWPEN(13)		!for wstring18 call- purple
	   call wrstring18(160,432,'            ',0,0)
	   call wrstring18(160,432,'JUMP ABORTED',13,0)
	   abort=.false.		!reset
	   if(naver.eq.0.and.navc.eq.0) goto 221	!skip data display
	endif
c
c Make array of points already scaled to screen units
c in xdisp,ydisp
c set arrays
	sy1=sy/acal		!so ADC0 scale is volts in CED
	n=naver
	if(control) n=navc

c
	j=0
	do 10 i=1,nsamp
	if(jmask(i).eq.0) goto 10	!skipped point
	x=float(i-1)*dx		!=msec from 0 to itsamp-1
	j=j+1
	xdisp(j)=x*sx + xoff
10	continue
c
	do 11 i=1,nsamp1
	ydisp(i)=float(iADC(i))*sy1 + yoff
	if(n.gt.1) then
	   if(control) then
		yav(i)=avcurcon(i)*sy1 + yoff
	   else
		yav(i)=avcur(i)*sy1 + yoff
	   endif
	endif
11	continue
	xdisp(nsamp1+1)=xdisp(nsamp1)+dx !need to draw bar if V jump goes to end of sample
c draw data
c	call IMOVEL(0,iADC(1))	!not needed before wpixel
	if(n.eq.0) goto 221	!nothing to plot
c	call timer(it1)
	if(n.gt.0) then
	   do 2 i=1,nsamp1
	   call grapix(xdisp(i),ydisp(i),ix,iy)
	   call wpixel(ix,iy,10)	!current sweep green
2	   continue
	endif
	if(n.gt.1) then
	   do 23 i=1,nsamp1
	   call grapix(xdisp(i),yav(i),ix,iy)
	   call wpixel(ix,iy,12)     !average in red
23	   continue
	endif
221	continue
	if(ncjump.eq.0) goto 22
c Draw horizontal bars to mark position of jumps
c First C-jump starts at point iDACc(nDc) which coincides with iADC(nAc)
c (plus dnAc microsec), and ends at point ilen later where ilen=pulse length
c expressed in terms of DAC or ADC units
	ipen=15		!white
	call lincols(ipen,0)
c	j1=nAc
c	itcum=0		!cumulative time from start of 1st pulse
	t1=(float(nAc-1)*dA + dnAc)/1000. !msec from trigger to start of 1st pulse
	ylev=ymin+0.95*(ymax-ymin)	!y level to draw bar in world units
	do 20 i=1,ncjump
	  t2=t1+float(ilenc(i))*1.e-3		!msec from trigger to end of pulse
	  if(t2.lt.xmin) goto 20
	  if(t1.gt.xmax) goto 22		!finished
	  if(t1.ge.xmin.and.t2.le.xmax) then
	     call graMOV(t1,ylev)
	     call gralin(t2,ylev)
	  else if(t1.lt.xmin.and.t2.le.xmax) then
	     call graMOV(xmin,ylev)
	     call graLin(t2,ylev)
	  else if(t1.ge.xmin.and.t2.gt.xmax) then
	     call graMOV(t1,ylev)
	     call gralin(xmax,ylev)
	  endif
	  t1=t2 + float(igapc(i))*1.e-3	!time for start of next pulse in msec
20	continue
c Now bars  for vjumps/ramps, if any (bit lower, and yellow)
22	continue
	if(vjump) then
	  ipen=14           !yellow
	call lincols(ipen,0)
	  t1=(float(nAv-1)*dA + dnAv)/1000. !msec from trigger to start of 1st pulse
	  ylev=ymin+0.93*(ymax-ymin)	!y level to draw bar in world units
	  do 21 i=1,nvjump
	   t2=t1+float(ilenv(i))*1.e-3		!msec from trigger to end of pulse
	   if(t2.lt.xmin) goto 21
	   if(t1.gt.xmax) goto 203		!finished
	   if(t1.ge.xmin.and.t2.le.xmax) then
	     call graMOv(t1,ylev)
	     call graLin(t2,ylev)
	   else if(t1.lt.xmin.and.t2.le.xmax) then
	     call graMOV(xmin,ylev)
	     call gralin(t2,ylev)
	   else if(t1.ge.xmin.and.t2.gt.xmax) then
	     call graMOV(t1,ylev)
	     call gralin(xmax,ylev)
	   endif
	   t1=t2 + float(igapv(i))*1.e-3	!time for start of next pulse in msec
21	  continue
	endif
203	continue
c Return, still in graphics mode
	call NEWPEN(11)		!for wrstring18 call- white
	call wrstring18(16,464,title1,11,0)
	if(record) then
	   call NEWPEN(12)
	   call wrstring18(552,464,'RECORDING',12,0)
	   call INTCONV(ijd,cijd)
	   call wrstring18(552,448,'Sweep #'//charnb(cijd),12,0)
	else
	   call NEWPEN(14)
	   call wrstring18(552,464,'NO RECORD',14,0)
	   if(recscn) then	!last screen recorded
		recscn=.false.
		call INTCONV(ijd,cijd)
		call NEWPEN(12)
		call wrstring18(552,448,'Swp #'//charnb(cijd)//' recorded',
     &	12,0)
	   endif
	endif
	if(vjump) then
	   if(control) then
		call NEWPEN(13)
		call wrstring18(16,448,'CONTROL  ',13,0)
	   else
		call NEWPEN(10)
		call wrstring18(16,448,'AGONIST  ',10,0)
c		call wrstring18(16,448,'C-JUMP',10,0)
	   endif
	endif
c
	deALLOCATE(xdisp,ydisp,yav)
	RETURN
	END



