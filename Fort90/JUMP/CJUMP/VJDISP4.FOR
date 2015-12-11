	subroutine VJDISP4(avcur,avcurcon,avolt,avoltcon,nsamp,nvjump,
     & naver,navc,ilenv,igapv,ivolt1,ivolt2,ivhold,calfac,calvolt,
     & sampv,nAv,dnAv,dA,title1,itsamp,kmax,jmask,nsamp1,keepall,
     & vjump,ncjump,nAc,dnAc,ilenc,igapc,ibox,icwd,icqd)
c
c VJDISP4 -all arrays=kmax now
c Modif 07/12/91 11:58am so dimension of AVCUR now kmax2, and YDISP,
c Xdisp now 20480 (if memory, or speed problems display only every nth point)
c
c
c Version of VJDISP for CJUMP3 (===NB section that chooses data point # that
c 	corresponds to start and end of a jump not yet fixed)
c
c To plot
c (1) drug and control v-jump or ramp
c (2)net voltage jump (I vs t) or net voltage ramp, I vs V (in ADC1)
c (3) V vs t only
c
c	At present current scale (Y axis)=full scale of 1401 for both
c V-jump and I/V
c
c This version uses present values of AVCUR,ACURCON etc from main prog
c (VJDISPRD can read earlier values from disk)
c
c	Calc net curve from the present mean 'drug' current minus mean
c 'control'. This can always be done even if not 'recording'; need this
c to set up jumps before recording switched on.
c	 At present the current average is read in, so, to get single sweeps,
c do 1 control, 1 drug then display; and then start new average (so AVCUR
c is always average of naver=1)
c	Note that AVOLT=voltages for drug, and AVOLTCON=voltages for control,
c should be same so average them, and plot each current value
c against the same potential (otherwise subtraction of control from drug
c would be a problem)
c
c	real*4 avcur(2*kmax),avcurcon(2*kmax)	!average current (drug,control)
c	real*4 xdisp(2048),ydisp(3,2048)
c	real*4 xdisp(20480),ydisp(3,20480)

	integer*1 	jmask(kmax)
	integer*2 	videotyp
	integer*2   ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*4 	ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*4 	ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer   	ifst(10),ilast(10)
	real*4    	avcur(kmax),avcurcon(kmax)		!average current (drug,control)
	real*4    	avolt(kmax),avoltcon(kmax)	!average voltage (drug,control)
	real*4    	xdisp(60000),ydisp(3,60000)
	integer   	itype(10)
	character 	title1*79,def*30,cans*30,getint*10,getreal*13
c	character 	ci*11,cx*20,cy*20	!for debug
	character 	ccal*11,ccal1*11,unit*4,title*15,charout
      character*1	ans,getch	!for debug
	character*72 que(10),helps(5)
	logical   	discprt
	logical 	sampv,onejump,keepall,vjump,alpha
	logical 	deb			!for debug
	logical 	debug,caplock
	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
	common/lgrf/xpos,ypos,ipen		!ditto
	common/dp/discprt
c
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	debug()=caplock()
	alpha=videotyp().eq.3
	ic=15
	icup=15
	icf=8
	ibkw=4
	ixlow=200
	iylow=-1
	iyhiw=400
	title='OPTIONS'
c
	if(naver.eq.0.and.navc.eq.0) then
	   call BELL(1)
	   if(alpha) then
	      print 170
170	      format(' NO DATA TO PLOT YET')
		pause
	   else
		call wdialog(ibox,' NO DATA TO PLOT YET',icwd)
		ans=getch(b)
	   endif
	   RETURN
	endif
	inchar=11
4	format(i8)
101	format(a1)
	if(debug()) then
32	   print 33,itsamp/1000,nsamp1
33	   format(' Sample length ',i7, ' ms; # of points = ',i8,/,
     &	' Type data(i1) to data(i2) [0 to end]; i1,i2 = ')
	   read 31,i1,i2
31	   format(2i8)
	   if(i1.le.0) goto 99
	   do 40 i=i1,i2
	   if(.not.sampv) then
	      print 53,i,avcur(i),avcurcon(i)
	      if(discprt) write(8,53) i,avcur(i),avcurcon(i)
53	      format(1x,i8,2g13.6)
	   else
	      print 51,i,avcur(i),avcurcon(i),avolt(i),avoltcon(i)
	      if(discprt)write(8,51) i,avcur(i),avcurcon(i),
     &	avolt(i),avoltcon(i)
51	      format(1x,i8,4g13.6)
	   endif
40	   continue
	   goto 32
99	   continue
	endif   	!end of debug
c
c	ymin=-6.0		!volts
c	ymax=+5.5		!volts
	acal=6553.6		!adc units/volt
c	curcal=10.0*calfac*acal		!pA per 10 volts
c
c
	call wdialog(ibox,' PLOTING DATA ',icwd)

	iplot=1
111	continue
	if(sampv) then
	   if(naver.ge.1.and.navc.ge.1) then
	      if(alpha) then
	   	   print 1,iplot
1	   	   format(
     & 	   ' (1) UNSUBTRACTED jumps or ramps',/,
     & 	   ' (2) NET jump or ramp',/,
     & 	   ' (3) Voltage vs time',/,
     & 	   ' (4) No more plots ',/,
     & 	   ' Option number [',i2,'] = ')
	   	   read 4,i
	      else
	     	   que(1)='(1) UNSUBTRACTED jumps or ramps'
	     	   que(2)='(2) NET jump or ramp'
	     	   que(3)='(3) Voltage vs time'
	     	   que(4)='(4) No more plots '
	         j=1
	   	   nq=4
	         nhelp=1
	         call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &         title,helps,nhelp,j,charout,ival)
		   i=j
	  	endif
	   else if(naver.ge.1.and.navc.eq.0) then
	      if(alpha) then
	   	   print 22,iplot
22	         format(
     & 	   ' (1) AGONIST jump or ramp',/,
     &         ' (3) Voltage vs time',/,
     &         ' (4) No more plots ',/,
     & 	   ' Option number [',i2,'] = ')
	         read 4,i
	  	else
	         que(1)='(1) AGONIST jump or ramp'
	         que(2)='(2) Voltage vs time'
	         que(3)='(3) No more plots '
	         j=1
	         nq=3
	         nhelp=1
	         call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &         title,helps,nhelp,j,charout,ival)
		   i=j
	         if(j.eq.2) i=3
	         if(j.eq.3) i=4
	      endif
	   else if(naver.eq.0.and.navc.ge.1) then
	   	if(alpha) then
	   	   print 23,iplot
23	   	   format(
     & 	   ' (1) CONTROL jump or ramp',/,
     & 	   ' (3) Voltage vs time',/,
     & 	   ' (4) No more plots ',/,
     & 	   ' Option number [',i2,'] = ')
		   read 4,i
	  	else
		   que(1)='(1) CONTROL jump or ramp'
		   que(2)='(2) Voltage vs time'
		   que(3)='(3) No more plots '
		   j=1
	   	   nq=3
	         nhelp=1
	         call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &   	   title,helps,nhelp,j,charout,ival)
		   i=j
	         if(j.eq.2) i=3
	         if(j.eq.3) i=4
	  	endif
	   endif
	else if(.not.sampv) then
	   if(naver.ge.1.and.navc.ge.1) then
	   	if(alpha) then
	         print 24,iplot
24	         format(
     &         ' (1) UNSUBTRACTED jumps',
     &         ' (2) NET jump',
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
		   read 4,i
	  	else
		   que(1)='(1) UNSUBTRACTED jumps'
		   que(2)='(2) NET jump'
		   que(3)='(3) No more plots '
		   j=1
	   	   nq=3
	   	   nhelp=1
	   	   call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &   	   title,helps,nhelp,j,charout,ival)
		   i=j
	         if(j.eq.3) i=4
	  	endif
	   else if(naver.ge.1.and.navc.eq.0) then
	   	if(alpha) then
		     print 25,iplot
25	           format(
     &         ' (1) AGONIST jump',/,
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
		     read 4,i
	  	else
	         que(1)='(1) AGONIST jump'
	         que(2)='(2) No more plots'
	         j=1
	         nq=2
	         nhelp=1
	         call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &         title,helps,nhelp,j,charout,ival)
		   i=j
	         if(j.eq.2) i=4
	  	endif
	   else if(naver.eq.0.and.navc.ge.1) then
	    	if(alpha) then
	         print 7,iplot
7	         format(
     & 	   ' (1) CONTROL jump',/,
     & 	   ' (4) No more plots ',/,
     & 	   ' Option number [',i2,'] = ')
	   	   read 4,i
	      else
	         que(1)='(1) CONTROL jump'
	         que(2)='(4) No more plots'
	         j=1
	         nq=2
	         nhelp=1
	         call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &         title,helps,nhelp,j,charout,ival)
		   i=j
	         if(j.eq.2) i=4
	  	endif
	   endif
	endif

	if(i.gt.0) iplot=i      !otherwise default
	if(iplot.eq.4) RETURN
c
	if(iplot.eq.2.and.(naver.eq.0.or.navc.eq.0)) goto 111		!no net curve!
	if(iplot.eq.3.and.(.not.sampv)) goto 111		!no voltage!
	if(iplot.eq.1.or.iplot.eq.2) goto 90
	if(iplot.eq.3) goto 190		!V(t) vs t section
	if(iplot.gt.4) goto 111
c
90	continue
c Check how many jumps/ramps in the sweep (not defined if nvjump=0)
	itype(1)=0		!needed below if nvjump=0
	if(nvjump.ge.1) then
	 do 10 i=1,nvjump
	   if(ivolt1(i).eq.ivolt2(i)) then
		itype(i)=0	!ith pulse is a jump
	   else
		itype(i)=1	!ith pulse is a ramp
	   endif
10	 continue
	endif
c
c  Calculate which points from the entire sweep are to be plotted, in
c terms of index in iADC (following calc was used for drawing horizontal bars
c in LPLOTCJ, and below, but not used now to allow for fact that 1st point
c of jump may not be exactly coincident with an ADC sample; however this
c calc can still be used here -it will find ADC points at start and end of
c jump with max error of 1 point which is quite good enough for choosing
c points to be displayed).
c MODIF: 06/06/91 04:15pm so that if nvjump=0 or nvjump=1 and it is a jump
c (not a ramp) then whole sample
c is plotted
	ipuls=1				!in case jump to 169
	xmin=0.0				!ditto
	xmax=float(itsamp)/1000.	!msec -ditto
	dx=(xmax-xmin)/float(nsamp)	!ditto
	onejump=(nvjump.eq.1.and.ivolt1(1).eq.ivolt2(1))
	if(nvjump.eq.0.or.onejump) then
	   i1=1		!first point to be plotted
	   i2=nsamp1	!last point to be plotted
	   goto 169       !calc nplot=nsamp1
	endif
c
	j1=nAv
	itcum=0		!cumulative time from start of 1st pulse (int*2) (musec)
	do 21 i=1,nvjump
	  itcum=itcum+ilenv(i)
	  il=ifixr(float(itcum)/dA)   !# of elements of iADC corresp to ilen
	  j2=nAv+il		! 1st pulse is at iADC(nAv)
	  ifst(i)=j1	!index in iADC, avcur of 1st point of ith pulse
	  ilast(i)=j2	!index in iADC, avcur of last point of ith pulse
	  itcum=itcum+igapv(i)
	  ig=ifixr(float(itcum)/dA)   !# of elements of iADC corresp to igap
	  j1=nAv+ig		! 1st V-jump is at iADC(nAv)
	  if(debug()) then
		if(discprt) write(8,52) i,ifst(i),ilast(i)
52		format(' i,ifst(i),ilast(i) = ',3i8)
		pause
	  endif
21	continue
c
c
c====return here to plot diff pulse?
c
	ipuls=1
	if(nvjump.gt.1) then
	   do 11 i=1,nvjump
	      if(itype(i).eq.0) then
		   if(alpha) then
		      print 12,i
12		      format(' Pulse number ',i2,' is a jump')
		   else
		      call wdialog(ibox,' Pulse number '//getint(i,nm)(1:nm)
     &	      //' is a jump',icwd)
		   endif
	   	else
		   if(alpha) then
		      print 13,i
13		      format(' Pulse number ',i2,' is a ramp')
		   else
		      call wdialog(ibox,' Pulse number '//getint(i,nm)(1:nm)
     &	      //' is a ramp',icwd)
		   endif
	      endif
11	   continue
141	   if(alpha) then
	      print 14
14	      format(' Plot pulse number = ')
	      read 4,ipuls
	   else
		call defolti(ipuls,def)
		call qdialog(ibox,' Plot pulse number',def,icqd,cans)
		call getinpi(cans,ipuls)
	   endif
	   if(ipuls.lt.1.or.ipuls.gt.nvjump) goto 141
	endif
c Now plot pulse # ipuls.
c For a ramp, plot from first point to last
c For V-jump start plot before ifst() to get baseline, and, to get
c off-jump plot equal length after ilast()
c Scale X in msec for jumps, the x=0 at time of on-jump
c Scale X in mV for ramps
	if(itype(ipuls).eq.0) then	!jump
	   jlen=ilast(ipuls)-ifst(ipuls)	!or +1 ?======
	   ipre=jlen/3
	   i1=ifst(ipuls)-ipre	!start early for baseline
c next bit is as in VJDSPRD3 (orig commented out below)
c Not clear whether to use nsamp or nsamp1 but this part not yet
c fixed if they differ!
	   if(i1.ge.1) then
	      xmin=-float(ipre)*dA/1000.			!msec
	   else if(i1.lt.1) then
		i1=1
		xmin=-float(iTPREVv)*0.001 !t to 1st Vjump (ms) so it starts at x=0
	   endif
	   i2=ilast(ipuls)+jlen
	   if(i2.le.nsamp) then
	      xmax=2.0*float(ilenv(ipuls))*1.e-3	!msec
	   else if(i2.gt.nsamp) then
		i2=nsamp
		xmax=float(itsamp-iTPREv)*0.001	!msec
	   endif
	else
	   i1=ifst(ipuls) 	!for ramp
	   i2=ilast(ipuls)
	   xmin=float(ivolt1(ipuls))
	   xmax=float(ivolt2(ipuls))
	   if(xmax.lt.xmin) then
		x=xmax
		xmax=xmin
		xmin=x
	   endif
	endif
	dx=(xmax-xmin)/float(nplot-1)	!only if keepall=true
c=======need to fix dx if keepall=false
169	continue	!jump here if nvjump=0 or 1 jump (not ramp)
	nplot=i2-i1+1
c
c Make ymin smaller than any actual data, to leave margin at bottom
c for the Drawbox boxes, and narrower margin at top for messages -do
c this in call to SCALEL
c Scale the y-axis to fill screen well
	ymin1=1.e37
	ymax1=-1.e37
	ymin2=1.e37
	ymax2=-1.e37
	ymin3=1.e37
	ymax3=-1.e37
	do 150 i=1,nplot
	   j=i1+i-1					!=i1,i1+1,...
	   y1=avcur(j)
	   y2=avcurcon(j)
	   y3=y1-y2
	   if(y1.lt.ymin1) ymin1=y1
	   if(y1.gt.ymax1) ymax1=y1
	   if(y2.lt.ymin2) ymin2=y2
	   if(y2.gt.ymax2) ymax2=y3
	   if(y3.lt.ymin3) ymin3=y3
	   if(y3.gt.ymax3) ymax3=y3
150	continue
	if(iplot.eq.1) then
	   if(naver.ge.1.and.navc.eq.0) then
		ymin=ymin1
		ymax=ymax1
	   else if(naver.eq.0.and.navc.ge.1) then
		ymin=ymin2
		ymax=ymax2
	   else if(naver.ge.0.and.navc.ge.1) then
		ymin=ymin1
		ymax=ymax1
		if(ymin2.lt.ymin1) ymin=ymin2
		if(ymax2.gt.ymax1) ymax=ymax2
	   endif
	endif
	if(iplot.eq.2) then
	   ymin=ymin3
	   ymax=ymax3
	endif
	if(debug()) print 704,ymin,ymax
704	format(' ymin,ymax = ',2g13.6)
c	call FIXAX(ymin,ymax,ymin4,ymax4,ytic,0)
c	ymin=ymin4
c	ymax=ymax4
	if(debug()) print 704,ymin,ymax
	curcal=(ymax-ymin)*calfac	!ymax-ymin in pA
	ymin=ymin/acal
	ymax=ymax/acal
	if(debug()) print 704,ymin,ymax
c	curcal=(ymax-ymin)*calfac*acal	!pA per ymax-ymin=full scale volts
c
c For I/V make x-scale wider than data to give good shape for I/V plot
	if(itype(ipuls).eq.1) then	!I/V
	   rx=abs(xmax-xmin)
	   xmin1=xmin-0.25*rx
	   xmax1=xmax+0.25*rx
	else
	   xmin1=xmin
	   xmax1=xmax
	endif
c
c Plot points i1 to i2 (and, for jump, mark posn of jump with bar)
	imode=18
c	i2=0		!overwrites i2 above!
	i2par=0
	call PLOTS(0,i2par,imode)		!start graphics
	call INITLGRF
	ry=abs(ymax-ymin)
	call SCALEL(xmin1,xmax1,ymin-0.1*ry,ymax+0.15*ry)	!leave margins
c
c Make array of points already scaled to screen units in xdisp,ydisp
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
	sy1=sy/acal				!ADC0 scale in volts in CED
c	if(debug().and.discprt) write(8,53) ipuls,i1,i2
c53	format(' ipuls,i1,i2 = ',3i8)
c Note: the voltage for I/V is not sampled at exactly same time as current
c Current on ADC0 is first point, voltage is one tick later (0.5*dx later,
c where dx=1/(sample rate per channel), so Vm(i) is voltage at 0.5*dx later
c than Cur(i).  Want to plot cur(i) against the voltage at the same moment,
c which, insofar as voltage changes approx linearly over a time=dx,
c should be Vm(i)= mean of V(i-1) and V(i); for cur(1) must therefore
c extrapolate so Vm(1)=V(t1)-0.5*(V(t2)-V(t1))
c    NB for test data in which V output is recorded on both ADC0 and ADC1, so
c I/V should be linear (net=zero), get better result without interp (x=xj below)
c e.g. when 'current' goes up a step, from 100 to 120 say, the 'voltage' does
c same, so 'current'=120 gets plotted against 'voltage'=110=0.5*(100+120) for
c the 1st point after the step, but next value of 'current'=120 still gets
c plotted against 'voltage' of 120; thus 1st point after each step is a bit out
c of line giving the I/V the appearence of hysteresis (check with debug
c statements at label 32 above, and 54 below (and at 163 in defvolt)
	if(itype(ipuls).eq.1) then		!ramp
	  if(i1.gt.1) then
c	   xlast=0.5*(avolt(i1-1)+avoltcon(i1-1))*vcal
	   xlast=avolt(i1-1)*vcal		!==========for tests
	  else
c	   v1=0.5*(avolt(1)+avoltcon(1))*vcal
c	   v2=0.5*(avolt(2)+avoltcon(2))*vcal
	   v1=avolt(1)*vcal		!==========for tests
	   v2=avolt(2)*vcal		!==========for tests
	  endif
	endif
c Problem in defining xdisp turns on how i1,i2 are defined when only part
c of the sample is plotted (1 jump or ramp selected from whole sweep). If
c whole sweep plotted, so i1=1,i2=nsamp1 there is no problem. In latter case
c the following loop (161) defines the nsamp1 values of x correctly
c Also a problem if sample points omitted for a V-ramp, though not likely
c to do so!
	if(nvjump.eq.0.or.onejump) then
	  j=0
	  do 161 i=1,nsamp
	   if(jmask(i).eq.0) goto 161	!skipped point
	   x=float(i-1)*dx		!=msec from 0 to itsamp-1
	   j=j+1
	   xdisp(j)=x*sx + xoff
161	  continue
	else if(keepall) then		!can use orig calc (jmask all=1)
	  do 162 i=1,nplot
	   j=i1+i-1					!=i1,i1+1,...
	   if(itype(ipuls).eq.0) then		!jump
		x=xmin+float(i-1)*dx  		!time
	   else		!ramp
c	      xj=0.5*(avolt(j)+avoltcon(j))*vcal	!mean of drug & con voltage (mV)
	      xj=avolt(j)*vcal	!mean voltage (mV)===========FOR TESTS
		x=xj		!========test without interp-better for test data!
	   endif
	   xdisp(i)=x*sx + xoff
162	  continue
	else
	   call BELL(3)
	   if(alpha) then
	   print 163
163	   format(' VJDISP3 not yet fixed to plot section of sweep when',
     &  ' points omitted')
	   pause
	   else
	     call wdialog(ibox,
     &     ' VJDISP3 not yet fixed to plot section of sweep when points
     &omitted',icwd)
	   endif
	   RETURN
	endif
c
c	Now define Ydisp (if not whole sample used the appropriate values
c	are already omitted from iADC, avcur etc
	do 15 i=1,nplot
	   j=i1+i-1					!=i1,i1+1,...
c	   ydisp(i)=float(iADC(j))*sy + yoff
	   ydisp(1,i)=avcur(j)*sy1 + yoff	!jump in 'drug'
	   ydisp(2,i)=avcurcon(j)*sy1 + yoff	!jump in 'control'
	   ydisp(3,i)=(avcur(j)-avcurcon(j))*sy1 + yoff	!net jump
c	   if(discprt) write(8,80)
c     &	xdisp(i),ydisp(1,i),ydisp(2,i),ydisp(3,1)		!debug!!
c80	   format(4g13.6)
15	continue
	xdisp(nsamp1+1)=xdisp(nsamp1) !need to draw bar if V jump goes to end of sample
c
c	if(debug()) pause 'position 1'
c draw data
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c	print 200
c200	format(' Plot point by point [N] ? ')	!debug
c      read 101,ans
c101   format(a1)
c      deb=UC(ans).eq.'Y'	!problem with debug() causing crashes
c	deb=.false.
c
c
c For V-jump, draw bar to mark posn of jump, and zero current line
c For I/V draw axes at 0,0
c	   if(debug().and.discprt) write(8,81) xmin,xmax,ymin,ymax
c81	   format(' xmin,xmax,ymin,ymax= ',4g13.6)
	if(itype(ipuls).eq.0) then	!jump
	   ipen=9
	   call MOVEL(xmin,0.)
	   call DRAWL(xmax,0.)     !blue line at zero current
	endif
	if(itype(ipuls).eq.1) goto 203		!ramp
c Bars for jump:
	if(ncjump.eq.0) goto 202
c Draw horizontal bars to mark position of jumps (code from LPLOTCJ)
c First C-jump starts at point iDACc(nDc) which coincides with iADC(nAc)
c (plus dnAc microsec), and ends at point ilen later where ilen=pulse length
c expressed in terms of DAC or ADC units
	ipen=15		!white
	t1=(float(nAc-1)*dA + dnAc)/1000. !msec from trigger to start of 1st pulse
	t1=t1-xmin		!xmin may not be 0 if jump selected from whole sweep
	ylev=ymin+1.05*(ymax-ymin)	!y level to draw bar in world units
	do 20 i=1,ncjump
	  t2=t1+float(ilenc(i))*1.e-3		!msec from trigger to end of pulse
	  if(t2.lt.xmin) goto 20
	  if(t1.gt.xmax) goto 202		!finished
	  if(t1.ge.xmin.and.t2.le.xmax) then
	     call MOVEL(t1,ylev)
	     call DRAWL(t2,ylev)
	  else if(t1.lt.xmin.and.t2.le.xmax) then
	     call MOVEL(xmin,ylev)
	     call DRAWL(t2,ylev)
	  else if(t1.ge.xmin.and.t2.gt.xmax) then
	     call MOVEL(t1,ylev)
	     call DRAWL(xmax,ylev)
	  endif
	  t1=t2 + float(igapc(i))*1.e-3	!time for start of next pulse in msec
20	continue
c Now bars  for vjumps/ramps, if any (bit lower, and yellow)
202	continue
	if(vjump) then
	  ipen=14           !yellow
	  t1=(float(nAv-1)*dA + dnAv)/1000. !msec from trigger to start of 1st pulse
	  t1=t1-xmin	!xmin may not be 0 if jump selected from whole sweep
	  ylev=ymin+1.03*(ymax-ymin)	!y level to draw bar in world units
	  do 201 i=1,nvjump
	  t2=t1+float(ilenv(i))*1.e-3		!msec from trigger to end of pulse
	  if(t2.lt.xmin) goto 201
	  if(t1.gt.xmax) goto 203		!finished
	  if(t1.ge.xmin.and.t2.le.xmax) then
	     call MOVEL(t1,ylev)
	     call DRAWL(t2,ylev)
	  else if(t1.lt.xmin.and.t2.le.xmax) then
	     call MOVEL(xmin,ylev)
	     call DRAWL(t2,ylev)
	  else if(t1.ge.xmin.and.t2.gt.xmax) then
	     call MOVEL(t1,ylev)
	     call DRAWL(xmax,ylev)
	  endif
	  t1=t2 + float(igapv(i))*1.e-3	!time for start of next pulse in msec
201	continue
	endif
203	continue
	if(itype(ipuls).eq.1) then		!ramp
	   ipen=15
	   call MOVEL(xmin,0.)		!draw axes
	   call DRAWL(xmax,0.)     !x axis
c	   call MOVEL(0.,-5.0)
c	   call DRAWL(0.,5.0)     !y axis
	   call MOVEL(0.,ymin)
	   call DRAWL(0.,ymax)     !y axis
	   call REALTOCH(xmin,ccal,inchar)
	   call REALTOCH(xmax,ccal1,inchar)
	   call NEWPEN(15)		!white -for GTEXT call
	   call GTEXT(27,0,'X-scale = '//ccal//' to '//ccal1//' mV')
	endif
c Plot data
	call NEWPEN(12)		!red -for GTEXT call
	if(iplot.eq.1.and.naver.gt.0) then
	   do 2 i=1,nplot
	    call SETPIX(xdisp(i),ydisp(1,i),12)		!drug=red
2	   continue
	   call GTEXT(0,0,'Agonist     ')
	endif
	if(naver.eq.0) call GTEXT(0,0,'(No agonist)')
	call NEWPEN(11)		!light blue -for GTEXT call
	if(iplot.eq.1.and.navc.gt.0) then
	   do 3 i=1,nplot
	    call SETPIX(xdisp(i),ydisp(2,i),11)		!con=light blue
3	   continue
	   call GTEXT(1,0,'Control     ')
	endif
	if(navc.eq.0) call GTEXT(1,0,'(No control)')
	call NEWPEN(14)		!yellow -for GTEXT call
	if(iplot.eq.2.and.naver.gt.0.and.navc.gt.0) then
	   do 6 i=1,nplot
	    call SETPIX(xdisp(i),ydisp(3,i),14)		!net=yellow
6	   continue
	   call GTEXT(1,0,'NET current ')
	endif
	ymin5=ymin*calfac*acal		!in pA
	ymax5=ymax*calfac*acal		!in pA
	if(curcal.le.1000.) then
	   call REALTOCH(ymin5,ccal,inchar)
	   call REALTOCH(ymax5,ccal1,inchar)
	   unit=' pA '
c	   call REALTOCH(curcal,ccal,inchar)		!pA for full scale
c	   call GTEXT(26,1,' Full scale = '//ccal//' pA')
	else if((curcal.gt.1000.).and.curcal.le.1.e6) then
	   call REALTOCH(ymin5*1.e-3,ccal,inchar)
	   call REALTOCH(ymax5*1.e-3,ccal1,inchar)
	   unit=' nA '
c	   call REALTOCH(0.001*curcal,ccal,inchar)	!nA for full scale
c	   call GTEXT(26,1,' Full scale = '//ccal//' nA')
	else if(curcal.gt.1.e6) then
	   call REALTOCH(ymin5*1.e-6,ccal,inchar)
	   call REALTOCH(ymax5*1.e-6,ccal1,inchar)
	   unit=' muA'
c	   call REALTOCH(1.e-6*curcal,ccal,inchar)	!microamp full scale
c	   call GTEXT(26,1,' Full scale = '//ccal//' muA')
	endif
	call NEWPEN(15)		!white -for GTEXT call
	call GTEXT(28,0,'Y-scale = '//ccal//' to '//ccal1//unit)
	goto 999
c
c
c Separate bit for V(t) vs t
190	continue
	xmin=0.0
	xmax=float(itsamp)/1000.		!msec
	dx=(xmax-xmin)/float(nsamp)
c	ymin=-6.0		!volts
c	ymax=+5.5		!volts
	acal=6553.6		!adc units/volt
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
	imode=18
	i2par=0
	call PLOTS(0,i2par,imode)		!start graphics
	call INITLGRF
c In call to SCALEL make margins by calling with range greater than xmin,xmax,..
c	rx=abs(xmax-xmin)
c	ry=abs(ymax-ymin)
c	xmin1=xmin-0.05*rx
c	xmax1=xmax+0.05*rx
c	ymin1=ymin-0.05*ry
c	ymax1=ymax+0.05*ry
	ymin1=1.e37
	ymax1=-1.e37
	if(naver.gt.0.and.navc.gt.0) then
	 do 151 i=1,nsamp1
	   y1=avolt(i)
	   y2=avoltcon(i)
	   if(y1.lt.ymin1) then
		ymin1=y1
		imin=i
	   endif
	   if(y1.gt.ymax1) then
		ymax1=y1
		imax=i
	   endif
	   if(y2.lt.ymin1) ymin1=y2
	   if(y2.gt.ymax1) ymax1=y2
151	 continue
	else if(naver.gt.0.and.navc.eq.0) then
	 do 155 i=1,nsamp1
	   y1=avolt(i)
	   if(y1.lt.ymin1) then
		ymin1=y1
		imin=i
	   endif
	   if(y1.gt.ymax1) then
		ymax1=y1
		imax=i
	   endif
155	 continue
	else if(naver.eq.0.and.navc.gt.0) then
	 do 156 i=1,nsamp1
	   y1=avoltcon(i)
	   if(y1.lt.ymin1) then
		ymin1=y1
		imin=i
	   endif
	   if(y1.gt.ymax1) then
		ymax1=y1
		imax=i
	   endif
156	 continue
  	endif
	if(debug()) print 704,ymin1,ymax1
c	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
	if(debug()) print 704,ymin,ymax
c	ymin=ymin*vcal
c	ymax=ymax*vcal
	ymin=ymin1*vcal
	ymax=ymax1*vcal
	if(debug()) print 704,ymin,ymax
	ry=abs(ymax-ymin)
	call SCALEL(xmin,xmax,ymin-0.1*ry,ymax+0.1*ry)	!leave margins
	sy1=sy*vcal				!ADC0 scale in volts in CED
	if(debug()) then
	   print 705,vcal,curcal
705	   format(' vcal,curcal = ',2g13.6)
	   pause
	endif
	j=0		!index for xdisp
	do 160 i=1,nsamp
	   x=xmin+float(i-1)*dx
	   if(jmask(i).eq.0) goto 160  		!skipped X value (Y values OK)
	   j=j+1
	   xdisp(j)=x*sx + xoff
160	continue
	do 152 i=1,nsamp1
	   ydisp(1,i)=avolt(i)*sy1 + yoff		!voltage in 'drug'
	   ydisp(2,i)=avoltcon(i)*sy1 + yoff		!voltage in 'control'
152	continue
	xdisp(nsamp1+1)=xdisp(nsamp1)+dx !need to draw bar if V jump goes to end of sample
c zero line
	ipen=9
	call MOVEL(xmin,0.)
	call DRAWL(xmax,0.)     !blue line at zero current
c Draw data
	call NEWPEN(12)		!red -for GTEXT call
	if(naver.gt.0) then
	   do 154 i=1,nsamp1
	    call SETPIX(xdisp(i),ydisp(1,i),12)		!drug=red
154	   continue
	   call GTEXT(0,0,' Agonist     ')
	endif
	if(naver.eq.0) call GTEXT(0,0,' (No agonist)')
	call NEWPEN(11)		!light blue -for GTEXT call
	if(navc.gt.0) then
	   do 153 i=1,nsamp1
	    call SETPIX(xdisp(i),ydisp(2,i),11)		!con=light blue
153	   continue
	   call GTEXT(1,0,' Control     ')
	endif
	if(navc.eq.0) call GTEXT(1,0,' (No control)')
c Draw Y axis and calibration
c	ipen=15
	ipen=9
	call MOVEL(xmin,ymin)
	call DRAWL(xmin,ymax)     !y axis -now dark blue
	call NEWPEN(15)
	call REALTOCH(ymin,ccal,inchar)
	call REALTOCH(ymax,ccal1,inchar)
	call GTEXT(28,0,' Y-scale = '//ccal//' to '//ccal1//' mV')
c If there is only one ramp mark the min and max voltages it should reach
c with brown bars of length 0.1*(xmax-xmin)
	nr=0
	do 158 i=1,nvjump
	   if(ivolt1(i).ne.ivolt2(i)) then
		nr=nr+1
		ipuls=i
	   endif
158	continue
	if(nr.eq.1) then
	   v1=float(ivolt1(ipuls))
	   v2=float(ivolt2(ipuls))
	   if(v1.gt.v2) then
		v=v1
		v1=v2
		v2=v
	   endif
	   xl=0.05*abs(xmin-xmax)
	   ipen=6
	   x=xmin+float(imin-1)*dx
	   call MOVEL(x-xl,v1)
	   call DRAWL(x+xl,v1)
	   x=xmin+float(imax-1)*dx
	   call MOVEL(x-xl,v2)
	   call DRAWL(x+xl,v2)
	endif
c
999	continue
	call LOCATE(1,1)
c	print 58
c58	format(' Another plot [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') then
	   if(sampv) then
		if(naver.ge.1.and.navc.ge.1) then
		   iplot=iplot+1
		else
		   iplot=iplot+1
		   if(iplot.eq.2) iplot=3
		endif
	   else
		if(naver.ge.1.and.navc.ge.1) then
		   iplot=iplot+1
		   if(iplot.eq.3) iplot=4
		else
		   if(iplot.eq.1) iplot=4
		endif
	   endif
	   if(iplot.gt.4) iplot=1
c	   goto 111
c	endif
	call NEWPEN(15)		!for GTEXT call
	call GTEXT(0,60,'ANY KEY to continue')
	call ANYKEY
	goto 111
c	RETURN
	end
