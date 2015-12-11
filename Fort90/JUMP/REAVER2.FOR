	subroutine REAVER2(istrec,Yval,nj1,nj2,n1,n2,jmean,mono,jmask,
     &  iver1,ikeep1,nkeep,kstep,control,itsamp,vjump,sampv,keepall,
     &  nsamp,nsamp0,nmax,ndv1,ndimd,makav,calfac,calvolt,idest,
     &  ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &  filter,itpref,fcnew)
c
c To do graphics and averaging in CJFIT (after array sizes etc defined
c in REAVER1). Totals kept now in local 2D arrays TOTCUR, TOTVOLT (these
c were call avcur, avolt in original but this is potentially confusing!)
c 	THIS VERSION USES IOANA GRAPHICS, NOT LAHEY. Will be fastest if
c scaling is done as in SCAN by first converting to intermediate units?
c No -because each sweep is read only once and displayed only once, so
c no advantage to converting to intermed units when reading it.  Read
c values in READREL as before, but convert to pixel (VGA) units for display.
c
c OLD NOTES: see REAVER1
c
	real*4 Yval(ndv1,ndimd)		!as allocated in CJFIT
c Local allocatable arrays
	ALLOCATABLE::TOTCUR,TOTVOLT	!local in REAVER2
	real*4 TOTCUR(:,:),TOTVOLT(:,:)
	ALLOCATABLE::jcol			!local in REAVER2, for IDCPIX1
	integer*1 jcol(:)
	ALLOCATABLE::ixdisp,iydisp,iydispv,iyavc,iyavv   !local in REAVER2
	ALLOCATABLE::YIN,YOUT		!for filtering
	real*4 YIN(:),YOUT(:)
	integer*2 ixdisp(:),iydisp(:),iydispv(:),iyavc(:),iyavv(:)
	integer*1 jmask(nsamp0)
	logical mono,curdisp,sampv,allocated,sampatch
c Declarations as in REAVER1
c=	real*4 t1v(10),t2v(10),t1c(10),t2c(10)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 istrec(500)
	integer*2 ivhclamp,ipatch
	integer*2 ivhcsav,ivhsav
      character cdate*11,ctime*11,ch*1,getch*1
	character title1*79
	logical discprt,pon,slock,vjump,control
	logical control1,vjump1,sampv1,fixpot,filter
c      character*1 ans,UC
c for CJUMP3/4 data
	integer*2 iver1
c=	integer*2 ikeep1(4,2),kstep(5)
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
c=	integer*2 ikeep11(4,2),kstep1(5)	!===to read next sweep
	integer*4 ikeep11(4,2)
	integer*2 kstep1(5)
	logical keepall
	integer*4 videotyp

C For DCMENU
	integer Lb(30)
	character*78 text(30)
	common/DMENU/csizb,ifonb,csize,ifont,nboxlast,nblast		!for DCMENU
c For dialog
	character defolt*30,cans*30		!to hold text & result of QDIALOG
c For IDCPIX/SETACT
	integer iact(0:15)
	character cnum*11,cnum1*11,string*60
c
	common/dp/discprt
c==	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
c==	common/lgrf/xpos,ypos,ipen		!ditto
c=	COMMON/cjpar/nsweep,
c=     & naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
c=     & nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
c=     & ivhold,irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
c=     & t1v,t2v,t1c,t2c,nDv,iDd,keepall
c
	pon()=slock()
c
c Allocate local arrays
	ALLOCATE (ixdisp(nsamp),iydisp(nsamp),iydispv(nsamp),
     &	iyavc(nsamp),iyavv(nsamp),jcol(nsamp))
c NB jv=up to 4, jc up to 2, so allocate accordingly (can run out of memory
c when nsamp=50000 if ndimd=10!)
	ndc=2
c==	ALLOCATE (TOTCUR(nsamp,ndimd),TOTVOLT(nsamp,ndimd))
	ALLOCATE (TOTCUR(nsamp,ndc))
	if(sampv) then
	   ndv=4
	   ALLOCATE (TOTVOLT(nsamp,ndv))
	endif
c
	idest=0
c
c Start graphics
	if(videotyp().ne.18) then
	  idev=0					!screen
	  call INIPLT(idev,.false.,1.0)	!start Hgraph
c	  imode=18
c	  i2=0
c==	  call PLOTS(0,0,18)		!start Lahey graphics
	endif
c==	call INITLGRF
c
	icw=11		!colour to write in dialog box (light blue)
	icw1=12		!red to write in dialog box for warnings
c	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	ictd=icw		!icw is older notation -better use value in common/dialval/
	icfd=7		!frame col for dialog box 1
	icbd=0		!background col for dialog box (0 until fixed)
c
	ibk=3			!background outside data area
	ibk1=1		!background for data area dark blue (or 8=grey?)
	if(mono) then
c==	   ipen=15
	   ibk=0	!for monochrome
	   ibk1=1	!for monochrome
	   icw=15
	   icw1=15
	endif
	icfd2=11	!frame colour for dialog box 2
c Scaling for Ioana graphics:
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Use Y coords (0-479) between 127, 457 for now, to span 65536 ADC units
	yd1=127.	!lowest point on screen (Y pixels) for display of channels
	yd2=457.	!highest point on screen (Y pixels) for display of channels
c=	ibk2=0
	sfac1=(yd2-yd1)/65536.	!scal fac for ADC to pixel units
c	calfac1=calfac/sfac1		!converts pixel display units to pA
	sfac3=sfac1/calfac		!converts pA to pixel units
	iyoff=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre?

c Fill background
	call CLEARS(0,480,ibk)	!fill screen with light blue (for outside data area)
	iyd1=55
	iyd2=387
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
c	   subroutine DEFBOX(ibox,irhi,iclo,nrow,ncol,icb)
	call DEFDIALOG(1,1,2,4,60,icbd)	!define dialog box #1
	call DEFDIALOG(2,1,65,4,14,icbd)	!define dialog box #1
	call OPENDIALOG(1,icfd,.true.)	!draw dialog box #1
	call OPENDIALOG(2,icfd2,.true.)	!draw dialog box #2
c
	if(jmean.eq.1) then
	   jc=1		!current in yval(i,1)
	   jv=2		!voltage in yval(i,2)
	   if(makav.eq.2) jv=3		!voltage in yval(i,3)
	else 			!for 2nd mean
	   jc=2		!current in yval(i,2)
	   jv=4		!voltage in yval(i,4)
	endif
c
c==	ndelt=ngr	!to ensure =<2048 points displayed
	ndelt=1	!display every point now
c NB skipping plotted points when keepall=false not done yet (more complex
c since really want to skip points only in those regions where all are kept)
	if(keepall.and.nsamp.gt.2048) then		!suggest point omission
	   ndelt=1 + (nsamp-1)/2048
         call INTCONV(nsamp,cnum)
	   n=NBLANK(cnum)
 	   call DEFOLTi(ndelt,defolt)
	   call QDIALOG(1,
     &	'Sample = '//cnum(1:n)//' points: plot every nth'
     &	,defolt,12,cans)
	   call GETINPi(cans,ndelt)
	endif
c
c==	xmin=0.0
c==	xmax=float(itsamp)/1000.		!msec
c Make ymin smaller than any actual data, to leave margin at bottom
c for the Drawbox boxes, and narrower margin at top for messages
c Data are already scaled to pA etc, but better convert back to volts
c (at input of 1401) for display, if only so that current and voltage
c can both be shown without calling SCALEL again
c==	ymin=-6.0		!volts
c==	ymax=+5.5		!volts
c	yc1=-1.0		!posn for top and bottom of cursor (volts)
c	yc2=+1.0
	acal=6553.6		!adc units/volt
c==	curscal=1.0/(acal*calfac) !pA/calfac=ADC; ADC/acal=V so pA*curscal=volts
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
c==	vscal=1.0/(vcal*acal)	!mV/vcal=ADC; ADC/acal=V so mv*vscal=volts
c Lahey Scaling: 1401 units are -32768 to +32752 in 16 unit increments; dividing
c==	call SCALEL(xmin,xmax,ymin,ymax)
c==	syc=sy*curscal	!so means (kept in pA) scaled correctly
c==	syv=sy*vscal	!so means (kept in mV) scaled correctly
c
c Put up boxes
c defs for DCMENU
c	nbox=10	!number of boxes for DCMENU
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
c=	icol3=12	!red text/border
c=	icol4=15	!white fill
c=	if(mono) then
c=	   icol3=15		!white text/border
c=	   icol4=0		!black fill
c=	endif
	ifonb=0
	csizb=1.7
	ifont=4
	csize=2.
	nbline=5		!define to prevent error when deleting dcmenu
	nbox=10		!define to prevent error when deleting dcmenu
	curdisp=.true.
	nbox=9	!number of boxes for DCMENU
	call SETLBOX(3,Lb,1)
	call NUMSET			!set num lock on ready for response
	if(.not.sampv) Lb(1)=0
	Lb(9)=1
	call DCMENU(0,5,Lb,text,0,0)		!delete all
	if(curdisp) then
	   text(1)='1.SHOW VOLTAGE'
	else
	   text(1)='1.SHOW CURRENT'
	endif
	text(2)='2. REJECT   '
	text(3)='3. ACCEPT   '
c====add EXPAND?
	text(9)='9. FINISH   '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c
c Initialise TOTCUR,TOTVOLT
	do i=1,nsamp
	   do j=1,ndc
		TOTCUR(i,j)=0.0
	   enddo
       enddo
	if(sampv) then
	  do i=1,nsamp
		do j=1,ndv
		   TOTVOLT(i,j)=0.0
		enddo
         enddo
	endif
c
	idef=1
	curdisp=.true.
	nmean=0		!number of sweeps accepted for average
c
	do 13 iswp=n1,n2			! start of loop for each event
	irec=int4(istrec(iswp))
	if(irec.le.-1.and.irec.ge.-32768) then
	   irec=irec+65536
	else if(irec.lt.-32768) then
	   print*,' IREC IS TOO BIG!!'
	   STOP
	endif
c====start altered bit
c Read the specified record  'header' : BUT do not overwrite values
c for the first sweep (#n1) (at present the whole 3rd line of read statement,
c	  and also ilen2c, igap2c do overwrite previous values -rest do not
c	  and can be used to test if parametes match)
c	read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control1,
c     & vjump1,sampv1,nsamp1,itsamp1
	call READ14(irec,cdate,ctime,title1,naver,navc,iav,control1,
     &    vjump1,sampv1,nsamp1,itsamp1,nAv1,dnAv1,nvjump1,iTPREv1,
     &    ilen2v,igap2v,
     &    ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc1,dnAc1,
     &    ncjump1,ilen2c,igap2c,nDv,iDd1,iver1,nsweep1,swtime1,isweep1,
     &    ikeep11,nkeep1,kstep1,amVpA11,ftape1,gain1,errfac1,nmax,
     &    ivhclamp,ipatch,ffilt,sampatch,0,defname,npatch,temp)
c (NB don't read nDv1 here as in orig, because this symbol used for array dim)
	nsamp0=nsamp1		!# of points if all points kept
	if(iver1.le.-1003) then
c=	   if(allocated(jmask)) DEALLOCATE(jmask)
c=	   ALLOCATE(jmask(nsamp0))
	   call CALCNS2(ikeep11,nkeep1,kstep1,nsamp1,nsamp11,jmask,
     &    keepall,nsamp0)
	   nsamp0=nsamp1		!orig # of points samples
	   nsamp=nsamp11		!# of points kept on disc
	endif
c
	if(fixpot) then
	   ivhold=ivhsav
	   ivhclamp=ivhcsav
	endif
c Xdisp same for all sweeps so calc on first sweep only, after jmask defined
c (was prev calc before loop, but jmask not defined now, if this is done)
c Xdisp is in now integer*2 in VGA pixel units for IDCPIX i.e. 0-639.
	if(iswp.eq.n1) then
	 if(keepall) then
	   dx=(639.)/float(nsamp)
	   i1=0
	   do 15 i=1,nsamp,ndelt
	    x=float(i-1)*dx
	    i1=i1+1
	    ixdisp(i1)=int2(ifixr(x))
15	   continue
	 else
	   dx=(639.)/float(nsamp0)
	   j=0
	   do 151 i=1,nsamp0
	    if(jmask(i).eq.0) goto 151	!skipped point
	    x=float(i-1)*dx			!=msec from 0 to itsamp-1
	    j=j+1
	    ixdisp(j)=int2(ifixr(x))
151	   continue
	 endif
	endif		!end of calc of xdisp on 1st cycle only
c
c Print (not screen) the summary details
	call PREC(iswp,title1,cdate,ctime,naver,navc,iav,vjump1,
     &   control1,iver1,3)
	call PPAR(0,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     & ivhclamp,ipatch,ffilt)
c Test a few things to check samples comparable
c===next statement altered
	if(iswp.gt.n1.and.(itsamp.ne.itsamp1.or.nsamp.ne.nsamp1.or.
     & (vjump.neqv.vjump1).or.(sampv.neqv.sampv1))) then		!===
c Note: neqv is logical 'not equal'
	   call INTCONV(iswp,cnum)
	   call INTCONV(n1,cnum1)
	   string='Parameters for the data sets'//charnb(cnum)//
     &	' and '//charnb(cnum1)//' do not match:'
	   call WDIALOG(1,charnb(string),ictd)
	   call BELL(3)
c==	   call LOCATE(0,0)
c==	   print 33,iswp,n1
c==33	   format(
c==     &   '&Parameters for the data sets',i3,' and',i3,' do not match:')
c
c==	   if(itsamp.ne.itsamp1) print 331,float(itsamp)/1000.,
c==     &	float(itsamp1)/1000.
c==331	   format(' Sample duration = ',2g13.6)
c==	   if(nsamp.ne.nsamp1) print 332,nsamp,nsamp1
c==332	   format(' Number of points = ',2i8)
c==	   if(vjump.neqv.vjump1) print 333,vjump,vjump1
c==333	   format(' Vjump = ',2l4)
c==	   if(sampv.neqv.sampv1) print 334,sampv,sampv1
c==334	   format(' Sampv = ',2l4)
	   if(itsamp.ne.itsamp1) then
		call DCFORMAT(float(itsamp)/1000.,7,1,cnum)
		call DCFORMAT(float(itsamp1)/1000.,7,1,cnum1)
		string=' Sample duration = '
     &		//charnb(cnum)//','//charnb(cnum1)
	      call WDIALOG(1,charnb(string),icw1)
	   else if(nsamp.ne.nsamp1) then
		call INTCONV(nsamp,cnum)
		call INTCONV(nsamp1,cnum1)
		string=' Number of points = '
     &		//charnb(cnum)//','//charnb(cnum1)
	      call WDIALOG(1,charnb(string),icw1)
	   else if(vjump.neqv.vjump1) then
	      call WDIALOG(1,' One is V-jump, other is not',icw1)
	   else if(sampv.neqv.sampv1) then
	      callWDIALOG(1,' One has voltage sample, other has not',icw1)
	   endif
	   iopt=1
 	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     &	'(1) Reject this sweep; (2) Restart the average'
     &	,defolt,11,cans)
	   call GETINPi(cans,iopt)
c
c==	   print 20
c==20	   format(
c==     &   ' (1) Reject this sweep',/,
c==     &   ' (2) Restart the average',/,
c==     &   ' Option number [1] = ')
c==	   iopt=1
c==	   call INPUTi(iopt)
c==	   if(iopt.le.0.or.iopt.gt.2) iopt=1
	   if(iopt.eq.1) goto 19
	   call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	   idest=6
	   goto 999		!back to REAVER1 with idest=6
c==	   goto 26
	endif
	if(vjump.and.(control.neqv.control1)) then
	   call BELL(2)
c==	   call LOCATE(0,0)
	   call INTCONV(n1,cnum)
	   call INTCONV(iswp,cnum1)
	   if(control) then
	      string='Sweep '//charnb(cnum)//
     &	' was a control; this one (#'//charnb(cnum1)//') is NOT:'
	      call WDIALOG(1,charnb(string),ictd)
c==	      print 27,n1,iswp
c==27		format(
c==     &    '&Sweep ',i3,' was a control; this one (',i3,') is not:')
	   else
	      string='Sweep '//charnb(cnum)//
     &	' was NOT a control; this one (#'//charnb(cnum1)//') is:'
c==	      print 28,n1,iswp
c==28		format(
c==     &    '&Sweep ',i3,' was not a control; this one (',i3,') is:')
	      call WDIALOG(1,charnb(string),ictd)
	   endif
c
 	   call DEFOLTi(idef,defolt)
	   call WDIALOG(1,
     &	'(1) Reject this sweep; (2) Average it anyway;',11)
	   call QDIALOG(1,
     &	' (3) Restart the average',defolt,11,cans)
	   call GETINPi(cans,idef)
c==	   print 29,idef		!keep default for next time it happens
c==29	   format(
c==     &   ' (1) Reject this sweep',/,
c==     &   ' (2) Average it anyway',/,
c==     &   ' (3) Restart the average',/,
c==     &   ' Option number [',i2,'] = ')
c==	   call INPUTi(idef)

	   if(idef.eq.1) goto 19
	   if(idef.eq.3) then
		call VIDEOMOD(3)		!utility lib- this makes graph go for good!
		idest=6
		goto 999		!back to REAVER1 with idest=6
c==		goto 26
	   endif
	endif
	irec=int4(istrec(iswp))			!read data set #iswp
	if(irec.le.-1.and.irec.ge.-32768) then
	   irec=irec+65536
	else if(irec.lt.-32768) then
	   print*,' IREC IS TOO BIG!!'
	   STOP
	endif
c  Read data into Yval initially, to save rewriting READREL, but
c  copy it into TOTCUR, XDISP etc for display and averaging. Put
c  final average back into correct YVAL elements before leaving REAVER
c
	call READREL(irec,nsamp,iav,sampv,calfac,calvolt,
     &   ipatch,ivhclamp,Yval,jc,jv,ndv1,ndimd)
	if(filter) then
	   nd1=-53
	   nd2=nsamp+54
	   ALLOCATE(yin(nd1:nd2),yout(nd1:nd2))
	   do i=1,nsamp
		if(i.le.itpref) y1=y1+yval(i,jc)	!mean of 1st itpref points
		if(i.gt.nsamp-itpref) y2=y2+yval(i,jc) !mean of last itpref points
		yin(i)=yval(i,jc) 		!copy data to ytemp
	   enddo
	   y1=y1/float(itpref)
	   y2=y2/float(itpref)
	   do i=nd1,0
		yin(i)=y1
	   enddo
	   do i=nsamp+1,nd2
		yin(i)=y2
	   enddo
	   srate=float(irate)
	   call GFILTERR(yin,yout,nd1,nd2,srate,fcnew,nd1,nd2)
	   do i=1,nsamp
		yval(i,jc)=yout(i)
	   enddo
	   DEALLOCATE(yin,yout)
	endif
c Make Ydisp array for display of current (+ average)
c Equivalencing method, and Yval1 now abandoned!
c=c (see TJC.FOR for test of ndelt calcs -but these now marked 'c=' because
c=c they are replaced by equivalencing method; note that first element
c=c of jth col of Yval, viz Yval(1,j), corresponds to Yval1(1+(j-1)*n), where
c=c n=2048 here)
c=	ndelt=ngr	!ensures =< 2048 points displayed
	i=0
c=	ifst=1+(jc-1)*2048
c=	do 16 i1=ifst,nsamp+ifst-1,ndelt		!OK even if nsamp>2048
c=	 i=i+1
c=	 ydisp(i)=YVAL1(i1)*syc + yoff   !copy current, in volts at CED input
c=16	continue
	do i1=1,nsamp,ndelt
	   i=i+1		!in case ndelt.ne.1
c==	   ydisp(i)=YVAL(i1,jc)*syc + yoff   !copy current, in volts at CED input
	   iydisp(i)=int2(ifixr(YVAL(i1,jc)*sfac3)) + iyoff   !copy current
	enddo
	ndisp=i	!=number to display on exit
c
c======first latency code removed here
c
	if(sampv) then
c=	   i=0
c=	   ifst=1+(jv-1)*2048
c=	   do 161 i1=ifst,nsamp+ifst-1,ndelt		!OK even if nsamp>2048
c=	    i=i+1
c=	    ydispv(i)=YVAL1(i1)*syv + yoff		 !same for voltage
c=161	   continue
	   i=0
	   do i1=1,nsamp,ndelt
	      i=i+1
c==		ydispv(i)=YVAL(i1,jv)*syv + yoff		 !same for voltage
	      iydispv(i)=int2(ifixr(YVAL(i1,jv)*sfac3)) + iyoff   !copy voltage
	   enddo
	endif
c Put average into display arrays (TOTCUR hold TOTALS)
c (===ifst method now abandoned,as above)
	if(nmean.ge.1) then
	   en=float(nmean)
c=	   i=0
c=	   ifst=1+(jc-1)*2048
c=	   do 162 i1=ifst,nsamp+ifst-1,ndelt
c=	    i=i+1
c=	    yavc(i)=syc*avcur(i1)/en + yoff
c=162	   continue
	   i=0
	   do i1=1,nsamp,ndelt
	   	i=i+1
	   	iyavc(i)=int2(ifixr(sfac3*TOTCUR(i1,jc)/en)) + iyoff
	   enddo
	   if(sampv) then
c=	      i=0
c=	      ifst=1+(jv-1)*2048
c=		do 163 i1=ifst,nsamp+ifst-1,ndelt
c=		 i=i+1
c=		 yavv(i)=syv*avolt(i1)/en + yoff
c=163		continue
		i=0
		do i1=1,nsamp,ndelt
	   	   i=i+1
		   iyavv(i)=int2(ifixr(sfac3*TOTVOLT(i1,jv)/en)) + iyoff
		enddo
	   endif
	endif
c
	call INTCONV(iswp,cnum)
	call CLRDIALOG(2,icbd)
	call WDIALOG(2,'Sweep #'//charnb(cnum),10)
	if(nmean.ge.1) then
	   call INTCONV(nmean,cnum)
	   call WDIALOG(2,'Mean of '//charnb(cnum),12)
	endif
c Display data for current sweep
22	continue
	icol=10				!current sweep green
	if(mono) icol=15 			!bright white for monochrome
	call SETACT(iact,icol)		!draw green whatever
	do i=1,ndisp
	   jcol(i)=icol			!no need to use RPIXEL in IDCPIX
	enddo
	if(curdisp) then
	   call IDCPIX1(ixdisp,iydisp,ndisp,iact,jcol,1)
	else
	   call IDCPIX1(ixdisp,iydispv,ndisp,iact,jcol,1)
	endif
c
c Display current average
	if(nmean.ge.1) then
	   call SETACT(iact,-1)		!do not draw unless
	   iact(ibk1)=12			!draw red on background
	   iact(icol)=13			!draw purple if already green
	   if(curdisp) then
		call IDCPIX1(ixdisp,iyavc,ndisp,iact,jcol,0)
	   else
		call IDCPIX1(ixdisp,iyavv,ndisp,iact,jcol,0)
	   endif
	endif
c Need sep bit for monochrome that joins points for average -not done yet
c
60	continue          !return here for next char until accepted or rejected
	call NUMSET
	ch=GETCH(ktype)
	ival=ichar(ch)
	ikey=-1
	if(ktype.eq.3) ikey=ival-48
	call NUMCLR
c
	if(ikey.eq.9) goto 111	!finish
c
c===========large latency section removed here
c Accept
	if(ikey.eq.3) then
	   if(pon()) write(7,211)
	   if(discprt) write(8,211)
211	   format(' Accepted')
c
	   nmean=nmean+1
c
c	  Allocate TOTCUR as above, except ndelt=1 (average all nsamp points)
c=	   ifst=1+(jc-1)*2048
c=	   do 34 i=ifst,nsamp+ifst-1		!OK even if nsamp>2048
c=		avcur(i)=avcur(i)+yval1(i)
c=34	   continue
	   do i=1,nsamp
		TOTCUR(i,jc)=TOTCUR(i,jc)+yval(i,jc)
	   enddo
	   if(sampv) then
c=		ifst=1+(jv-1)*2048
c=		do 35 i=ifst,nsamp+ifst-1		!OK even if nsamp>2048
c=		 avolt(i)=avolt(i)+YVAL1(i)
c=35		continue
		do i=1,nsamp
		   TOTVOLT(i,jv)=TOTVOLT(i,jv)+YVAL(i,jv)
		enddo
	   endif
	   goto 19		!next event
	endif
c
c Reject
	if(ikey.eq.2) then
	   if(pon()) write(7,210)
	   if(discprt) write(8,210)
210	   format(' Rejected')
	   goto 19		!next event
	endif
c
c Swap current/voltage displays
	if(ikey.eq.1.and.sampv) then
c   delete the previous display
c -just fill data area to clear
	   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
c==	   do 23 i=1,ndisp
c==	   x=xdisp(i)
c==	   y=ydisp(i)
c==	   if(.not.curdisp) y=ydispv(i)
c==	   call SETPIX(x,y,0)		!delete
c==23	   continue
c==	   if(nmean.ge.1) then
c==		do 181 i=1,ndisp
c==		x=xdisp(i)
c==		y=yavc(i)
c==		if(.not.curdisp) y=yavv(i)
c==		call SETPIX(x,y,0)		!delete
c==181		continue
c==	   endif
c
	   curdisp=.not.curdisp
	   call DCMENU(-1,5,Lb,text,0,0)	!delete box 1
	   if(curdisp) then
		text(1)='1.SHOW VOLTAGE'
	   else
		text(1)='1.SHOW CURRENT'
	   endif
	   call DCMENU(-1,5,Lb,text,icol1,icol2)	!draw box 1
	   goto 22		!draw new display
	endif
c
	goto 60		!get another character if none of above done
c
19	continue
c=	call PLOTS(0,0,18)		!restart graphics
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
c==	call INIPLT(idev,.false.,1.0)	!restart Hgraph
c==	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c
13	continue	!end of loop for each event
c
c Finish:
111	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c Now have TOTAL of NMEAN events, in TOTCUR (and, if sampv, mean of
c corresponding membrane pots in TOTVOLT). Assign them to appropriate Yval(i,j)
c elements now.
c
	if(nmean.eq.0) then
	   call BELL(1)
	   print 252
252	   format(' Zero events averaged')
	   idest=4
	   goto 999		!deallocate local arrays and return with idest=4
	endif
	en=float(nmean)
c=	ifst=1+(jc-1)*2048
c=	do 25 i=ifst,nsamp+ifst-1		!OK even if nsamp>2048
c=	 YVAL1(i)=avcur(i)/en
c=25	continue
	do i=1,nsamp
	   YVAL(i,jc)=TOTCUR(i,jc)/en
	enddo
c
	if(sampv) then
c=	   ifst=1+(jv-1)*2048
c=	   do 251 i=ifst,nsamp+ifst-1		!OK even if nsamp>2048
c=	    YVAL1(i)=avolt(i)/en
c=251	   continue
	   do i=1,nsamp
	      YVAL(i,jv)=TOTVOLT(i,jv)/en
	   enddo
	endif
c
	if(makav.eq.2.and.jmean.eq.1) then
	   jmean=2		!doing the 2nd average
241	   print 24 ,nmean,nsamp
24	   format(
     & ' First average has ',i5,' sweeps, with ',i7, ' points:',/,
     &  ' For 2nd average, now read record numbers n1 to n2: n1,n2 = ')
	   call INPUT2i(n1,n2)
	   if(n2.lt.n1.or.n1.lt.1) goto 241
	   nj2=n1	!to specify param for 2nd average (same for n1,...,n2)
	   idest=5		!return with idest=5
	   goto 999
c=   	   goto 113		!do 2nd average
	endif
c
      if(pon()) write(7,30) nmean
      if(discprt) write(8,30) nmean
30	format(
     & 1x,i5,' sweeps from CJUMP.DAT averaged',/)
	iopt=1
c
	print 31,nmean,nsamp,iopt
31	format(
     & ' Average of ',i5,' sweeps, with ',i7, ' points:',/,
     & ' (1) Fit and/or plot it',/,
     & ' (2) Abandon this one and calculate another average',/,
     & ' (3) Exit now (average trace not kept)',/,
     & '  Option number [',i2,'] = ')
	call INPUTi(iopt)
	idest=iopt
c
	if(iopt.eq.2) then
	   idest=6
	   goto 999		!back to REAVER1 with idest=6
c==	   goto 26
	endif
c
999	continue
c Dealloc local arrays before leaving
	DEALLOCATE(ixdisp,iydisp,iydispv,iyavc,iyavv,totcur,jcol)
	if(allocated(totvolt)) then
	   DEALLOCATE(totvolt)
	endif
	RETURN
	end

