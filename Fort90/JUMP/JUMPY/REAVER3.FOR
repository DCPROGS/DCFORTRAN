c== fix optional skip on zero1 (open at end)
c===
	subroutine REAVER3(istrec,Yval,nj1,nj2,n1,n2,jmean,mono,jmask,
     &  iver1,ikeep1,nkeep,kstep,control,itsamp,vjump,sampv,keepall,
     &  nsamp,nsamp0,nmax,ndv1,ndimd,makav,calfac,calvolt,idest,
     &  ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &  filter,itpref,fcnew,ioffset,newform,
     &  align,tintlat,jumps,tzero,tsamp,endalign,tintend,tact,
     &  njd,ndim,ndim1)
c
c REAVER3 is version of REAVER2 which includes case where 1st latency is to
c be removed before averaging.
c 03/04/97 07:40am added option to line up on final transition (this uses
c voltage arrays to hold the time-reversed trace so can't do it when
c sampv=true)
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
c NOTES ON TIMES:
c Times measured from t=0 at first point in sample
c tintlat=first latency
c tintend=length of final shut time
c In original trace:
c 	activation starts at t=tfst=tzero + tintlat
c	activation ends at tlast=tsamp-tintend
c	length of activation tact = tlast-tfst
c When trace shifted to put first opening at tzero
c 	activation starts at t=tzero
c	activation ends at tzero+tact
c When trace shifted to put last shutting at tsamp-tzero
c 	activation starts at t=tsamp-tzero-tact
c	activation ends at tsamp-tzero
c PROBLEM -see GETSCAM -sum of all inetervals in a jump is, for some reason
c usually a few ms less that tsamp!  Therfore calculations below replaced
c by use of activation length measured in GETSCAN by adding individual
c intervals, so now use:
c		tac=tact(ij)
c		tlast=tfst+tac
c
c OLD NOTES: see REAVER1/2
c
	real*4 Yval(ndv1,ndimd)		!as allocated in CJFIT
c For 'align'
	real*4 tintlat(ndim)
	real*4 tintend(ndim1)
	real*4 tact(ndim1)
	integer*2 jumps(ndim)
	logical align,null,zero,bad,zoomed,newform,omit,undef,skip
	logical endalign,setcol
	integer fndcur
c
c Local allocatable arrays
	ALLOCATABLE::TOTCUR,TOTVOLT	!local in REAVER2
	real*4 TOTCUR(:,:),TOTVOLT(:,:)
	ALLOCATABLE::jcol			!local in REAVER2, for IDCPIX1
	integer*1 jcol(:)
	ALLOCATABLE::ixdisp,iydisp,iydispv,iyavc,iyavv   !local in REAVER2
	ALLOCATABLE::ixdispz,iydispz			   !for zoomed data
	ALLOCATABLE::YIN,YOUT		!for filtering
	real*4 YIN(:),YOUT(:)
	ALLOCATABLE::Yfill		!for alignment
	real*4 yfill(:)
	integer*2 ixdisp(:),iydisp(:),iydispv(:),iyavc(:),iyavv(:)
	integer*2 ixdispz(:),iydispz(:)
	integer*1 jmask(nsamp0)
	logical mono,curdisp,sampv,allocated,sampatch
c Declarations as in REAVER1
c=	real*4 t1v(10),t2v(10),t1c(10),t2c(10)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
c==	integer*2 istrec(500)
	integer*4 istrec(1000)
	integer*2 ivhclamp,ipatch
	integer*2 ivhcsav,ivhsav
      character cdate*11,ctime*11,ch*1,getch*1
	character title1*79
	logical discprt,pon,slock,vjump,control
	logical control1,vjump1,sampv1,fixpot,filter
      character*1 ans,UC
c for CJUMP3/4 data
	integer*2 iver1
c=	integer*2 ikeep1(4,2),kstep(5)
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
c=	integer*2 ikeep11(4,2),kstep1(5)	!===to read next sweep
	integer*4 ikeep11(4,2)
	integer*2 kstep1(5)
	logical keepall

C For DCMENU
	integer Lb(18)
	character*78 text(18)
	common/DMENU/csizb,ifonb,csize,ifont,nboxlast,nblast		!for DCMENU
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
c For dialog
	character defolt*30,cans*30		!to hold text & result of QDIALOG
c For IDCPIX/SETACT
	integer iact(0:15)
	character cnum*11,cnum1*11,string*60
c For mouse (drag to change vertical position)
c	logical mouse_on,mous_set,kbhit
c	common/mousval/mouse_on,nbutton
c
	common/dp/discprt
c
	pon()=slock()
c
	bad=.false.		!in case not 'align'
	omit=.false.	!in case not 'align'
	undef=.false.	!in case not 'align'
	idrev=3		!initially display both align on start and align on end
	ixz1=500		!initial default pos for zoom cursor
	ixz2=1500		!initial default pos for zoom cursor

c Allocate local arrays
	ALLOCATE (ixdisp(nsamp),iydisp(nsamp),iydispv(nsamp),
     &	iyavc(nsamp),iyavv(nsamp),jcol(nsamp))
c NB jv=up to 4, jc up to 2, so allocate accordingly (can run out of memory
c when nsamp=50000 if ndimd=10!)
	ndc=2
	ALLOCATE (TOTCUR(nsamp,ndc))
	if(sampv.or.(align.and.endalign)) then
	   ndv=4
	   ALLOCATE (TOTVOLT(nsamp,ndv))
	endif
c
	idest=0
	zoomed=.false.
c
c Start graphics
	if(videotyp().ne.18) then
	  idev=0					!screen
	  call INIPLT(idev,.false.,1.0)	!start Hgraph
	  ivxlo=0		! for VIEWPORT
	  ivxhi=10000
	  ivylo=1
	  ivyhi=7484
	  ixlo=0
	  ixhi=10000
	  iylo=1
	  iyhi=7484
	  call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
	  call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
	  call SCALE(0.0,639.,0.0,479.)		!for FNDCUR
	endif
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
	   ibk=0	!for monochrome
	   ibk1=1	!for monochrome
	   icw=15
	   icw1=15
	endif
	icfd2=11	!frame colour for dialog box 2
c Scaling for Ioana graphics:
c Scaling: 1401 units are -32768 to +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Use Y coords (0-479) between iyd1, iyd2 for now, to span 65536 ADC units
	iyd1=55
	iyd2=387
	yd1=float(iyd1)
	yd2=float(iyd2)
c=	yd1=127.	!lowest point on screen (Y pixels) for display of channels
c=	yd2=457.	!highest point on screen (Y pixels) for display of channels
	sfac1=(yd2-yd1)/65536.	!scal fac for ADC to pixel units
c	calfac1=calfac/sfac1		!converts pixel display units to pA
	sfac3=sfac1/calfac		!converts pA to pixel units
	scalfac=1.0
	sfac4=scalfac*sfac3
c Define y-offset for display after data defined
	iyoff=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre?
	iyoff=iyoff-40
	iyoff1=iyoff+130			!offset for mean
	iyoff2=iyoff+40			!offset for trace aligned on end
	iyoff3=iyoff1+40			!offset for mean aligned on end
c
c Fill background
	call CLEARS(0,480,ibk)	!fill screen with light blue (for outside data area)
c= cf yd1,yd2
c=	yd1=127.	!lowest point on screen (Y pixels) for display of channels
c=	yd2=457.	!highest point on screen (Y pixels) for display of channels
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
c=	call HLINE(0,639,126,12)	!===check yd1
c=	call HLINE(0,639,457,12)	!===check yd2
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
c Use voltage arrays for 'time-reversed' trace (so can't do when sampv=true)
	if(sampv.and.align.and.endalign) then
	   call BELL(4)
	   print 77
77	   format(' CANNOT DO ALIGNMENT ON END WHEN V-SAMPLED AT PRESENT')
	endif
	jc1=jv			!for jump aligned on last opening
c
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
c Make ymin smaller than any actual data, to leave margin at bottom
c for the Drawbox boxes, and narrower margin at top for messages
c Data are already scaled to pA etc, but better convert back to volts
c (at input of 1401) for display, if only so that current and voltage
c can both be shown without calling SCALEL again
	acal=6553.6		!adc units/volt
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
c
c Put up boxes
c defs for DCMENU
c	nbox=10	!number of boxes for DCMENU
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	ifonb=0
	csizb=1.7
	ifont=4
	csize=2.
	nbline=5		!define to prevent error when deleting dcmenu
	nbox=10		!define to prevent error when deleting dcmenu
	curdisp=.true.
	nbox=9	!number of boxes for DCMENU
	call SETLBOX(9,Lb,0)
	call NUMSET			!set num lock on ready for response
	if(sampv) Lb(1)=1
	if(align.and.endalign) Lb(5)=1
	Lb(2)=1
	Lb(3)=1
	if(makav.eq.1) Lb(4)=1	!zoom works only for single trace at present
	if(align.and.endalign) Lb(5)=1
	Lb(6)=1
	Lb(7)=1
	Lb(8)=1
	Lb(9)=1
c=	call DCMENU(0,5,Lb,text,0,0)		!delete all
	call FILLWIN(0,0,639,iyd1-2,ibk)	!fill DCmenu area with colour ibk
	if(sampv) then
	   if(curdisp) then
		text(1)='1.SHOW VOLTAGE'
	   else
		text(1)='1.SHOW CURRENT'
	   endif
	endif
	text(2)='2. REJECT   '
	text(3)='3. ACCEPT   '
	if(.not.zoomed) then
	   text(4)='4. ZOOM     '
	else
	   text(4)='4. UNZOOM   '
	endif
	if(align.and.endalign) then
	   text(5)='5. FIX DISPLAY'
	endif
	text(6)='6. FIX SCALE  '
	text(7)='7. FIX OFFSET '
	if(ndelt.gt.1) then
	   text(8)='8. ALL POINTS '
	else
	   text(8)='8. OMIT POINTS'
	endif
	text(9)='9. FINISH   '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c
c Initialise TOTCUR,TOTVOLT
	do i=1,nsamp
	   do j=1,ndc
		TOTCUR(i,j)=0.0
	   enddo
       enddo
	if(sampv.or.(align.and.endalign)) then
	  do i=1,nsamp
		do j=1,ndv
		   TOTVOLT(i,j)=0.0
		enddo
         enddo
	endif
c
c Invisible call to ARROWiv to define values used in VARROW (in REAVER3)
	if(align) then
	   iya1=iyd2-100
	   iya2=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre?
	   call ARROWiv(0,iya1,0,iya2,0.3,20,0)
	endif
c
	idef=1
	curdisp=.true.
	nmean=0		!number of sweeps accepted for average
c
	do 13 iswp=n1,n2			! start of loop for each event
	irec=istrec(iswp)
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
     &    ivhclamp,ipatch,ffilt,sampatch,0)
c (NB don't read nDv1 here as in orig, because this symbol used for array dim)
	sfac3=sfac1/calfac		!converts pA to pixel units
	sfac4=scalfac*sfac3
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
	sfac3=sfac1/calfac		!converts pA to pixel units
	sfac4=scalfac*sfac3
	if(iswp.gt.n1.and.(itsamp.ne.itsamp1.or.nsamp.ne.nsamp1.or.
     & (vjump.neqv.vjump1).or.(sampv.neqv.sampv1))) then		!===
c Note: neqv is logical 'not equal'
	   call INTCONV(iswp,cnum)
	   call INTCONV(n1,cnum1)
	   string='Parameters for the data sets'//charnb(cnum)//
     &	' and '//charnb(cnum1)//' do not match:'
	   call WDIALOG(1,charnb(string),ictd)
	   call BELL(3)
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
	   if(iopt.eq.1) goto 19
	   call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	   idest=6
	   goto 999		!back to REAVER1 with idest=6
	endif
	if(vjump.and.(control.neqv.control1)) then
	   call BELL(2)
	   call INTCONV(n1,cnum)
	   call INTCONV(iswp,cnum1)
	   if(control) then
	      string='Sweep '//charnb(cnum)//
     &	' was a control; this one (#'//charnb(cnum1)//') is NOT:'
	      call WDIALOG(1,charnb(string),ictd)
	   else
	      string='Sweep '//charnb(cnum)//
     &	' was NOT a control; this one (#'//charnb(cnum1)//') is:'
	      call WDIALOG(1,charnb(string),ictd)
	   endif
c
 	   call DEFOLTi(idef,defolt)
	   call WDIALOG(1,
     &	'(1) Reject this sweep; (2) Average it anyway;',11)
	   call QDIALOG(1,
     &	' (3) Restart the average',defolt,11,cans)
	   call GETINPi(cans,idef)
c
	   if(idef.eq.1) goto 19
	   if(idef.eq.3) then
		call VIDEOMOD(3)		!utility lib- this makes graph go for good!
		idest=6
		goto 999		!back to REAVER1 with idest=6
	   endif
	endif
	irec=istrec(iswp)			!read data set #iswp
c  Read data into Yval initially, to save rewriting READREL, but
c  copy it into TOTCUR, XDISP etc for display and averaging. Put
c  final average back into correct YVAL elements before leaving REAVER
c
	call READREL(irec,nsamp,iav,sampv,calfac,calvolt,
     &   ipatch,ivhclamp,Yval,jc,jv,ndv1,ndimd,ioffset,newform)
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
	i=0
	do i1=1,nsamp,ndelt
	   i=i+1		!in case ndelt.ne.1
	   iydisp(i)=int2(ifixr(YVAL(i1,jc)*sfac4)) + iyoff   !copy current
	enddo
	ndisp=i	!=number to display on exit
c
	if(sampv) then
	   i=0
	   do i1=1,nsamp,ndelt
	      i=i+1
	      iydispv(i)=int2(ifixr(YVAL(i1,jv)*sfac4)) + iyoff   !copy voltage
	   enddo
	else if(align.and.endalign) then
	   i=0
	   do i1=1,nsamp,ndelt
	      i=i+1
	      iydispv(i)=int2(ifixr(YVAL(i1,jc1)*sfac4)) + iyoff2
	   enddo
	endif
c
c Put average into display arrays (TOTCUR hold TOTALS)
	if(nmean.ge.1) then
	   en=float(nmean)
	   i=0
	   do i1=1,nsamp,ndelt
	   	i=i+1
	   	iyavc(i)=int2(ifixr(sfac4*TOTCUR(i1,jc)/en)) + iyoff1
	   enddo
	   if(sampv) then
		i=0
		do i1=1,nsamp,ndelt
	   	   i=i+1
		   iyavv(i)=int2(ifixr(sfac4*TOTVOLT(i1,jv)/en)) + iyoff1
		enddo
	   else if(align.and.endalign) then
		i=0
		do i1=1,nsamp,ndelt
	   	   i=i+1
		   iyavv(i)=int2(ifixr(sfac4*TOTVOLT(i1,jc1)/en)) + iyoff3
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
c
c Display data for current sweep
22	continue
c For aligned data, suppress initial display for null sweeps because cannot
c be shifted so final display only is needed
c NO -show null trace too
c NB tintlat() is now indexed same as cjump.dat, and values that have
c not been fitted in SCAN, or are not usable are labeled as undef(ined), bad
c or omit.
	if(align) then
c===	   ij=iswp-njump1+1		!=1,2,...,njumpt=index for tintlat()
	   ij=iswp				!=n1,..,n2=index for tintlat()
	   tin=tintlat(ij)
c   define times as defined at top of subroutine
c In original trace:
c 	activation starts at t=tfst=tzero + tintlat
c	activation ends at tlast=tsamp-tintend
c	length of activation tact = tlast-tfst
c When trace shifted to put first opening at tzero
c 	activation starts at t=tzero
c	activation ends at tzero+tact
c When trace shifted to put last shutting at tsamp-tzero
c 	activation starts at t=tsamp-tzero-tact
c	activation ends at tsamp-tzero
	   tfst=tzero + tintlat(ij)
	   if(endalign) then
		tac=tact(ij)
		tlast=tfst+tac
c==		tlast=tsamp - tintend(ij)
c==		tact=tlast - tfst
	   endif
	endif
c
	icol=10				!current sweep green
	if(mono) icol=15 			!bright white for monochrome
	call SETACT(iact,icol)		!draw green whatever
	do i=1,ndisp
	   jcol(i)=icol			!no need to use RPIXEL in IDCPIX
	enddo
c Initial display
c NB jcol() now stays set to green throughout so current trace
c can be displayed with imode=0 (faster as avoids reading exh pixel)
	setcol=.false.	!do not alter jcol() in call to IDCPIX
	if(curdisp) then
	   call IDCPIX3(ixdisp,iydisp,ndisp,iact,jcol,1,setcol)
	else
	   call IDCPIX3(ixdisp,iydispv,ndisp,iact,jcol,1,setcol)
	endif
c Display lines for position of conc pulse
c Also define x position of the jump (command pulse). Point # nAc is
c dnAc microsec before the start of the (first) cjump, and jump is
c ilenc(1)*0.001 ms long.  Sample is itsamp microsec long so
c dx pixel units represent itsamp*1.e-3/ndisp milliseconds
c irate=sample rate (Hz) so 1000/irate ms between points
c (values for current jump are nAc1 and dnAc1)
	finter=1.e6/float(irate) 	!microsec between sample points
	dnAx=dx*dnAc1/finter		!x (pixel units) for dnAc microsec
	dlenx=dx*float(ilenc(1))/finter !x (pixel units) for ilenc(1) microsec
	x0=float(nAc1-1)*dx + dnAx  !x value for start of conc pulse
	x01=x0 + dlenx
	ix0=ifixr(x0)
	ix01=ifixr(x01)
	call VLINE(ix0,iyd1+40,iyd2-20,14)	 !line at start of pulse
	call HLINE(ix0,ix01,iyd2-20,14)
	call VLINE(ix01,iyd1+40,iyd2-20,14)	 !line at end of pulse
c
c Display current average (for aligned data, show mean only on aligned trace)
	if(nmean.ge.1.and.(.not.align)) then
	   call SETACT(iact,-1)		!do not draw unless
	   iact(ibk1)=12			!draw red on background
	   iact(icol)=13			!draw purple if already green
	   if(curdisp) then
		call IDCPIX3(ixdisp,iyavc,ndisp,iact,jcol,0,setcol)
	   else
		call IDCPIX3(ixdisp,iyavv,ndisp,iact,jcol,0,setcol)
	   endif
	endif
c
23	if(align) then
c        Remove menu until fndcur completed
	   call FILLWIN(0,0,639,iyd1-2,ibk)	!fill DCmenu area with colour ibk
c===	   ij=iswp-njump1+1		!=1,2,...,njumpt=index for tintlat()
	   ij=iswp				!=n1,..,n2=index for tintlat()
	   njump=jumps(ij)
	   call INTCONV(njump,cnum)
	   call INTCONV(iswp,cnum1)
	   tin=tintlat(ij)
	   null=-1.5.le.tin.and.tin.le.-0.5
	   bad=-2.5.le.tin.and.tin.le.-1.5
	   omit=-3.5.le.tin.and.tin.le.-2.5
	   undef=-4.5.le.tin.and.tin.le.-3.5
	   zero=tin.eq.0.0
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   if(bad) then
		call BELL(2)
		call WDIALOG(1,
     &	'First latency is bad for sweep #'//charnb(cnum1)//
     &	': reject now',12)
	   else if(omit) then
		call BELL(2)
		call WDIALOG(1,
     &	    'Sweep #'//charnb(cnum1)//' is to omitted: reject now',
     &		12)
	   else if(undef) then
		call BELL(2)
		call WDIALOG(1,
     &		'Sweep #'//charnb(cnum1)//
     &	' has undefined latency: reject now',12)
	   else if(null) then
		call BELL(1)
		call WDIALOG(1,
     &		'Sweep #'//charnb(cnum1)//' is null',ictd)
	   else if(zero) then
		call BELL(1)
		call WDIALOG(1,
     &		'Sweep #'//charnb(cnum1)//' is open at t=0',ictd)
	   else
		call DCFORMAT(tintlat(ij),8,2,cnum1)
c Arrow to mark latency: dx pixel units represent itsamp*1.e-3/ndisp milliseconds
c so tintlat ms = tintlat*nsamp/(0.001*itsamp) pixel units (NB ndisp=nsamp
c if ndelt=1, but not otherwise)
		time=tintlat(ij) + tzero
		indfst0=1+ifixr((time/tsamp)*float(ndisp))	!index in ixdisp
		ix1=ixdisp(indfst0)
		call VARROW(ix1,iya1,iya2,14)
		call WDIALOG(1,'First latency for jump #'//charnb(cnum)//
     &    	   ' = '//charnb(cnum1),ictd)
	   endif
c  Now mark bit of baseline to fill the end after shifting
c (no shift needed if latency=0 or null sweep if bad, then offer option
c to accept anyway)
	   skip=null.or.zero.or.omit.or.undef
	   if(.not.skip) then
		call WDIALOG(1,
     & 'Mark start of baseline section (to fill end after shift)',ictd)
		ix=7000
		iy=7000
		call PUTCUR(ix,iy)
		ch=char(FNDCUR(ix1,iy1))	!call SCALE gives ix1 in ms
	      x1=XWORLD(ix1)
		iltype=2       !dash
		call MOVETO(x1,yd1,0,iltype)
		call MOVETO(x1,yd2,1,iltype)
		time=x1           !in pixels -640 pixels=nsamp points
		index1=1+ifixr((time/640.)*float(nsamp))	!index in Yval
		call WDIALOG(1,
     & 'Mark end of baseline section',ictd)
		ix=9000
		iy=7000
		call PUTCUR(ix,iy)
		ch=char(FNDCUR(ix1,iy1))	!call SCALE gives ix1 in ms
	      x1=XWORLD(ix1)
		iltype=7       !dash-dot
		call MOVETO(x1,yd1,0,iltype)
		call MOVETO(x1,yd2,1,iltype)
		time=x1
		index2=1+ifixr((time/640.)*float(nsamp))	!index in Yval
c Now make new display array with shifted data.  Start with point that
c corresponds with latency-tzero (so still have points tzero ms before jump)
c so if latency=0 than first point on display is yval(1,jc).  Need to redefine
c Yval(), because this is what is averaged, then make new iydisp().
c
c NB if we are also looking at activations aligned on last shutting
c then make shifted array for them FIRST because this is done in
c Yval(i,jc1) so does not alter the original trace in Yval(i,jc) (if
c align on first opening is done first, this alters Yval(i,jc)).
c In this case use first tsamp points as fill because they will be at
c the right level (if there is baseline drift using points from end
c gives a glitch)
		nf=max(nAc1,index2-index1+1)
		ALLOCATE(yfill(nf))
		srate=float(irate)
		dts=1000./srate	!ms between points
		if(endalign) then
		   nfill=nAc1		!redefine fill
		   do i=1,nfill
			yfill(i)=Yval(i,jc)
		   enddo
c        put points from t=0 to t=tzero at end of array (last nAc1 points)
		   i=nsamp+1
		   do i1=1,nAc1
			i=i-1
			Yval(i,jc1)=Yval(i1,jc)
		   enddo
		   nd1=i	!last value written
c    next put the activation, with points in normal time order, but shifted right
		   ilast=1 + ifixr(tlast/dts)		!=1 for tlast=0
c         now write all points from ilast down to 1st
		   i=nd1
	         do i1=ilast,1,-1
			i=i-1
			Yval(i,jc1)=Yval(i1,jc)		!trace in jc1
		   enddo
		   nd1=i
c        and fill any space left at the start of array
		   if(nd1.gt.1) then
			i=nd1		!next yval to be filled
			i1=0		!index for Yfill
			do while(i.gt.1)
			   i=i-1
			   i1=i1+1
			   if(i1.gt.nfill) i1=1	!reset
			   Yval(i,jc1)=Yfill(i1)
			enddo
		   endif
c             display array for 'aligned on last transition'
		   i=0
		   do i1=1,nsamp,ndelt
			i=i+1		!in case ndelt.ne.1
			iydispv(i)=int2(ifixr(YVAL(i1,jc1)*sfac4)) + iyoff2
		   enddo
		endif		!end of 'if(endalign)'
c
c Now shift to align on first opening
c Copy marked bit of baseline to be used to fill ends into a temporary
c array, yfill().
		nfill=index2-index1+1	!length of chunk for filling
		do i=1,nfill
		   yfill(i)=Yval(i+index1-1,jc)
		enddo
		ifst=1 + ifixr(tintlat(ij)/dts)	!=1 for latency=0
		i=0
		do i1=ifst,nsamp
		   i=i+1
		   Yval(i,jc)=Yval(i1,jc)
		enddo
		nd1=i		!=number of points defined up to end of sample
c Now fill the rest with baseline data defined above
		if(nd1.lt.nsamp) then
		   i=nd1		!next yval to be filled
		   i1=0		!index for Yfill
		   do while(i.lt.nsamp)
			i=i+1
			i1=i1+1
			if(i1.gt.nfill) i1=1	!reset
			Yval(i,jc)=Yfill(i1)
		   enddo
		endif
c Yval now shifted and filled -redefine iydisp()
		i=0
		do i1=1,nsamp,ndelt
		   i=i+1		!in case ndelt.ne.1
		   iydisp(i)=int2(ifixr(YVAL(i1,jc)*sfac4)) + iyoff   !copy current
		enddo
c
		DEALLOCATE(yfill)
c Yval now shifted and filled -redefine iydisp()
	   endif	!end of 'not null'
	endif
c Split the 'if(align)' section here so can jump to 200 for redraw after unzoom
200	   continue		!return after unzoom to redraw
	if(align) then
c Erase last trace
	   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
c Restore DCMENU
	   call DCMENU(nbox,5,Lb,text,icol1,icol2)
c Display new one (if null or zero then Yval() and iydisp() are unchanged
	   if(endalign.and.(.not.skip)) then
		call SETACT(iact,icol)		!draw green whatever
		if(idrev.eq.1) then
		   call SETACT(iact,10)		!draw green
		   call IDCPIX3(ixdisp,iydisp,ndisp,iact,jcol,1,setcol)
		else if(idrev.eq.2) then
		   iact(icol)=11			!draw blue
		   call SETACT(iact,11)		!draw blue
		   call IDCPIX3(ixdisp,iydispv,ndisp,iact,jcol,1,setcol)
		else if(idrev.eq.3) then
		   call SETACT(iact,10)		!draw green
		   call IDCPIX3(ixdisp,iydisp,ndisp,iact,jcol,1,setcol)
		   call SETACT(iact,-1)		!do not draw unless
		   iact(ibk1)=11			!draw blue on background
		   iact(icol)=3			!draw dark blue if already green
		   call IDCPIX3(ixdisp,iydispv,ndisp,iact,jcol,0,setcol)
		endif
c arrow at point where ends aligned
	      time=tsamp-tzero
	      indlast=1+ifixr((time/tsamp)*float(ndisp))
	      ix1=ixdisp(indlast)
	      call VARROW(ix1,iya1,iya2,15)
c for debug draw line whole height of window
		call VLINE(ix1,iyd1+10,iyd2-10,15)
c  and arrow at the start of this activation
	      time=tsamp-tzero-tac
	      indlast1=1+ifixr((time/tsamp)*float(ndisp))
	      ix1=ixdisp(indlast1)
	      call VARROW(ix1,iya1,iya2,15)
c for debug draw line whole height of window
		call VLINE(ix1,iyd1+10,iyd2-10,15)
	   else
		call IDCPIX3(ixdisp,iydisp,ndisp,iact,jcol,1,setcol)	!green
	   endif
c  Draw arrow at tzero
	   time=tzero
	   indfst=1+ifixr((time/tsamp)*float(ndisp))
	   ix1=ixdisp(indfst)
	   call VARROW(ix1,iya1,iya2,14)
c for debug draw line whole height of window
	   call VLINE(ix1,iyd1+10,iyd2-10,14)
c and arrow at end of activation
	   time=tzero+tac
	   indfst1=1+ifixr((time/tsamp)*float(ndisp))
	   ix1=ixdisp(indfst1)
	   call VARROW(ix1,iya1,iya2,14)
c for debug draw line whole height of window
	   call VLINE(ix1,iyd1+10,iyd2-10,14)
c And display current mean of shifted traces
	   if(nmean.ge.1) then
		if(idrev.eq.1) then
		   call SETACT(iact,-1)		!do not draw unless
		   iact(ibk1)=12			!draw red on background
		   iact(icol)=6			!draw brown if already green
		   iact(13)=6			!draw brown if already purple
		   call IDCPIX3(ixdisp,iyavc,ndisp,iact,jcol,0,setcol)
		else if(idrev.eq.2) then
		   call SETACT(iact,-1)		!do not draw unless
		   iact(ibk1)=13			!draw purple on background
		   iact(icol)=6			!draw brown if already green
		   iact(12)=6			!draw brown if already red
		   call IDCPIX3(ixdisp,iyavv,ndisp,iact,jcol,0,setcol)
		else if(idrev.eq.3) then
		   call SETACT(iact,-1)		!do not draw unless
		   iact(ibk1)=12			!draw red on background
		   iact(icol)=6			!draw brown if already green
		   iact(13)=6			!draw brown if already purple
		   call IDCPIX3(ixdisp,iyavc,ndisp,iact,jcol,0,setcol)
		   call SETACT(iact,-1)		!do not draw unless
		   iact(ibk1)=13			!draw purple on background
		   iact(icol)=6			!draw brown if already green
		   iact(12)=6			!draw brown if already red
		   call IDCPIX3(ixdisp,iyavv,ndisp,iact,jcol,0,setcol)
		endif
	   endif
	endif		!end of 'if align'
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
c Accept
	if(ikey.eq.3) then
	   if(align.and.(bad.or.omit.or.undef)) goto 22
	   if(pon()) write(7,211)
	   if(discprt) write(8,211)
211	   format(' Accepted')
c
	   nmean=nmean+1
c
c	  Allocate TOTCUR as above, except ndelt=1 (average all nsamp points)
	   do i=1,nsamp
		TOTCUR(i,jc)=TOTCUR(i,jc)+yval(i,jc)
	   enddo
	   if(sampv) then
		do i=1,nsamp
		   TOTVOLT(i,jv)=TOTVOLT(i,jv)+YVAL(i,jv)
		enddo
	   else if(align.and.endalign) then
		do i=1,nsamp
		   TOTVOLT(i,jc1)=TOTVOLT(i,jc1)+YVAL(i,jc1)
		enddo
	   endif
c Can accept/reject while zoomed, but reset here for next trace
	   zoomed=.false.
	   call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
	   text(4)='4.  ZOOM    '
	   call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	   goto 19		!next event
	endif
c
c Reject
	if(ikey.eq.2) then
	   if(pon()) write(7,210)
	   if(discprt) write(8,210)
210	   format(' Rejected')
c Can accept/reject while zoomed, but reset here for next trace
	   zoomed=.false.
	   call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
	   text(4)='4.  ZOOM    '
	   call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	   goto 19		!next event
	endif
c
c  Zoom/unzoom -put new data into special
c allocated array so original data unchanged
	if(ikey.eq.4) then
	 if(zoomed) then		!unzoom
	   zoomed=.false.
	   call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
	   text(4)='4.  ZOOM    '
	   call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
c      erase and redraw original
	   call WDIALOG(1,'Now accept, reject or finish',ictd)
	   goto 200		!redraw
c
	 else		!zoom
c
	   zoomed=.true.
	   call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
	   text(4)='4.  UNZOOM   '
	   call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	   call WDIALOG(1,
     & 'Mark start of section to be zoomed',ictd)
c put up cursors in same place as used for last zoom
c	   ix=5000
	   iy=7400
	   call PUTCUR(ixz1,iy)
	   ch=char(FNDCUR(ix1,iy1))	!call SCALE gives ix1 in ms
	   x1=XWORLD(ix1)
	   ixz1=ix1		!for next default
	   iltype=7       !dash-dot
	   call MOVETO(x1,yd1,0,iltype)
	   call MOVETO(x1,yd2,1,iltype)
	   time=x1           !in pixels -640 pixels=nsamp points
	   index1=1 + ifixr((time/640.)*float(ndisp))	!index in iydisp
	   if(index1.lt.1) index1=1
	   call WDIALOG(1,
     & 'Mark end of section to be zoomed',ictd)
12	   continue
c	   ix=9000
	   iy=7400
	   if(ixz2.lt.ixz1) ixz2=ixz1+50
	   call PUTCUR(ixz2,iy)
	   ch=char(FNDCUR(ix1,iy1))	!call SCALE gives ix1 in ms
	   ixz2=ix1		!for next default
	   x2=XWORLD(ix1)
	   if(x2.lt.x1) then
		x=x2
		x2=x1
		x1=x
c		call BELL(1)
c		goto 12
	   endif
	   iltype=7       !dash-dot
	   call MOVETO(x2,yd1,0,iltype)
	   call MOVETO(x2,yd2,1,iltype)
	   time=x2
	   index2=1 + ifixr((time/640.)*float(ndisp))	!index in iydisp
	   if(index2.gt.ndisp) index2=ndisp
	   nzoom=index2-index1+1	!length of chunk for filling
	   ALLOCATE(ixdispz(nzoom),iydispz(nzoom))
	   dx=(640.)/float(nzoom-1)
	   i1=0
	   do i=1,nzoom
	 	x=float(i-1)*dx
		ixdispz(i)=int2(ifixr(x))
		iydispz(i)=iydisp(i+index1-1)
	   enddo
c	    erase data
	   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
c         draw zoomed data
	   icol=10
	   if(mono) icol=15 			!bright white for monochrome
	   call SETACT(iact,icol)			!draw green whatever
	   if(.not.(align.and.endalign)) then
	      call IDCPIX3(ixdispz,iydispz,nzoom,iact,jcol,1,setcol)
	   else if(idrev.eq.1.or.idrev.eq.3) then
	      call IDCPIX3(ixdispz,iydispz,nzoom,iact,jcol,1,setcol)
	   endif
c	 draw arrow at start of act, if on screen
	   ind1=indfst-index1+1
	   if(align.and.indfst.ge.index1.and.indfst.le.index2) then
		ix1=ixdispz(ind1)
		call VARROW(ix1,iya1,iya2,14)
	   endif
c	 draw arrow at end of act, if on screen
	   ind1=indfst1-index1+1
	   if(align.and.indfst1.ge.index1.and.indfst1.le.index2) then
		ix1=ixdispz(ind1)
		call VARROW(ix1,iya1,iya2,14)
	   endif
	   if(align.and.endalign.and.idrev.ge.2) then	!zoom other trace!
	      i1=0
	      do i=1,nzoom
		   iydispz(i)=iydispv(i+index1-1)	!use same array for other trace?
		enddo
		call SETACT(iact,-1)		!do not draw unless
		iact(ibk1)=11			!draw blue on background
		iact(icol)=3			!draw dark blue if already green
		call IDCPIX3(ixdispz,iydispz,nzoom,iact,jcol,0,setcol)
		ind2=indlast-index1+1
	 	if(indlast.ge.index1.and.indlast.le.index2) then
		   ix1=ixdispz(ind2)
		   call VARROW(ix1,iya1,iya2,15)
		endif
		ind2=indlast1-index1+1
	 	if(indlast1.ge.index1.and.indlast1.le.index2) then
		   ix1=ixdispz(ind2)
		   call VARROW(ix1,iya1,iya2,15)
		endif
	   endif
	   DEALLOCATE(ixdispz,iydispz)
	   call WDIALOG(1,'Now accept, reject or finish',ictd)
	   goto 60
	 endif
	endif
c
c Control display for jumps aligned on last transition
c idrev=1 when both only traces aligned on start shown
c idrev=2 when both only traces aligned on end shown
c idrev=3 when both traces (aligned on start and aligned on end) shown
	if(ikey.eq.5) then
 	   call DEFOLTi(idrev,defolt)
	   call QDIALOG(1,
     &	'Show traces aligned on (1) start, (2) end, (3) both'
     &	,defolt,11,cans)
	   call GETINPi(cans,idrev)
	   if(align) goto 200
	   goto 22		!draw new display
	endif
c
C Fix scale
	if(ikey.eq.6) then
 	   call DEFOLTr(scalfac,defolt)
	   call QDIALOG(1,
     &	'Scale factor for display of current trace'
     &	,defolt,ictd,cans)
	   call GETINPr(cans,scalfac)
	   sfac4=sfac3*scalfac
	   goto 60
	endif
c Fix offset
	if(ikey.eq.7) then
 	   call DEFOLTi(iyoff,defolt)
	   call QDIALOG(1,
     &	'Vertical offset for display of current trace'
     &	,defolt,ictd,cans)
	   call GETINPi(cans,iyoff)
 	   call DEFOLTi(iyoff1,defolt)
	   call QDIALOG(1,
     &	'Vertical offset for display of mean trace'
     &	,defolt,ictd,cans)
	   call GETINPi(cans,iyoff1)
	   call WDIALOG(1,'Now accept, reject or finish',ictd)
	   iyoff2=iyoff+40   		!offset for trace aligned on end
	   iyoff3=iyoff1+40  		!offset for mean aligned on end
	   goto 60
	endif
c Swap current/voltage displays
	if(ikey.eq.1.and.sampv) then
c   delete the previous display
c -just fill data area to clear
	   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
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
	if(ikey.eq.8) then	!CHANGE ndelt
         call INTCONV(nsamp,cnum)
	   n=NBLANK(cnum)
 	   call DEFOLTi(ndelt,defolt)
	   call QDIALOG(1,
     &	'Sample = '//cnum(1:n)//' points: plot every nth'
     &	,defolt,12,cans)
	   call GETINPi(cans,ndelt)
c
c  recalculate the X array
	 if(keepall) then
	   dx=(639.)/float(nsamp)
	   i1=0
	   do i=1,nsamp,ndelt
	    x=float(i-1)*dx
	    i1=i1+1
	    ixdisp(i1)=int2(ifixr(x))
	   enddo
	 else
	   dx=(639.)/float(nsamp0)
	   j=0
	   do i=1,nsamp0
	    if(jmask(i).ne.0) then	!=0 for skipped point
	     x=float(i-1)*dx			!=msec from 0 to itsamp-1
	     j=j+1
	     ixdisp(j)=int2(ifixr(x))
	    endif
	   enddo
	 endif
c Recalculate all ydisp arrays
	i=0
	do i1=1,nsamp,ndelt
	   i=i+1		!in case ndelt.ne.1
	   iydisp(i)=int2(ifixr(YVAL(i1,jc)*sfac4)) + iyoff   !copy current
	enddo
	ndisp=i	!=number to display on exit
	if(sampv) then
	   i=0
	   do i1=1,nsamp,ndelt
	      i=i+1
	      iydispv(i)=int2(ifixr(YVAL(i1,jv)*sfac4)) + iyoff   !copy voltage
	   enddo
	else if(align.and.endalign) then
	   i=0
	   do i1=1,nsamp,ndelt
	      i=i+1
	      iydispv(i)=int2(ifixr(YVAL(i1,jc1)*sfac4)) + iyoff2
	   enddo
	endif
c
c Put average into display arrays (TOTCUR hold TOTALS)
	if(nmean.ge.1) then
	   en=float(nmean)
	   i=0
	   do i1=1,nsamp,ndelt
	   	i=i+1
	   	iyavc(i)=int2(ifixr(sfac4*TOTCUR(i1,jc)/en)) + iyoff1
	   enddo
	   if(sampv) then
		i=0
		do i1=1,nsamp,ndelt
	   	   i=i+1
		   iyavv(i)=int2(ifixr(sfac4*TOTVOLT(i1,jv)/en)) + iyoff1
		enddo
	   else if(align.and.endalign) then
		i=0
		do i1=1,nsamp,ndelt
	   	   i=i+1
		   iyavv(i)=int2(ifixr(sfac4*TOTVOLT(i1,jc1)/en)) + iyoff3
		enddo
	   endif
	endif
c===
	   if(ndelt.gt.1) then
		text(8)='8. ALL POINTS '
	   else
		text(8)='8. OMIT POINTS'
	   endif
	   call DCMENU(-8,5,Lb,text,0,0)	!delete box 8 only
	   call DCMENU(-8,5,Lb,text,icol1,icol2)	!draw box 8 only
	   call WDIALOG(1,'Now accept, reject or finish',ictd)
	   goto 200			!redisplay same traces with new ndelt
	endif
c
	goto 60		!get another character if none of above done
c
19	continue
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
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
	do i=1,nsamp
	   YVAL(i,jc)=TOTCUR(i,jc)/en
	enddo
c
	if(sampv) then
	   do i=1,nsamp
	      YVAL(i,jv)=TOTVOLT(i,jv)/en
	   enddo
	else if(align.and.endalign) then
	   do i=1,nsamp
	      YVAL(i,jc1)=TOTVOLT(i,jc1)/en
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

