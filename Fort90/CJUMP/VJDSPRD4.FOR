c===================================================================

	subroutine VJDSPRD4(ndev,kmax,datfil,ibox,icwd,icqd)

c===================================================================
c
c VJDSPRD4 needs kmax as param (- needed for calns2 -also make arguments
c into parameters so can declare data arrays as kmax)
c
c Modif 07/12/91 11:58am so dimension of AVCUR now kmax2, and YDISP,
c Xdisp now 20480 (if memory, or speed problems display only every nth point)
c
c VJDSPRD3 is version of VJDSPRD2 for CJUMP3
c VJDISPRD is version of VJDISP in which jumps plotted are not the
c current ones, but are read from disc. This is sep routine, without
c AVCUR etc as params, so that values read from disc do not overwrite
c the present ones in main prog.
c
c To plot
c (1)net voltage jump (I vs t); (2)net voltage ramp, I vs V (in ADC1)
c (3) V vs t only
c
c	At present current scale (Y axis)=full scale of 1401 for both
c V-jump and I/V
c
c	Note that AVOLT=voltages for drug, and AVOLTCON=voltages for control,
c should be same so average them, and plot each current value
c against the same potential (otherwise subtraction of control from drug
c would be a problem)
c
c These arrays all local in VJDISPRD:
	ALLOCATABLE :: xdisp,ydisp,avcur,avcurcon,avolt,avoltcon
	allocatable :: jmask,iadc,iadc1
	integer*2 iADC(:),iADC1(:)
	integer*2 istrec2(250)		!=500 bytes (kept as int*2 to fit in 1 rec)
	integer*4 istrec(1000)		! ne version
	integer*1 jmask(:)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ipatch,ivhclamp
	integer ifst(10),ilast(10)
	integer itype(10)
	integer*2 iver1,kstep(5),videotyp
	integer*4 ikeep1(4,2),ikeep(4,2,30)
	real*4    	avcur(:),avcurcon(:)		!average current (drug,control)
	real*4    	avolt(:),avoltcon(:)	!average voltage (drug,control)
	real*4    	xdisp(:),ydisp(:,:)
	real*4 swval(30) 		!values that change between sweeps
	character title1*79,ccal*11,ccal1*11,unit*4,titlew*15
      character cdate*11,ctime*11,ndev*2,datfil*24,getch
      character ans,UC,getint*11,cans*30,def*30,charout
	character*50 que(5),helps(3)
c      character ci*11,cx*20,cy*20		!for debug
	logical vjump,sampv,control,control1,keepall,onejump,alpha
	logical discprt,debug,deb,caplock
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
	allocate(avcur(kmax),avcurcon(kmax),avolt(kmax),avoltcon(kmax))
	allocate(jmask(kmax),iadc(kmax),iadc1(kmax))
	ixlow=200
	iyhiw=200
	iylow=-1
	ic=15
	icf=8
	icup=15
	ibkw=4
	nhelp=1
	titlew='OPTIONS'
c
4	format(i8)
101	format(a1)
	nch=11
c
c Section to read data back from CJUMP.DAT for display
c	datfil=ndev//'\CJUMP.DAT'
      OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
c NB recl is in bytes
c==	read(14,rec=1) njd,nxtrec,ISTREC
c Modifs for new cjump.dat format.
	 read(14,rec=1) njd
	 if(njd.lt.1) then
	    CLOSE(unit=14)
c          Re-open in correct format
	    OPEN(unit=14,file=datfil,status='UNKNOWN',
     &	 access='TRANSPARENT')
	    read(14,rec=1) jver,njd,nxtrec,ioffset,(ISTREC(i),i=1,njd)
	 else
	    read(14,rec=1) njd,nxtrec,(ISTREC2(i),i=1,250)
c Convert the integer*2 istrec2() into integer *4 for all subsequent use
	    do i=1,njd
		rec=int4(istrec2(i))
		if(irec.le.-1.and.irec.ge.-32768) then
		   irec=irec+65536
		else if(irec.lt.-32768) then
		   print*,' IREC IS TOO BIG!!'
		   STOP
		endif
		istrec(i)=irec
	    enddo
	endif
	    irec=istrec(njd)
c
	call intconv(njd,getint)
	nm=nblank1(getint)
	call wdialog(ibox,getint(1:nm)//' sweeps on disc : '
     &//ndev,icwd)

59	continue
	if(alpha) then
	   print 62,njd,ndev
62	   format(' ',i4,' sweeps on disc ',a2,/,
     &   ' [0] No (more) listing',/,
     &   ' (1) List brief details only',/,
     &   ' (2) List full details ',/,
     &   ' Option number [0] = ')
	   read 4,iopt
	   if(iopt.eq.0) goto 61
	else
	   if(first.eq.0) ans=getch(b)
	   first=1
	   jm=1
	   que(1)='(1) No (more) listing'
	   que(2)='(2) List details '
	   que(3)='(3) Exit '
	   nhelp=1
	   nopt=3
	   call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &   titlew,helps,nhelp,jm,charout,ival)
	   if(jm.eq.1) goto 61
	   if(jm.eq.3) then
      	CLOSE(unit=14)
		RETURN
	   endif
	endif

67	continue
	if(alpha) then
	   print 60
60	   format(' List for sweeps n1 to n2:  n1 (,n2) = ')
	   read 63,n1,n2
63	   format(2i8)
	else
	    n1=0
	    n2=0
	    call defolt2i(n1,n2,def)
	    call qdialog(ibox,' List for sweeps n1 to n2:  n1 (,n2)',def,
     &    icqd,cans)
	    call getinp2i(cans,n1,n2)
	endif

	if(n1.eq.0) goto 59

	if(n2.eq.0) n2=n1
	do 64 i=n1,n2
	   irec=istrec(i)
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &   vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &   ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     &   ivhclamp,ipatch,ffilt
	   if(debug()) then
	     print 707,cdate,ctime,title1,naver,navc,iav,control,
     &     vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilenc,igapc,nDv,iDd,nkeep,kstep,(ikeep1(i1,1),i1=1,4),
     &     (ikeep1(i1,2),i1=1,4)
	     write(8,707)cdate,ctime,title1,naver,navc,iav,control,
     &     vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilenc,igapc,nDv,iDd,nkeep,kstep,(ikeep1(i1,1),i1=1,4),
     &     (ikeep1(i1,2),i1=1,4)
	   endif
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &   ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
c Define nsamp1,jmask,keepall
	   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &   kmax,keepall)
	   call PREC(i,title1,cdate,ctime,naver,navc,iav,vjump,
     &   control,1,ibox,icwd)
c To print values for a single sweep, set ikeep(i,j,1)=ikeep1(i,j) and
c call PPAR2 with nsweep=1
	   ns=1
	   do 54 i1=1,4
	  	ikeep(i1,1,1)=ikeep1(i1,1)
	  	ikeep(i1,2,1)=ikeep1(i1,2)
54	   continue
	   if (alpha) then
	   	call PPAR2(iopt,0,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   	calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & 	vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & 	amVpA1,ftape,gain,ns,swtime,ismode,swval,nkeep,ikeep,kstep,
     & 	jkeep,nsamp1,tkpre,tkpost)
	   else
		que(1)='1. List more'
		que(2)='2. No more listing'
		nopt=2
		ir=1
		call PPAR4(cdate,ctime,iTSAMP,iTPREc,nsamp,iDd,
     & 	calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & 	vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & 	amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & 	jkeep,nsamp1,tkpre,tkpost,que,nopt,ir,-1,title1,i,
     &      ipatch,ivhclamp,ffilt)
		if(ir.gt.0.and.ir.lt.3) iopt=ir
		if(iopt.eq.2) goto 61
		if(i.eq.n2) goto 59
	   endif
64	continue
	if(alpha) goto 59	!list more?
c===========================================================================

61	continue
	call intconv(njd,getint)
	nm=nblank1(getint)
	if(alpha) then
	   print 68,njd
68	   format(' Number of records on disc = ',i5,/,
     &   ' To show single sweep specify its number; to show difference',/,
     &   ' between two sweeps (drug and control) specify both numbers',/,
     &   ' Read sweeps:  n1 (,n2) [more details] =  ')
	   read 63,nj1,nj2
	else
	   call wdialog(ibox,'Number of records on disc = '//
     &   getint(1:nm),icwd)
         call wdialog(ibox,
     &'  To show single sweep specify its number; to show difference'
     &   ,icwd)
         call wdialog(ibox,
     &'  between two sweeps (drug and control) specify both numbers'
     &   ,icwd)
	    nj1=0
	    nj2=0
	    call defolt2i(nj1,nj2,def)
	    call qdialog(ibox,' Read sweeps :  n1 (,n2)',def,
     &    icqd,cans)
	    call getinp2i(cans,nj1,nj2)
	endif
	if(nj1.le.0) then
	   call wdialog(ibox,
     &   'Check which record is control (and make it #nj1)',icwd)
	   goto 67
	endif
	if(nj2.gt.0) then
	   irec=istrec(nj1)
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &	vjump
	   call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     &	control,1,ibox,icwd)
	   irec=istrec(nj2)
	   read(14,rec=irec) cdate,ctime,title1,naver1,navc1,iav,control1,
     &	vjump
	   call PREC(nj2,title1,cdate,ctime,naver1,navc1,iav,vjump,
     &	control1,1,ibox,icwd)
	   if(control.and.(.not.control1)) then
		ncon=1		!#nj1 is control
	   else if(control1.and.(.not.control)) then
		ncon=2		!#nj2 is control
	   else if(control.and.control1) then
		call BELL(2)
		if(alpha) then
		print 130
130		format(' Both records are controls O.K. [N] ? ')
		read 101,ans
		else
		   ans='N'
		   call defolta(ans,def)
		   call qdialog(ibox,'Both records are controls : OK ?',
     &	   def,icqd,cans)
		   call getinpa(cans,ans)
		endif
		if(UC(ans).ne.'Y') goto 59	!read again
		ncon=1		!#nj1 is control
		if(alpha) then
		   print 132
132		   format('&For net trace subtract #1 from #2 [Y] ? ')
		   read 101,ans
		else
		   ans='Y'
		   call defolta(ans,def)
		   call qdialog(ibox,'&For net trace subtract #1 from #2',
     &	   def,icqd,cans)
		   call getinpa(cans,ans)
		endif
		if(UC(ans).eq.'N') ncon=2
	   else if((.not.control).and.(.not.control1)) then
		call BELL(2)
		if(alpha) then
		   print 131
131		   format(' Neither record is a control O.K. [N] ? ')
		   read 101,ans
		else
		   ans='Y'
		   call defolta(ans,def)
		   call qdialog(ibox,'Neither record is a control : O.K. ? ',
     &	   def,icqd,cans)
		   call getinpa(cans,ans)
		endif
		if(UC(ans).ne.'Y') goto 59	!read again
		ncon=1		!#nj1 is control
		if(alpha) then
		   print 132
		   read 101,ans
		else
		   ans='Y'
		   call defolta(ans,def)
		   call qdialog(ibox,'&For net trace subtract #1 from #2',
     &	   def,icqd,cans)
		   call getinpa(cans,ans)
		endif
		if(UC(ans).eq.'N') ncon=2
	   endif
c   Make nj1 the control
	   if(ncon.eq.2) then
		n=nj1
		nj1=nj2
		nj2=n
	   endif
	endif
c

	if(debug()) then
	   print 705,nj1,nj2
705	   format(' control = ',i5,': drug = ',i5)
	   pause
	endif
c
c Read 'control':
c NB if nj2=0, so only one trace is read in, the data is in AVCURCON,AVOLTCON
c whether it is control (CONTROL=true) or drug
c NOTE: Iav=0 (whatever naver/navc) means that
c iADC follows header; Iav=1 (naver/navc>1) means that AVCUR() follows header;
c then data that follows is twice as long (=AVCUR(i)=real*4)
	irec=istrec(nj1)
	read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     & ivhclamp,ipatch,ffilt
	call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	dA=1.e6/float(irate)	!# of microsec between ADC samples
c Define nsamp1,jmask,keepall
	call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     & kmax,keepall)
	if(debug()) then
	   print 707,cdate,ctime,title1,naver,navc,iav,control,
     &  vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,
     &  ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &  ncjump,ilenc,igapc,nDv,iDd,nkeep,kstep,(ikeep1(i1,1),i1=1,4),
     &  (ikeep1(i1,2),i1=1,4)
707	   format(1x,2a11,/,1x,a79,/,3i8,3l4,/,3i8,g12.5,2i8,/,
     &  10i6,/,10i6,/,10i6,/,10i6,/,2g12.5,4i6,g12.5,i6,/,
     &  10i6,/,10i6,/,2i8,/,i8,5x,5i3,/,4i5,/,4i5)
	   pause
	endif
c
c	nmean=navc				!normally
c	if(.not.control) nmean=naver	!e.g. if both records are 'agonist'
	nrec=1 + (nsamp1-1)/256		!for iADC(i)  =int*2
	if(iav.eq.1) nrec=2*nrec	!for AVCUR(i) =real*4
c	if(nmean.gt.1) nrec=2*nrec	!for AVCURCON(i) =real*4
c
c	if(nmean.eq.1) then
	if(iav.eq.0) then
	   n=1
	   do 501 j=1,nrec
	   m=n+255
	   irec=irec+1
	   read(14,rec=irec) (iADC(i),i=n,m)
	   n=n+256
501	   continue
	else
	   n=1
	   do 502 j=1,nrec
	   m=n+127
	   irec=irec+1
	   read(14,rec=irec) (AVCURCON(i),i=n,m)
	   n=n+128
502	   continue
	endif
c
c If IADC() read copy it to AVCUR for plotting (or it would be overwritten
c by the control iADC() read in below)
c	if(nmean.eq.1) then
	if(iav.eq.0) then
	   do 74 i=1,nsamp1
		AVCURCON(i)=float(iADC(i))
74	   continue
	endif
c
c If voltage sampled, read it too
	if(.not.sampv) goto 71
	if(iav.eq.0) then
	   n=1
	   do 503 j=1,nrec
	   m=n+255
	   irec=irec+1
	   read(14,rec=irec) (iADC1(i),i=n,m)
	   n=n+256
503	   continue
	else
	   n=1
	   do 504 j=1,nrec
	   m=n+127
	   irec=irec+1
	   read(14,rec=irec) (AVOLTCON(i),i=n,m)
	   n=n+128
504	   continue
	endif
c
c If IADC1() read copy it to AVOLT for plotting (or it would be overwritten
c by the control iADC1() read in below)
	if(iav.eq.0) then
	   do 76 i=1,nsamp1
		AVOLTCON(i)=float(iADC1(i))
76	   continue
	endif
c
c
71	continue
c Now read 'drug'
	if(nj2.le.0) goto 73
	irec=istrec(nj2)
	   read(14,rec=irec)cdate,ctime,title1,naver1,navc1,iav1,control1,
     &  vjump,sampv,nsamp2,itsamp1,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate1,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &   ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,kmax,
     &   ivhclamp,ipatch,ffilt


	call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
c Define nsamp1,jmask,keepall
	   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &	 kmax,keepall)
c Test a few things to check samples comparable
	if(itsamp.ne.itsamp1.or.irate.ne.irate1.or.nsamp.ne.nsamp2) then
	   call BELL(3)
	   if(alpha) then
	   print 20
20	   format(' Parameters for the 2 data sets do not match')
	   pause
	   else
	      call wdialog(ibox,
     &      ' Parameters for the 2 data sets do not match',icwd)
		ans=getch(b)
	   endif
	   RETURN
	endif
c	nmean=naver1			!normally
c	if(control1) nmean=navc1	!e.g. if both records are 'control'
	dA=1.e6/float(irate)	!# of microsec between ADC samples
c
	nrec=1 + (nsamp1-1)/256		!for iADC(i)  =int*2
c	if(nmean.gt.1) nrec=2*nrec	!for AVCUR(i) =real*4
	if(iav1.eq.1) nrec=2*nrec	!for AVCUR(i) =real*4
c
	if(iav1.eq.0) then
	   n=1
	   do 401 j=1,nrec
	   m=n+255
	   irec=irec+1
	   read(14,rec=irec) (iADC(i),i=n,m)
	   n=n+256
401	   continue
	else
	   n=1
	   do 402 j=1,nrec
	   m=n+127
	   irec=irec+1
	   read(14,rec=irec) (AVCUR(i),i=n,m)
	   n=n+128
402	   continue
	endif
c If IADC() read copy it to AVCUR for plotting
	if(iav1.eq.0) then
	   do 75 i=1,nsamp1
		AVCUR(i)=float(iADC(i))
75	   continue
	endif
c
c If voltage sampled, read it too

	if(.not.sampv) goto 73
	if(iav1.eq.0) then
	   n=1
	   do 403 j=1,nrec
	   m=n+255
	   irec=irec+1
	   read(14,rec=irec) (iADC1(i),i=n,m)
	   n=n+256
403	   continue
	else
	   n=1
	   do 404 j=1,nrec
	   m=n+127
	   irec=irec+1
	   read(14,rec=irec) (AVOLT(i),i=n,m)
	   n=n+128
404	   continue
	endif
c
c If IADC1() read copy it to AVOLT for plotting
	if(iav1.eq.0) then
	   do 77 i=1,nsamp1
		AVOLT(i)=float(iADC1(i))
77	   continue
	endif
c
73	continue
      CLOSE(unit=14)
c End of reading of data from disc
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
	endif
99	continue
c
c
	iplot=1
	acal=6553.6		!adc units/volt
c===================================================
	ALLOCATE(xdisp(nsamp+1),ydisp(3,nsamp1+1))

111	continue
c NB if nj2=0 so only one trace is read in, the data is in AVCURCON,AVOLTCON
c whether it is control (CONTROL=true) or drug
	if(sampv) then
	   if(nj1.gt.0.and.nj2.gt.0) then
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
		   klm=1
		   nopt=4
		   que(1)=' (1) UNSUBTRACTED jumps or ramps'
		   que(2)=' (2) NET jump or ramp'
		   que(3)=' (3) Voltage vs time'
		   que(4)=' (4) No more plots '
	         nhelp=1
	         call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &         titlew,helps,nhelp,klm,charout,ival)
		   i=klm
	   	endif
	   else if(nj2.eq.0.and.(.not.control)) then
	      if(alpha) then
	         print 22,iplot
22	         format(
     &         ' (1) AGONIST jump or ramp',/,
     &         ' (3) Voltage vs time',/,
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
		     read 4,i
	   	else
		   klm=1
		   nopt=3
		   que(1)=' (1) AGONIST jump or ramp'
		   que(3)=' (2) Voltage vs time'
		   que(4)=' (3) No more plots '
		   nhelp=1
		   call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &         titlew,helps,nhelp,klm,charout,ival)
		   i=klm
		   if(klm.eq.2) i=3
		   if(klm.eq.3) i=4
	   	endif
	   else if(nj2.eq.0.and.control) then
	      if(alpha) then
	         print 23,iplot
23	         format(
     &         ' (1) CONTROL jump or ramp',/,
     &         ' (3) Voltage vs time',/,
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
		     read 4,i
	   	else
		   klm=1
		   nopt=3
		   que(1)=' (1) CONTROL jump or ramp'
		   que(3)=' (2) Voltage vs time'
		   que(4)=' (3) No more plots '
		   nhelp=1
		   call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &         titlew,helps,nhelp,klm,charout,ival)
		   i=klm
		   if(klm.eq.2) i=3
		   if(klm.eq.3) i=4
	   	endif
	   endif
	else if(.not.sampv) then
	   if(nj2.gt.0) then
	   	if(alpha) then
		   print 24,iplot
24	         format(
     &         ' (1) UNSUBTRACTED jumps',
     &         ' (2) NET jump',
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
		   read 4,i
	      else
		   klm=1
		   nopt=3
		   que(1)=' (1) UNSUBTRACTED jumps'
		   que(2)=' (2) NET jump'
		   que(3)=' (3) No more plots '
		   nhelp=1
		   call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &         titlew,helps,nhelp,klm,charout,ival)
		   i=klm
		   if(klm.eq.3) i=4
	   	endif
	   else if(nj2.eq.0.and.(.not.control)) then
	      if(alpha) then
	         print 25,iplot
25	         format(
     &         ' (1) AGONIST jump',/,
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
	         read 4,i
	      else
		   klm=1
		   nopt=2
		   que(1)=' (1) AGONIST jump'
		   que(2)=' (2) No more plots '
		   nhelp=1
		   call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &         titlew,helps,nhelp,klm,charout,ival)
		   i=klm
		   if(klm.eq.2) i=4
	   	endif
	   else if(nj2.eq.0.and.control) then
	      if(alpha) then
	         print 7,iplot
7	         format(
     &         ' (1) CONTROL jump',/,
     &         ' (4) No more plots ',/,
     &         ' Option number [',i2,'] = ')
	         read 4,i
	      else
		   klm=1
		   nopt=2
		   que(1)=' (1) CONTROL jump'
		   que(2)=' (2) No more plots '
		   nhelp=1
		   call POPMENU(ixlow,iylow,iyhiw,que,nopt,ic,icf,icup,ibkw,
     &         titlew,helps,nhelp,klm,charout,ival)
		   i=klm
		   if(klm.eq.2) i=4
	      endif
	   endif
	endif
c====
	if(i.gt.0) iplot=i      !otherwise default
	if(iplot.eq.4) then
	deALLOCATE(xdisp,ydisp)
	deallocate(avcur,avcurcon,avolt,avoltcon)
	deallocate(jmask,iadc,iadc1)
	RETURN
	endif
c
	if(iplot.eq.2.and.nj2.eq.0) goto 111		!no net curve!
	if(iplot.eq.3.and.(.not.sampv)) goto 111		!no voltage!
	if(iplot.eq.1.or.iplot.eq.2) goto 90
	if(iplot.eq.3) goto 190		!V(t) vs t section
	if(iplot.gt.4) goto 111
c
c
90	continue
c Check how many jumps/ramps in the sweep
	itype(1)=0		!needed below if nvjump=0
	if(nvjump.ge.1) then
	do 10 i=1,nvjump
	   if(ivolt1(i).eq.ivolt2(i)) then
		itype(i)=0	!ith pulse is a jump
	   else
		itype(i)=1	!ith pulse is a ramp
	   endif
10	continue
	endif
c
c  Calculate which points from the entire sweep are to be plotted, in
c terms of index in iADC (following calc was used for drawing horizintal bars
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
	itcum=0		!cumulative time from start of 1st pulse (microsec)
	do 21 i=1,nvjump
	  itcum=itcum+ilenv(i)
	  il=ifixr(float(itcum)/dA)   !# of elements of iADC corresp to ilen
	  j2=nAv+il		! 1st pulse is at iADC(nAv)
	  ifst(i)=j1	!index in iADC, avcur of 1st point of ith pulse
	  ilast(i)=j2	!index in iADC, avcur of last point of ith pulse
	  itcum=itcum+igapv(i)
	  ig=ifixr(float(itcum)/dA)   !# of elements of iADC corresp to igap
	  j1=nAv+ig		! 1st V-jump is at iADC(nAv)
	  if(debug().and.discprt) write(8,52) i,ifst(i),ilast(i)
52	  format(' i,ifst(i),ilast(i) = ',3i8)
21	continue
	if(debug()) pause '1'
c
c
c====return here to plot diff pulse?
c
	ipuls=1
	if(nvjump.gt.1) then
	   do 11 i=1,nvjump
		call intconv(i,getint)
		nm=nblank1(getint)
	   if(itype(i).eq.0) then
		if(alpha) then
		print 12,i
12		format(' Pulse number ',i2,' is a jump')
		else
		   call wdialog(ibox,'Pulse number '//getint(1:nm)//
     &	   ' is a jump',icwd)
		endif
	   else
		if(alpha) then
		print 13,i
13		format(' Pulse number ',i2,' is a ramp')
		else
		   call wdialog(ibox,'Pulse number '//getint(1:nm)//
     &	   ' is a ramp',icwd)
		endif
	   endif

11	   continue
141	   if(alpha) then
	      print 14
14	      format(' Plot pulse number = ')
	      read 4,ipuls
	   else
		ipuls=1
		call defolti(ipuls,def)
		call qdialog(ibox,'Plot pulse number',def,icqd,cans)
		call getinpi(cans,ipuls)
	   endif
	   if(ipuls.lt.1.or.ipuls.gt.nvjump) goto 141
	endif
c=============================================================

	if(debug()) pause '2'
c Now plot pulse # ipuls.
c For a ramp, plot from first point to last
c For V-jump start plot before ifst() to get baseline, and, to get
c off-jump plot equal length after ilast(). This is suitable if we
c have several (more or less short) V-jumps -if have one long one, eg
c just to temporarily change holding pot, then display all with
c Xmin=-iTPREv (so x=0 is start of jump) and xmax=
c Scale X in msec for jumps, the x=0 at time of on-jump: thus end of
c sample (point # nsamp1) will be at x=itsamp-itprev = time from start of
c first v-jump to end of sample
c Scale X in mV for ramps
	if(itype(ipuls).eq.0) then	!jump
	   jlen=ilast(ipuls)-ifst(ipuls)	!or +1 ?======
	   ipre=jlen/3
	   i1=ifst(ipuls)-ipre	!start early for baseline
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
c	ymin=-5.0		!volts to encompass full current scale
c	ymax=+5.0		!ditto
	acal=6553.6		!adc units/volt
c
c Scale the y-axis to fill screen well
	ymin1=1.e37
	ymax1=-1.e37
	ymin2=1.e37
	ymax2=-1.e37
	ymin3=1.e37
	ymax3=-1.e37
	if(nj2.eq.0) then
	  do 149 i=1,nplot
	   j=i1+i-1					!=i1,i1+1,...
	   y1=avcurcon(j)
	   if(y1.lt.ymin1) ymin1=y1
	   if(y1.gt.ymax1) ymax1=y1
149	  continue
	else if(nj2.ge.1) then
	  do 150 i=1,nplot
	   j=i1+i-1					!=i1,i1+1,...
	   y1=avcurcon(j)
	   y2=avcur(j)
	   y3=y2-y1
	   if(y1.lt.ymin1) ymin1=y1
	   if(y1.gt.ymax1) ymax1=y1
	   if(y2.lt.ymin2) ymin2=y2
	   if(y2.gt.ymax2) ymax2=y3
	   if(y3.lt.ymin3) ymin3=y3
	   if(y3.gt.ymax3) ymax3=y3
150	  continue
	endif
	if(debug()) pause '4'
	if(iplot.eq.1) then
	   ymin=ymin1
	   ymax=ymax1
	   if(nj2.ge.1) then
		if(ymin2.lt.ymin1) ymin=ymin2
		if(ymax2.gt.ymax1) ymax=ymax2
	   endif
	else if(iplot.eq.2) then
	   ymin=ymin3
	   ymax=ymax3
	endif
	if(debug()) print 704,ymin,ymax
704	format(' ymin,ymax = ',2g13.6)
c	call FIXAX(ymin,ymax,ymin4,ymax4,ytic,0)
c	ymin=ymin4
c	ymax=ymax4
	if(debug()) print 704,ymin,ymax
	if(debug()) then
	   print 704,ymin,ymax
	   pause
	endif
	curcal=(ymax-ymin)*calfac		!ymax-ymin in pA
	ymin=ymin/acal          !scale into volts
	ymax=ymax/acal
	if(debug()) then
	   print 704,ymin,ymax
	   print 706,vcal,curcal
	   pause
	endif
c	curcal=(ymax-ymin)*calfac*acal	!ymax-ymin in pA
c
c
c For I/V make x-scale wider than data to give good shape for I/V plot
	if(itype(ipuls).eq.1) then	!ramp=I/V plot
	   rx=abs(xmax-xmin)
	   xmin1=xmin-0.25*rx
	   xmax1=xmax+0.25*rx
	else 					!jump
	   xmin1=xmin
	   xmax1=xmax
	endif

c===========================================================
c
c Plot points i1 to i2 (and, for jump, mark posn of jump with bar)
	imode=18
c	i2=0		!overwrites i2 above!
	i2par=0
	   call VGA
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call papenq(xp,yp,ipap)
	   vxlo=0	! for VIEWPORT
	   vxhi=xp
	   vylo=0
	   vyhi=yp
	   xlo=0		! for graphboundary
	   xhi=xp
	   ylo=0.2*yp
	   yhi=yp
	call pixpos(0,53,xsc,ysc)
	ymenu=yp-ysc
	call mode(18)
	ry=abs(ymax-ymin)
	call gSCALE(xmin1,xmax1,ymin-0.1*ry,ymax+0.15*ry,is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)
c	call SCALEL(xmin1,xmax1,ymin-0.1*ry,ymax+0.15*ry)	!leave margins
c	call SCALEL(xmin1,xmax1,ymin,ymax)
	if(debug()) pause '5'
c
c Make array of points already scaled to screen units in xdisp,ydisp
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
	sy1=sy/acal				!ADC0 scale in volts in CED
c	if(debug().and.discprt) write(8,53) ipuls,i1,i2
c53	format(' ipuls,i1,i2 = ',3i8)
c Note: the voltage for I/V is not sampled at exactly same time as current
c Current on ADC0 is first point so should  for cur(i) should use
c Vm(i)= mean of V(i-1) and V(i); for cur(1) must therefore extrapolate
c so Vm(1)=V(t1)-0.5*(V(t2)-V(t1))
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
c	   print 163
c163	   format
	   call lincols(12,0)
         call wrstring18(8,432,
     &   'VJDISPRD4 not yet fixed to plot section of sweep when'//
     &  ' points omitted',12,0)
	   pause
	   RETURN
	endif

c
c	Now define Ydisp (if not whole sample used the appropriate values
c	are already omitted from iADC, avcur etc
	do 15 i=1,nplot
	   j=i1+i-1					!=i1,i1+1,...
c	   ydisp(i)=float(iADC(j))*sy + yoff
	   ydisp(1,i)=avcurcon(j)*sy1 + yoff	!jump in 'control'
	   ydisp(2,i)=avcur(j)*sy1 + yoff	!jump in 'drug'
	   ydisp(3,i)=(avcur(j)-avcurcon(j))*sy1 + yoff	!net jump
c	   if(discprt) write(8,80)
c     &	xdisp(i),ydisp(1,i),ydisp(2,i),ydisp(3,1)		!debug!!
c80	   format(4g13.6)
15	continue
	xdisp(nsamp1+1)=xdisp(nsamp1) !need to draw bar if V jump goes to end of sample
c
	if(debug()) pause 'position 1'
c draw data
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
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
	do 200 i=1,ncjump
	  t2=t1+float(ilenc(i))*1.e-3		!msec from trigger to end of pulse
	  if(t2.lt.xmin) goto 200
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
200	continue
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
	   call REALTOCH(xmin,ccal,nch)
	   call REALTOCH(xmax,ccal1,nch)
	   call lincolss(15,0)		!white -for wrstring18 call
c	   call WRSTRING18(ixc0,iyc0,
c     &		'X-scale = '//ccal//' to '//ccal1//' mV',1,15)
	   call wrstring18(160,8,'X-scale = '//ccal//' to '//ccal1//' mV',
      &  15,0)
	endif
c Plot data
	call lincols(12,0)		!red -for wrstring18 call
	if(iplot.eq.1) then
	   if(nj2.ge.1) then
		do 2 i=1,nplot
	 	call wPIXel(xdisp(i),ydisp(1,i),11)		!con=light blue
	 	call wPIXel(xdisp(i),ydisp(2,i),12)		!drug=red
2		continue
		call lincols(11,0)		!light blue -for wrstring18 call
		call INTCONV(nj1,ccal)
		if(control) then
		   call wrstring18(464,8,'Control: '//ccal,11,0)  !normally
		else
		   call wrstring18(464,8,'Agonist: '//ccal,11,0)  !in case both are agonist
		endif
		call lincols(12,0)		!red -for wrstring18 call
		call INTCONV(nj2,ccal1)
		if(.not.control1) then
		   call wrstring18(448,8,'Agonist: '//ccal1,12,0)  !normally
		else
		   call wrstring18(448,8,'Control: '//ccal1,12,0)  !in case both are controls
		endif
	   else if(nj2.eq.0) then
c if nj2=0 data is in ydisp(i,1) whether drug or control
		icol=12			!red
		if(control) icol=11	!light blue
		do 3 i=1,nplot
	 	call wPIXel(xdisp(i),ydisp(1,i),icol)
3		continue
		if(control) then
		   call lincols(11,0)		!light blue -for wrstring18 call
		   call INTCONV(nj1,ccal)
		   call wrstring18(0,0,'Control: '//ccal)
		else
		   call lincols(12,0)		!red -for wrstring18 call
		   call INTCONV(nj1,ccal1)
		   call wrstring18(1,0,'Agonist: '//ccal1)
		endif
	   endif
	else if(iplot.eq.2) then
	   call lincols(14,0)		!yellow -for wrstring18 call
	   do 6 i=1,nplot
	    call wPIXel(xdisp(i),ydisp(3,i),14)		!net=yellow
6	   continue
	   call wrstring18(1,0,'NET current ')
	endif
	ymin5=ymin*calfac*acal		!in pA
	ymax5=ymax*calfac*acal		!in pA
	if(curcal.le.1000.) then
	   call REALTOCH(ymin5,ccal,nch)
	   call REALTOCH(ymax5,ccal1,nch)
	   unit=' pA '
c	   call REALTOCH(curcal,ccal)		!pA for full scale
c	   call wrstring18(26,1,' Full scale = '//ccal//' pA')
	else if((curcal.gt.1000.).and.curcal.le.1.e6) then
	   call REALTOCH(ymin5*1.e-3,ccal,nch)
	   call REALTOCH(ymax5*1.e-3,ccal1,nch)
	   unit=' nA '
c	   call REALTOCH(0.001*curcal,ccal)	!nA for full scale
c	   call wrstring18(26,1,' Full scale = '//ccal//' nA')
	else if(curcal.gt.1.e6) then
	   call REALTOCH(ymin5*1.e-6,ccal,nch)
	   call REALTOCH(ymax5*1.e-6,ccal1,nch)
	   unit=' muA'
c	   call REALTOCH(1.e-6*curcal,ccal,nch)	!microamp full scale
c	   call wrstring18(26,1,' Full scale = '//ccal//' muA')
	endif
	call lincols(15,0)		!white -for wrstring18 call
	call wrstring18(28,0,'Y-scale = '//ccal//' to '//ccal1//unit)
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
c	call PLOTS(0,i2par,imode)		!start graphics
c	call INITLGRF
c In call to SCALEL make margins by calling with range greater than xmin,xmax,..
c	rx=abs(xmax-xmin)
c	ry=abs(ymax-ymin)
c	xmin1=xmin-0.05*rx
c	xmax1=xmax+0.05*rx
c	ymin1=ymin-0.05*ry
c	ymax1=ymax+0.05*ry
	ymin1=1.e37
	ymax1=-1.e37
	deb=debug()
	if(nj2.gt.0) then
	  do 151 i=1,nsamp1
	   y1=avoltcon(i)
	   y2=avolt(i)
	   if(deb) print 703,i,y1,y2,ymin1,ymax1
703	   format(' i,y1,y2,ymin1,ymax1=',i5,4g12.5)
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
151	  continue
	else if(nj2.eq.0) then		!voltage is in AVOLTCON
	  do 155 i=1,nsamp1
	   y1=avoltcon(i)
	   if(y1.lt.ymin1) then
		ymin1=y1
		imin=i
	   endif
	   if(y1.gt.ymax1) then
		ymax1=y1
		imax=i
	   endif
155	  continue
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
	   print 706,vcal,curcal
706	   format(' vcal,curcal = ',2g13.6)
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
	   ydisp(1,i)=avoltcon(i)*sy1 + yoff	!voltage in 'control'
	   ydisp(2,i)=avolt(i)*sy1 + yoff		!voltage in 'drug'
152	continue
	xdisp(nsamp1+1)=xdisp(nsamp1)+dx !need to draw bar if V jump goes to end of sample
c zero line
	ipen=9
	call MOVEL(xmin,0.)
	call DRAWL(xmax,0.)     !blue line at zero current
c Draw data
	if(nj2.ge.1) then
		do 154 i=1,nsamp1
	 	call wPIXel(xdisp(i),ydisp(1,i),11)		!con=light blue
	 	call wPIXel(xdisp(i),ydisp(2,i),12)		!drug=red
154		continue
		call lincols(11,0)		!light blue -for wrstring18 call
		call INTCONV(nj1,ccal)
		if(control) then
		   call wrstring18(0,0,' Control: '//ccal)  !normally
		else
		   call wrstring18(0,0,' Agonist: '//ccal)  !in case both are agonist
		endif
		call lincols(12,0)		!red -for wrstring18 call
		call INTCONV(nj2,ccal1)
		if(.not.control1) then
		   call wrstring18(1,0,' Agonist: '//ccal1)  !normally
		else
		   call wrstring18(1,0,' Control: '//ccal1)  !in case both are controls
		endif
	else if(nj2.eq.0) then
c if nj2=0 data is in ydisp(i,1) whether drug or control
		icol=12			!red
		if(control) icol=11	!light blue
		do 156 i=1,nsamp1
	 	call wPIXel(xdisp(i),ydisp(1,i),icol)
156		continue
		if(control) then
		   call lincols(11,0)		!light blue -for wrstring18 call
		   call INTCONV(nj1,ccal)
		   call wrstring18(0,0,' Control: '//ccal)
		else
		   call lincols(12,0)		!red -for wrstring18 call
		   call INTCONV(nj1,ccal1)
		   call wrstring18(1,0,' Agonist: '//ccal1)
		endif
	endif
c Draw Y axis and calibration
	ipen=9
c	ipen=15
	call MOVEL(xmin,ymin)
	call DRAWL(xmin,ymax)     !y axis -now dark blue like zero line
	call lincols(15,0)
	call REALTOCH(ymin,ccal,nch)
	call REALTOCH(ymax,ccal1,nch)
	call wrstring18(28,0,' Y-scale = '//ccal//' to '//ccal1//' mV')
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
	call LOCATE(1,0)
c	print 58
c58	format(' Another plot [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') then
	   if(sampv) then
		if(nj2.ge.1) then
		   iplot=iplot+1
		else
		   iplot=iplot+1
		   if(iplot.eq.2) iplot=3
		endif
	   else
		if(nj2.gt.0) then
		   iplot=iplot+1
		   if(iplot.eq.3) iplot=4
		else
		   if(iplot.eq.1) iplot=4
		endif
	   endif
	   if(iplot.gt.4) iplot=1
c	   goto 111
c	endif
	call lincols(15,0)		!for wrstring18 call
	call wrstring18(0,60,'ANY KEY to continue')
	call ANYKEY
	goto 111
c	RETURN
	end



