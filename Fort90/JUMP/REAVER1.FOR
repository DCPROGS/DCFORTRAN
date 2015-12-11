	subroutine REAVER1(istrec,title1,nj1,nj2,n1,n2,jmean,makav,
     &  iver1,ikeep1,nkeep,kstep,nsamp0,nmax,calfac,idest,
     &  nsweep,naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
     &  nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
     &  irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
     &  nDv,iDd,keepall,ivhold,
     &  ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &  filter,itpref,fcnew,fcn,align,njd)

C==========Replace all refs to CJUMP.DAT with full path and name of file

c To re-average records from CJUMP.DAT, before fitting, in CJFIT.
c Lahey V5.n version 08/31/94 09:28am
c
c NB: nsamp,itsamp,vjump,sampv,control,keepall are in common/CJPAR (is this nec?)
c Split REAVER into 2 parts -first part to read records n1 to n2
c to get necessary array size, return to main to allocate array,
c then call REAVER2 with alloc arrays as args and do graphics
c (Array jmask is local in REAVER1 -note that its index runs from
c i=1,nsamp0 -total number of points in original sweep, not to
c nsamp=number of points kept on disk)
c
c OLD NOTES:
c
c 01/20/93 08:52am Modified by adding nsamp0, jmask to parameters -needed
c for CJUMP3 data (they are not defined unless records were read in main prog).
c
c 07/15/91 04:48pm Yval made (2048,15) for compat with CJVDU (not needed here)
c REAVER1 is version of REAVER to cope with nsamp>2048 by using several
c Yval(i,j) arrays (j=1,2,..). Keep down display time (and size of xdisp,
c ydisp arrays) by showing not more than 2048 points (omit every nth)
c Skipping points made much simpler by EQUIValencing YVAL(i,j) to YVAL1(i)
c because latter contains, sequentially, the rows of former
c
c Modified 29/11/91 to detect and measure first latencies to channel
c opening.  Tint1(i) and Tint2(i) hold values for first latencies for
c the ON-CJUMP and OFF-CJUMP periods, respectively.  These latencies can
c subsequently be read into EKDIST via DISKIN.FOR to construct and fit
c first latency distributions.  Additional variables kept for EKDIST are:
c		nlon=number of 1st latency values for ON jump
c		-these are stored here in TINT1(1) to TINT1(nlon)
c		nloff=number of 1st latency values for OFF jump
c		-store here in TINT2(1) to TINT2(nloff)
c		tjlen=length of jump (real milliseconds) (=truncation
c		 point for ON jump latencies)
c		ttoff=truncation point for OFF latencies (=time from end
c		 of OFF jump (corrected for offset) to end of the sample)
c		toffset=time from trigger to when jump actually starts at
c		 the patch
c         all of the above variables are *4
c
c First latency option modified 27/12/91 to detect transitions using
c a 50% threshold crossing criterion.  Cubic spline interpolation also
c implemented to improve accuracy of latency estimate.  Routine will now
c give values for both the ON- and OFF-JUMP latencies (the latter provided
c no channels are open at OFF-JUMP (soff)) as well as the lifetime of
c the first opening after CJUMP.
c
c Modified 3/12/91 to show expanded display of detected openings in 1st
c latency routine to allow verification of the detection algorithm --
c should eventually be able to accept or reject each detected "opening".
c Latter is not finished.  The expanded display is shown in a window (or
c inset display) modeled after the window constructed previously for
c use in TRANAV by DC -- see LPLOTAV.FOR for details.
c
c The CJUMP.DAT file is already open -ask which records wanted and
c display them with Lahey graphics.  May want to make TWO new averages,
c to be subtracted (drug and control)
c  Read data into Yval initially, to save rewriting READREL, but
c  copy it into AVCUR, XDISP etc for display and averaging. Put
c  final average back into correct YVAL elements before leaving REAVER
c Put final averages into Yval for display and fit
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
c=	integer*2 istrec(500)
	integer*4 istrec(1000)
	integer*2 ivhclamp,ipatch
	integer*2 ivhcsav,ivhsav
      character cdate*11,ctime*11
	character title1*79
	logical discprt,pon,slock,vjump,sampv,control,fixpot,sampatch
c      character*1 ans,UC
	logical filter,align
c for CJUMP3/4 data
	integer*2 iver1
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	logical keepall
	ALLOCATABLE::jmask		!local in REAVER1
	integer*1 jmask(:)
	common/dp/discprt
	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
	common/lgrf/xpos,ypos,ipen		!ditto
c common for values in RELWRT and READAT (which reads back data written
c by RELWRT) -now parameters
c	COMMON/cjpar/nsweep,
c     & naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
c     & nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
c     & irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
c     & t1v,t2v,t1c,t2c,nDv,iDd,keepall,ivhold
c For interpolation of data and calculation of time to 50% threshold
	common/bisec/xl(11),yl(11),yl2(11),n,klo,khi
c=	external FUNC
c
c Colors:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple;
c 6=brown; 7=white; 8=grey; 9=dark blue -bright; 10=green -bright;
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	pon()=slock()
c
101   format(a1)
c
	if(idest.eq.5) then
	   idest=0
	   goto 113
	else if(idest.eq.6) then
	   idest=0
	   goto 26
	else
	   idest=0
	endif
c
26	continue
c===next line needed if makav.ne.3 to prevent error later
c==	meanlev=.false.
	if(align) then
	   makav=1
	else
	   print 10
10	   format(
     &    ' (1) Make a single new average',/,
     &    ' (2) Make two new averages (eg drug and control)',/,
     &    ' (3) List more details',/,
     &    ' Option number [1] = ')
	   makav=1
	   call INPUTi(makav)
	endif
	if(makav.eq.3) then
	   idest=67
	   RETURN
	endif
12	continue
	if(makav.eq.1) then
	  nj2=0
	  n1=1
	  n2=njd
	  print 11,n1,n2
11	  format(
     & ' Average sweeps n1 to n2 [',i2,',',i5,'] n1,n2 =  ')
	else if(makav.eq.2) then
	  print 112
112	  format(
     & ' For 1st average, use sweeps n1 to n2: [more details] n1,n2 = ')
	endif
	call INPUT2i(n1,n2)
	if(n1.lt.1) then
	   idest=67
	   RETURN
	endif
	nj1=n1	!to specify param for 1st average (same for n1,...,n2)
	if(n2.lt.n1.or.n1.lt.1) goto 12
c Read all details for the first one (need sample rate, calfac etc before
c starting loop)
	jmean=1		!doing the 1st average
c
c RETURN HERE (from REAVER2) FOR THE 2nd AVERAGE, if req
113	continue
	nmax=0		!not defined unless iver1.le.-1003
	irec=istrec(n1)
c Read the specified record  'header'
	call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,sampatch,0,defname,npatch,temp)
c NB iver1=-1001 for CJUMP2 data (values as far as swtime recorded) and
c iver1=-1003 for CJUMP3 data, for which all values read above are recorded
	if(.not.filter) fcn=ffilt
	nsamp0=nsamp		!# of points if all points kept
	if(iver1.le.-1003) then
	   if(allocated(jmask)) DEALLOCATE(jmask)
	   ALLOCATE(jmask(nsamp0))
	   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &    keepall,nsamp0)
	   nsamp0=nsamp		!orig # of points samples
	   nsamp=nsamp1		!# of points kept on disc
	endif
	call ILCONV(ilenc,igapc,ilenv,igapv,
     &  ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	call PREC(n1,title1,cdate,ctime,naver,navc,iav,vjump,
     &   control,iver1,1)
	call PPAR(1,0,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     & ivhclamp,ipatch,ffilt)
91	print 9,n1,n2,n1,n2,fcn
9	format(
     &' (1) Use this record as it is',/,
     &' (2) Specify new calibration for records ',i3,' to ',i3,/,
     &' (3) Specify new holding potential for records ',i3,' to ',i3,/,
     &' (4) Now filtered at fc = ',f8.1,' Hz -specify new filtering',/,
     &' (5) Specify different records',/,
     &' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.le.0.or.iopt.gt.5) iopt=1
	if(iopt.eq.5) goto 12
c
	if(iopt.eq.4) then
	   filter=.true.
	   print 53,fcn
53	   format(
     &    ' At present filtered at ',f9.1,' Hz -new final fc (Hz) = ')
	   call INPUTr(fcn)
	   if(fcn.lt.ffilt-1.) then
	     fcnew=1./sqrt((1.0/(fcn*fcn)) - (1.0/(ffilt*ffilt)))
	     print 52,ffilt,fcnew,fcn
           if(pon()) write(7,52) ffilt,fcnew,fcn
           if(discprt) write(8,52) ffilt,fcnew,fcn
52	     format(
     &  ' Data already filtered at ',g13.6,' Hz.',/,
     &  '  Filtered at fc = ',g13.6,' Hz to give final fc = ',g13.6)
	      tpref=0.8*float(itPREc)/1000.
	      print 54,tpref
54	      format(
     & ' Use mean of first and last x ms to eliminate end effect: [',
     &   	f8.2,'] x = ')
		call INPUTr(tpref)
		itpref=tpref*float(irate)/1000.	!number of points
	   else
		filter=.false.
	   endif
c
	else if(iopt.eq.2) then
c Defaults for calibration
	   if(iver1.gt.-1003) then
	     ftape=1.0		!some defaults
	     errfac=1.0
	     gain=10.0
	     amVpA1=10.		!mV/pA on patch clamp
	   endif
	   calvolt=10.		!factor by which Vout is greater then true Em
	   call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     &   itape,ftape,gain,errfac)
	   i=ifixr(calvolt)
	   print 68,i
68	   format(
     & ' mV out from clamp per mV membrane pot (integer) [',i3,'] = ')
	   call INPUTi(i)
	   calvolt=float(i)
c
	else if(iopt.eq.3) then
	   i=ivhold
	   print 683,i
683	   format('&Holding potential set by 1401 (mV) [',i4,'] = ')
	   call INPUTi(i)
	   ivhsav=int2(i)
	   ivhold=ivhsav
	   i=ivhclamp
	   print 684,i
684	   format(
     & '&Holding potential set on patch clamp box (mV) [',i4,'] = ')
     	   call INPUTi(i)
	   ivhcsav=int2(i)
	   ivhclamp=ivhsav
	   fixpot=.true.
	else if(iopt.ne.1) then
	   goto 91		!another choice?
	endif
c
c Print (only) all details
	call PREC(n1,title1,cdate,ctime,naver,navc,iav,vjump,
     &   control,iver1,3)
	call PPAR(0,2,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     & ivhclamp,ipatch,ffilt)
c
c Now loop through rest of records to find largest nsamp
c NB THIS IS SUPERFLUOUS! RECORDS FOR AVERAGING MUST ALL HAVE THE
c SAME nsamp etc, AS CHECKED IN REAVER2!
c	nsampmax=nsamp	!value from sweep #n1
c	ns0max=nsamp0
c	nmaxmax=nmax		!unless iver.le.-1004 so nmax defined
c	do iswp=n1+1,n2
c	   irec=int4(istrec(iswp))
cc Read the specified record  'header'
c	   call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
c     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
c     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
c     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
c     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
c     &     ivhclamp,ipatch,ffilt,sampatch,0,defname,npatch,temp)
cc NB iver1=-1001 for CJUMP2 data (values as far as swtime recorded) and
cc iver1=-1003 for CJUMP3 data, for which all values read above are recorded
c	   nsamp0=nsamp		!# of points if all points kept
c	   if(iver1.le.-1003) then
c		if(allocated(jmask)) DEALLOCATE(jmask)
c		ALLOCATE(jmask(nsamp0))
c		call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
c     & 	  keepall,nsamp0)
c		nsamp0=nsamp		!orig # of points samples
c		nsamp=nsamp1		!# of points kept on disc
c	   endif
c	   if(iver1.le.-1003) then
c		if(nmax.gt.nmaxmax) nmaxmax=nmax
c		if(nsamp0.gt.ns0max) ns0max=nsamp0
c	   endif
c	   if(nsamp.gt.nsampmax) nsampmax=nsamp
c	enddo
c	nmax=nmaxmax	!return to main prog
c	nsamp=nsampmax	!return to main prog
c	nsamp0=ns0max	!return to main prog
c Now return to main prog to allocate arrays
	if(allocated(jmask)) DEALLOCATE(jmask)
	RETURN		!from REAVER1
	end


