	subroutine NOISFIT(istart,iend,ngood,idata,idata1,nchan,
     & nread,nmax,nsec,npsec,isec,rate,calfac,calfac1,ioff,CONDAT,
     & npnt,ngsub,igval,ndev,madc1,contin,ibcor,nepsc,idiskq)
c To fit curve to specified region for subtraction before calc of spectrum
c Note that SSDNOIS now ignores CONSTR and constrains only the tau to be
c positive
c Modified 02/06/91 03:56pm to fit simulated epscs
c 01/17/91 08:56pm args removed (no longer used): nvalg,irecg,nint
c	Usually (madc1=false) both subtracted data (for noise) and unsubtracted
c data (for mean current) taken from the same channel viz ADC0, assumed to have
c a DC-recorded EPSC etc on it. No need sample on ADC1 at all.
c	Problem of data sections that are longer than 2048 points solved
c by using EQUIVALENCE to display all points. Need 2 Xval,Yval arrays (j=1
c for data, j=2 for residuals), but may have more than 2048 points so
c declare Xval(2048,8), equiv to Xval1(8192,2) and define up to 8192
c data points in latter as normal, but then call VPLOTR with former (and
c with ndim=8). In this way VPLOTR uses Xval for j=1,2,3,4 as though
c it was a single 8192 point data array. Also define, for input to VPLOTR'
c for example, ndat(1)=2048,ndat(2)=2048,ndat(3)=2 if there are 4098 values
c to be displayed (all ndat except last must be 2048). See tests in T2D.FOR.
c (to speed display could easily give nskip as a parameter of VPLOTR)
c
c NOT YET DONE:
c However it may be convenient to have Butterworth-filtered DC recording
c on ADC0 for noise, AND identical sample on ADC1, but Bessell filtered for
c easier fitting, and mean current, calculation (less ringing). Latter case
c assumed if MADC1 is true.
c
c Modifs to be done
c (2) option to redisplay (to replace goto 230)
c (3) fix vplotr so axes not redrawn, only data, for speed?
c Now fixed so that if good bit is longer than 2048 points then every
c 2nd point displayed (but all points used for fitting).
c c(4) in this version good bits must not be longer than 2048 points,
c c	otherwise truncated to 2048! But could display/fit every 2nd point,
c c	or average in groups of 2,3,.. to get down to 2048 points for display
c
	integer istart(512),iend(512),npnt(512)
c	integer thetsav(5,512)		!keep parameters
	integer*2 idata(81920)
	integer*2 idata1(2,40960)     !equivalence for sep of 2-channel data
c	integer*4 irecg(512)			!to keep good bits on disc
c	integer*2 nvalg(512)
	integer*2 igval(512)
	character CONDAT*30,ndev*2
	integer*2 lrow,int2
c for VPLOTR: NB calc curves will always have ncalc=301 points so no need to
c use equivalence trick for Xcal,Ycal
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	real*4 Xval(2048,8),Yval(2048,8)
	integer ndat(8),icurvd(8),isym(8),ijoin(8)	!for data
	real symsiz(8)				!for data
c	integer ncal(8),icurvc(8),iline(8)		!for calc curve
c	real*4 Xcal(2048,8),Ycal(2048,8)
	real*4 Xcal(2048,2),Ycal(2048,2)
	integer ncal(2),icurvc(2),iline(2)		!for calc curve
c equivalenced arrays for defining data
	real*4 Xval1(8192,2),Yval1(8192,2)
c	real*4 Xcal1(8192,2),Ycal1(8192,2)		!not needed
	integer ndat1(2)        !for Xval1,Yval1
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!needed for VPLOTR; not used
c	integer*2 videotyp
	character*40 titlex,titley
	character*44 title
	logical fitted,doframe,draft,autplt,vramp,landscap,contin
c general
	logical discprt,pon,debug,caplock,slock
	character*1 ans,UC
c for fitting
	character*10 TITLEP(20)		!names of params for fit
	logical constr,fixtau,fixamp,fixtau1,fixamp1
	logical refit,madc1,specg
	real*4 theta(20)
	integer JFIX(20),jfixsav(20)
      character*1 ch,fndcur
c
	common/data/Xfit(8192),Yfit(8192)
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,fixamp,relamp(10)
	common/dp/discprt
	EQUIVALENCE (Xval,Xval1)
	EQUIVALENCE (Yval,Yval1)
c	EQUIVALENCE (Xcal,Xcal1)	!not needed
c	EQUIVALENCE (Ycal,Ycal1)
	EXTERNAL SSDNOIS
c
c Define functions
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
4	format(i8)
	lastrow=3			!NB args of SCROLL=integer
	lrow=int2(lastrow)	!but args of LOCATE are int*2
c
	ndimd=8
	ndimc=2
	ichan=1		!use ADC0 only
c
	ig1=1
	ig2=ngood
c If continuing previous fitting, get ngsub from NOISPAR
	if(contin) then
         OPEN(unit=16,file=ndev//'\NOISPAR.DAT',status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(16,rec=1) ngsub
c	   Now read the last one written to disc to see which good bit in
c	   istart(),iend() it corresponds to
	   irec=5 + (ngsub-1)*116
	   read(16,rec=irec) ig1
	   CLOSE(unit=16)
	   ig1=ig1+1	!start with next good bit?
	   print 65,ngood,ig1-1,ig1,ngood
65	   format(
     & ' Number of good bits = ',i4,': got up to number ',i4,' before:',
     & /,' Go through rest of good bits (',i4,' to ',i4,') [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 62
c62		format('& go through numbers n1 to n2: n1,n2 = ')
c		read 63,ig1,ig2
		call INPUT2i(ig1,ig2)
		if(ig1.lt.1.or.ig1.gt.ngood) ig1=1
		if(ig2.lt.1.or.ig2.gt.ngood) ig2=ngood
	   endif
	else		!normal start
	   ngsub=0
	   print 60,ngood
60	   format(
     & ' Number of good bits = ',i4,': go through all of them [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 62
62		format('& go through numbers n1 to n2: n1,n2 = ')
		call INPUT2i(ig1,ig2)
c		read 63,ig1,ig2
63		format(2i8)
		if(ig1.lt.1.or.ig1.gt.ngood) ig1=1
		if(ig2.lt.1.or.ig2.gt.ngood) ig2=ngood
	   endif
	endif
c
c Define titles (titlep(kmax)='  Y(inf) ' defined when ncomp decided)
	do 614 i=1,10
	   j=2*i-1		!=1,3,5,...19
	   titlep(j)='  tau('//char(i+48)//') '		!j=1,3,5,...
	   titlep(j+1)='  amp('//char(i+48)//') '		!j=2,4,6,...,20
614	continue
c
	nfix=0
	nfixsav=0
	do 115 i=1,20
	jfix(i)=0		!initialise
115	jfixsav(i)=0	!copy, to restore JFIX after refit
	fixtau=.false.
	fixamp=.false.
	fixtau1=fixtau	!save to restore after refit
	fixamp1=fixamp	!save to restore after refit
	refit=.false.
	ncomp1=1
	print 31,ncomp1
31	format(
     & ' Number of components to fit (-1 to ask each time) [',i2,'] = ')
	call INPUTi(ncomp1)
c	read 4,i
c	if(i.ne.0) ncomp1=i	!so set to -1 to ask each time
	if(ncomp1.ge.1) then
	   kmax=2*ncomp1+1		!no of parameters
	   titlep(kmax)='  Y(inf) '
	   print 32
32	   format('&Fix the time constants for fits [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then
		fixtau=.true.
		fixtau1=fixtau	!save to restore after refit
		do 33 i=1,ncomp1
		print 34,i
34		format('& tau(',i2,') (ms) = ')
		j=2*i-1                	!tau=theta 1,3,5,7,9 (1 set above)
c		read 8,theta(j)
		call INPUTr(theta(j))
      	if(pon()) write(7,342) titlep(j),theta(j)
	      if(discprt) write(8,342) titlep(j),theta(j)
342		format(' Parameter ',a10,' fixed at ',g13.6)
		jfix(j)=1		!for fixed parameters
		jfixsav(j)=1		!to restore jfix after refit
		nfix=nfix+1
33		continue
		nfixsav=nfix
	      kfit=kmax-nfix
	   endif
	   if(ncomp1.ge.2) then
	    print 21
21	    format('&Fix the relative amplitudes for fits [Y] ? ')
	    read 101,ans
	    if(UC(ans).ne.'N') then
		fixamp=.true.
		fixamp1=fixamp	!save to restore after refit
		sumamp=0.0
		do 334 i=2,ncomp1
		print 341,i
341		format('& amp(',i2,')/ amp(1) = ')
c		read 8,relamp(i-1)	! 1,2,...,ncomp-1
		call INPUTr(relamp(i-1))
      	if(pon()) write(7,343) i,relamp(i-1)
	      if(discprt) write(8,343) i,relamp(i-1)
343		format(' Ratio amp(',i2,')/ amp(1) fixed at ',g13.6)
		sumamp=sumamp+relamp(i-1)
c	amps= theta 2,4,6,8,10 so fix param 4,6,... (may as well fix them
c	because even if simplex changed them they would be reset in SSDNOIS)
		jfix(2*i)=1		!fix amplitudes 2,3,...,ncomp
		jfixsav(2*i)=1	!to restore jfix after refit
		nfix=nfix+1
334		continue
		sumamp=1.0+sumamp		!=total amp/amp(1)
		nfixsav=nfix
	      kfit=kmax-nfix
	    endif
	   endif
	endif
c
c Read sections as req, and display each 'good' bit for fit
c start with x=t=0 for the first point, for now. dx=msec between points
c For simulated EPSCs number of points is same each time (=nepsc) so
c can cal Xval here
	dx=1.e3/rate	!=msec between points
	xmin=0.
	if(ibcor.eq.3) then
	  do 80 i=1,nepsc
	   Xval1(i,1)=xmin+float(i-1)*dx
80	  continue
	endif
c
	isec=0	!number of section that is currently in memory
c
c	do 1 ig=1,ngood
	do 1 ig=ig1,ig2
c Display current good bit, and define region to be fitted.
c The point with index=IS, idata(is), in section number ISEC has index
c	in = is + (isec-1)*npsec
c Point with index=IN in the whole data set occurs in section
c	isec = 1 + (in-1)/npsec
c and has index, within this section,
c	is = in - (isec-1)*npsec = in - ipsec
c Display starts at point with index is=isfst in current section so point
c with index=ID on display (1st displayed point=1) has index in section of
c	is = id + isfst - 1
c Similarly incurs,iscurs,idcurs= index in whole array, section, display resp
c of the data point nearest to the cursor.
400	continue		!return to completely redo current event
	if(ibcor.eq.3) then
         OPEN(unit=17,file=CONDAT,status='UNKNOWN',
     &   access='TRANSPARENT')		!open CONSIM.DAT
c 	 For CONSIM 'header' = 12 bytes, and each EPSC= kchan (number of
c      channels added) + nepsc data values, takes 4*(1+nepsc) bytes
c      so event #iepsc starts at irec=13 + (iepsc-1)*4*(1+nepsc)
c	 In this case data is already in floating pA so read directly
c	 into Yval1() (Xval1 defined above
	   irec=13+(ig-1)*4*(1+nepsc)
c	   read(17,rec=irec) kchan,(avcur(i),i=1,nepsc)
	   read(17,rec=irec) kchan,(Yval1(i,1),i=1,nepsc)
	   CLOSE(unit=17)
	   npntig=nepsc
	   iendig=iend(ig)
	   goto 82
	endif
c
c Now normal (ibcor=2) case
	npnt(ig)=iend(ig)-istart(ig)+1
	iendig=iend(ig)
	npntig=npnt(ig)
c	nskip=1
c	if(npnt(ig).gt.2048) then
c	   nskip=2
c112	   n=npnt(ig)/nskip
c	   if(n.gt.2048) then
c		nskip=nskip+1
c		goto 112
c	   call BELL(1)
c         print 111,npnt(ig),nskip
c         if(pon()) write(7,111) npnt(ig),nskip
c         if(discprt) write(8,111)npnt(ig), nskip
c111	   format('&Good bit is ',i6,' points: every ',i3,
c     &	'points displayed')
c	endif
	isec1=1+(istart(ig)-1)/npsec		!section for start point
	isec2=1+(iendig-1)/npsec		!section for end point
c read in isec1, if not already in
	if(isec1.ne.isec) then
	  ipsec=(isec1-1)*npsec		!to avoid repeated recalc
	  call READSEC1(isec1,idata,nread,nsec,ioff,nmax,CONDAT)
	  isec=isec1
	endif
c Define Yval1,Xval1
	is1=istart(ig)-ipsec	!index of start point within section
	if(isec1.eq.isec2) then
	   is2=iendig-ipsec
	else
	   is2=npsec		!read next section(s) below to finish it
	endif
	ndone=is2-is1+1	!=npnt(ig) if all in 1 section
	if(nchan.eq.1) then
	   i1=0
	   do 11 i=is1,is2
	   i1=i1+1		!=1,2,...,ndone;
	   if(i1.lt.1.or.i1.gt.8192.or.i.lt.1.or.i.gt.81920) then
		print 700,i,i1,is1,is2,isec,isec1,isec2,ig,istart(ig),iendig
700		format(' i,i1,is1,is2,isec,isec1,isec2,ig,istart,iend = ',/,
     &	5i9,/,5i9)
		stop
	   endif
	   Yval1(i1,1)=float(idata(i))*calfac
	   Xval1(i1,1)=xmin+float(i1-1)*dx
	   il1=i		!index of last idata() used
11	   continue
	else
	   i1=0
	   do 12 i=is1,is2
	   i1=i1+1		!=1,2,...,ndone;
	   Yval1(i1,1)=float(idata1(ichan,i))*calfac
	   Xval1(i1,1)=xmin+float(i1-1)*dx
	   il1=i		!index of last idata() used
12	   continue
	endif
	ndone=i1
c	if(debug()) then
c         print 122
c         if(discprt) write(8,122)
c122	   format('   i     i1     Xval1(i1)    Yval1(i1)')
c	   i1=0
c	   do 121 i=is1,is2
c	   i1=i1+1		!=1,2,...,ndone;
c	   if(nchan.eq.1) then
c	      Yval1(i1,1)=float(idata(i))*calfac
c	   else
c	      Yval1(i1,1)=float(idata1(ichan,i))*calfac
c	   endif
c	   Xval1(i1,1)=xmin+float(i1-1)*dx
c         print 123,i,i1,Xval1(i1,1),Yval1(i1,1)
c         if(discprt) write(8,123) i,i1,Xval1(i1,1),Yval1(i1,1)
c123	   format(2i8,2g13.6)
c121	   continue
c	endif
c If good bit covers more than on section, do the rest
	if(isec2.gt.isec1) then
	   do 5 is=isec1+1,isec2
	   ipsec=(is-1)*npsec		!to avoid repeated recalc
	   call READSEC1(is,idata,nread,nsec,ioff,nmax,CONDAT)
	   isec=is			!section now in
c	   is1=1			!index of start point within section
	   is1=il1-npsec+1	!=1 if npsec=1
	   if(is.eq.isec2) then
	      is2=iendig-ipsec	!last section in
	   else
		is2=npsec		!more section(s) to be read
	   endif
	   if(nchan.eq.1) then
		i1=ndone
		do 13 i=is1,is2
		i1=i1+1		!=ndone+1,ndone+2,...
		Yval1(i1,1)=float(idata(i))*calfac
		Xval1(i1,1)=xmin+float(i1-1)*dx
	      il1=i		!index of last idata() used
13		continue
	   else
		i1=ndone
		do 14 i=is1,is2
		i1=i1+1		!=ndone+1,ndone+2,...
		Yval1(i1,1)=float(idata1(ichan,i))*calfac
		Xval1(i1,1)=xmin+float(i1-1)*dx
	      il1=i		!index of last idata() used
14		continue
	   endif
	   ndone=i1      !for next loop if > 2 sections!
5	   continue
	endif
c
82	continue		!jump to here when ibcor=3
c
c Xval1,Yval1 now defined for current good bit, so display it
c
c	symsiz(1)=-1.0 	!default symbol sizes
	ncjump=0
	nvjump=0
	iy1c=-1		!no jump logos
	vramp=.false.
c
c Misc inputs for VPLOT
	ilabel=1
	titlex='time (ms)'
	titley='current (pA)'
	iscal=1		!scale internally
	ifitype=4		!display of exponential parameters
	cbig=2.5
	ifont=4
	ilog=0
	itx=1			!normal tick orientation
	ity=1
	ntx=5
	nty=5
	ixlo=-1		!whole screen
	xcross=xmin
	ycross=ymin
	doframe=.false.
	landscap=.true.
	draft=.false.
	autplt=.false.
	fitted=.false.
c Now define ndat() etc appropriately for the EQUIV array input. Normal
c definition if number of points not greater than 2048, otherwise as
c below
	ncurvd=1
	icurvd(1)=1
	ndat(1)=npntig		!if <=2048, reotherwise defined below
	nar1=1			!if <=2048, reotherwise defined below
	ndat1(1)=npntig		!for Xval1 -up to 8192 at present
	isym(1)=0			!points
	ijoin(1)=-1		!points not joined
	symsiz(1)=0.3*cbig
	if(npntig.gt.2048) then
	   nar1=1 + (npntig-1)/2048	!number of contiguous 2048 point arrays
	   ncurvd=nar1
	   do 113 i=1,nar1
		icurvd(i)=i
		isym(i)=0
		ijoin(i)=-1
		symsiz(i)=0.3*cbig
		ndat(i)=2048		!nar1-1 'full' arrays -last one fixed below
113	   continue
	   ndat(nar1)=npntig-(nar1-1)*2048	!plus the odd points in last one
	endif
c calc curves:
	ncurvc=0		!none here
c
	iask=2	!do not ask before leaving display; leave graph on screen
c
30	continue	!return here to plot
c
c	if(debug()) then
c	   pause 'plot here'
c	   goto 705
c	endif
c
	call VPLOTR(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & Xcal,Ycal,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,vramp,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title,cbig,ifont,landscap,fitted,iask,
     & theta,ifitype,ncomp)
c
c705	continue
	if(fitted) then
	   if(refit) then
	      refit=.false.
		nfix=nfixsav
		fixtau=fixtau1
		fixamp=fixamp1
		do 325 i=1,20
325		jfix(i)=jfixsav(i)
	   endif
c 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   if (icurvd(1).eq.1) then
	      call LOCATE(0,0)		!top of page
	      print 321
321	      format('&Display data with fit subtracted [Y] ? ')
		read 101,ans
		if(UC(ans).ne.'N') then
		   ncurvd=nar2		!display residuals
	   	   do 1132 i=1,nar2
		   icurvd(i)=i+4		!=5,6,...
c		   if(debug()) then
c			print 1133,ncurvd,i,icurvd(i)
c1133			format(' ncurvd,i,icurvd(i)= ',3i8)
c			pause
c		   endif
1132		   continue
c		   icurvd(1)=2		!display residuals
		   icurvc(1)=2		!+ calc curve=zero line
	         ifitype=0		!omit display of exponential parameters
		   call FIXAX(yminr,ymaxr,ymin,ymax,ytic,0)
		   ycross=ymin
		   goto 30
		endif
	   endif
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 324
324	   format('&Fit O.K. (say N to refit or abandon) [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		call LOCATE(0,0)		!start typing at top of page
		print 66
66		format(
     &	'&Abandon; go on to next (say N to refit) [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   refit=.true.
c		   goto 298
		   goto 400
		else		!go on to next
 		   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		   refit=.false.
		   nfix=nfixsav
		   fixtau=fixtau1
		   fixamp=fixamp1
		   do 67 i=1,20
67		   jfix(i)=jfixsav(i)
		   goto 1			!next good bit
		endif
	   endif
c Alter istart(),iend() values? Even if cursors not used again below,
c must still alter istart(), iend() so that they correspond to the
c fitted region only.  The fitted values are in Yval1(i,j), i=1,ndat1(j), j=1
c (with corresponding subtracted values in j=2) but fitted only from #ifirst
c to #ilast.
c If req, can reduce length of good bit further by defining part of it
c to be used, as now follows.
	   i1delt=ifirst-1       !in case jump to 3291
	   i2delt=ndat(1)-ilast
	   itim1=xmin+float(ifirst-1)*dx
	   xx1=0.
	   xx2=xval1(ndat1(2),2)
	   call LOCATE(0,0)		!top of page
	   print 320
320	   format('&Redefine, with cursors, region to be used [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 3291
35	   call LOCATE(0,0)		!top of page
	   print 3011
3011	   format(
     & '&Mark 1st X value be used for spectrum with cursor; hit enter')
	   ix1=ISCRX(dx)
	   call PUTCUR(ix1,7000)		!start with cursor left
         ch=FNDCUR(ix1,iy1)		!define start point
	   call BELL(1)
         xx1=XWORLD(ix1)
	   if(xx1.lt.0.) xx1=0.	!1st point whether subtracted or not
	   iltype=3		!short dash
	   call MOVETO(xx1,ymin,0,iltype)
	   call MOVETO(xx1,ymax,1,iltype)
	   call LOCATE(0,0)		!top of page
	   print 3021
3021	   format(
     & '&Mark last X value be used for spectrum with cursor; hit enter')
	   ix2=ISCRX(xval1(ndat1(2),2))
	   call PUTCUR(ix2,7000)		!start with cursor right
         ch=FNDCUR(ix2,iy2)		!define end of arrow
	   call BELL(1)
         xx2=XWORLD(ix2)
	   if(xx2.gt.xval1(ndat1(2),2)) xx2=xval1(ndat1(2),2)
	   call MOVETO(xx2,ymin,0,iltype)
	   call MOVETO(xx2,ymax,1,iltype)
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
c299	   format(
c     & '&                                                           ')
	   call LOCATE(0,0)		!top of page
	   print 313,xx1,xx2
313	format(
     & '&Use data from X = ',g13.6,' to ',g13.6,' O.K. [Y] ? ')
	  read 101,ans
	   if(UC(ans).eq.'N') then
c           If not OK delete cursors and try again
	      call MOVETO(xx1,ymin,0,iltype)
	      call MOVETO(xx1,ymax,-1,iltype)
	      call MOVETO(xx2,ymin,0,iltype)
	      call MOVETO(xx2,ymax,-1,iltype)
		goto 35
	   endif
c Calc modified i1delt,i2delt here so corrected istart(),iend()
c are set properly below
c Calc index in Yval1 of the new 1st,last points to be used
	   i1=1 + ifixr(float(xx1-xmin)/dx)		!index of new 1st point
	   i2=1 + ifixr(float(xx2-xmin)/dx)		!index of new last point
	   i1delt=i1-1
	   i2delt=ndat1(1)-i2
	   if(debug()) then
		print 704,ifirst,ilast,i1,i2
		if(discprt) write(8,704) ifirst,ilast,i1,i2
704		format(' Original ifirst,ilast= ',2i8,' new = ',2i8)
	   endif
	   ifirst=i1
	   ilast=i2
c Calc, as time relative to the t=0 used for fitting, the time of the
c first point on the good bit of data now defined (=that with the
c modified istart() index in all data)
	   itim1=xmin+float(i1-1)*dx
c
c Mark a section on this 'good' bit as bad? (to be masked from spectrum
c if short enough)
3291	   continue
	   nomit=0
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 328
328	   format(
     & '&Mark a section of this as ''bad'' [N] ? ')
	   read 101,ans
	   if(UC(ans).ne.'Y') goto 329
36	   call LOCATE(0,0)		!top of page
	   print 306
c306	format(
c     & '&Mark first point to be omitted with cursor; hit enter')
	   call PUTCUR(5000,7000)		!start with cursor central
         ch=FNDCUR(ix1,iy1)		!define start point
	   call BELL(1)
         xx11=XWORLD(ix1)
	   if(xx11.lt.xx1+dx) xx11=xx1+dx
	   iltype=1		!dotted
	   call MOVETO(xx11,ymin,0,iltype)
	   call MOVETO(xx11,ymax,1,iltype)
	   call LOCATE(0,0)		!top of page
	   print 311
c311	format(
c     & '&Mark last point to be omitted with cursor; hit enter ')
	   ix2=ISCRX(xx11)+500
	   call PUTCUR(ix2,7000)		!start with cursor central
         ch=FNDCUR(ix2,iy2)		!define end of arrow
	   call BELL(1)
         xx21=XWORLD(ix2)
	   if(xx21.gt.xx2-dx) xx21=xx2-dx
	   call MOVETO(xx21,ymin,0,iltype)
	   call MOVETO(xx21,ymax,1,iltype)
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 308,xx11,xx21
c308	format(
c     & '&Omit from X = ',g13.6,' to ',g13.6,' O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
c           If not OK delete cursors and try again
	      call MOVETO(xx11,ymin,0,iltype)
	      call MOVETO(xx11,ymax,-1,iltype)
	      call MOVETO(xx21,ymin,0,iltype)
	      call MOVETO(xx21,ymax,-1,iltype)
		goto 36
	   endif
	   nomit=1		!signals that a bit omitted
329	continue
c Find the revised istart(ig),iend(ig) from xx1,xx2, and store parameters of
c fit for re-generating subtracted record after PSIN1. Possible (though
c unlikely) problem is that the revised start and/or end points could be
c in different sections from the original ones, so record the CHANGE in
c start/end point and apply this change directly to the orig start/end which
c is index in ALL data so sections do not matter.
c Note that Xval1(i1,1)=xmin+float(i-1)*dx, =xmin for i=1 up to
c xmin+(ndat-1)*dx for last point; thus i=1+(x-xmin)/dx =1 for x=xmin
c and i=1+([xmin+(ndat-1)*dx] - xmin)/dx = 1 + (ndat-1)*dx/dx = ndat for last
c point
	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,3) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 315,ig,istart(ig),iendig
	   if(pon()) write(7,315) ig,istart(ig),iendig
	   if(discprt) write(8,315) ig,istart(ig),iendig
315	   format(' Bit #',i4,'; original istart,iend = ',2i10)
	   if(i1delt.lt.0) then
		call BELL(1)
		print 3141,i1delt
3141		format('&RANGE CANNOT BE EXTENDED: i1delt = ',i8)
		i1delt=0		!so istart not changed
	   endif
	   istart(ig)=istart(ig)+i1delt		!changed istart(ig)
c	   i2=1 + ifixr(float(xx2-xmin)/dx)	!index of new last point
c	   i2delt=npntig-i2				!change in end point
	   if(i2delt.lt.0) then
		call BELL(1)
		print 3142,i2delt
3142		format('&RANGE CANNOT BE EXTENDED: i2delt = ',i8)
		i2delt=0		!so iend not changed
	   endif
	   iend(ig)=iendig-i2delt			!changed iend(ig)
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,3) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 314,ig,istart(ig),iend(ig)
	   if(pon()) write(7,314) ig,istart(ig),iend(ig)
	   if(discprt) write(8,314) ig,istart(ig),iend(ig)
314	   format(' Bit #',i4,';      new istart,iend = ',2i10)
c If area marked as bad, reset it to 1.e20
	   if(nomit.eq.1) then
c	      i1b=1 + ifixr(float(xx11-xmin)/dx)	!index of 1st bad point
c	      i2b=1 + ifixr(float(xx21-xmin)/dx)	!index of last bad point
cc Above are indices in Yval1(i,1), so the start and end of the bad
cc bit, as indices in all data (like istart,iend), are found as follows
cc (because Yval1(1,1) corresp to istart(ig))
c		ibstart=istart(ig)+i1b-1
c		ibend=istart(ig)+i2b-1
cc and corresp indices in Yval1(i,2) are:
c		i1b=i1b-ifirst+1		!so i1b=ifirst becomes i1b=1
c		i2b=i2b-ifirst+1		!ie indices in Yval(i,2)
c		if(i1b.lt.1) i1b=1
c		if(i2b.gt.8192) i2b=8192
c Above bit not quite right. Whether bits to be omitted are defined on
c (a) all data with fitted curve (graph starts at negative time) or on
c (b) the graph of subtracted data, it is true in ech case that the smallest
c time allowed is t=0. (or better t=dx so omitted part always within accepted
c part) so index of start and end of omitted part, relative to the point with
c t=0) must always be
	      i1b=1 + ifixr(xx11/dx)	!index of 1st bad point (=1 if xx11=0)
	      i2b=1 + ifixr(xx21/dx)	!index of last bad point
		ibstart=istart(ig)+i1b-1
		ibend=istart(ig)+i2b-1
	      do 330 i=i1b,i2b
330	      Yval1(i,2)=1.e20
      	print 333,xx11,xx21,i1b,i2b
	      if(pon()) write(7,333) xx11,xx21,i1b,i2b
	      if(discprt) write(8,333) xx11,xx21,i1b,i2b
333		format(
     & 	' Omit from spectrum from t= ',g10.3,' to ',g10.3,/,
     &	': i.e. points ',2i8)
	   endif
c323	   continue
c Write the current good bit, after subtraction of fitted curve, back to
c a temporary disc file to allow easy access to subtracted records after
c the resolution etc has been decided in PSIN1.  Use a TRANSPARENT file
c because fixed record length not convenient here. Subtracted data is
c in Yval1(i,2), i=1,ndat1(2), so modified range is Yval1(1+i1delt,2) to
c Yval1(ndat1(2)-i2delt,2). Write also the corresponding UNSUBTRACTED
c values from Yval1(i,1) to facilitate calc of mean current.
c Say this is the NGSUBth bit to be recorded on disc. We need to record
c (preferably on disc so can go back to reanalyse same good bits)
c (a) the start record for each good bit, and (b) the number of values
c that are recorded (c) which of the original good bits this corresponds
c to (in case some subtracted bits rejected at this stage and not
c written to disc, so ngsub<ngood)
c NVAL=NVALG(ngsub) subtracted values are recorded, occupying 4*nval bytes
c (records), and same number of unsubtracted =8*nval bytes altogether,
c starting at record (ie byte) #IRECG(ngsub), and corresponding to original
c good bit #ig in IGVAL(ngsub). Disk record starts with the
c integer*4 array IRECG(512)=2048 bytes, initialised to -1(NB record number
c may be too big for integer*2),and integer*2 NVALG(512) (1024 bytes) and
c IGVAL(512) (1024 bytes), so data starts at byte 4096+1=4097
c NB if usable area not further reduced with cursors then i1,i2 here are
c equal to ifirst,ilast, the fitted points in Yval1(i,1)
	i1=1+i1delt
	i2=ndat1(1)-i2delt
c Note that values in Yval1(i,2) do not correspond to those in Yval1(i,1)
c but Yval1(1,2) corresponds to Yval1(ifirst,1), so
	i3=i1-ifirst+1	!index in Yval1(i,2) starts at 1 if no further modif
	i4=i2-ifirst+1
c NB Yval1(i,2) corresponds to Yval1(i+ifirst-1,1)
	nval=i4-i3+1		!number of values to be written =i2-i1+1
	if(debug()) then
        print 702,i1,i2,i3,i4,i1delt,i2delt,ndat1(2),nval
        if(discprt) write(8,702) i1,i2,i3,i4,i1delt,i2delt,ndat1(2),nval
702	  format(' i1,i2,i3,i4,i1delt,i2delt,ndat1(2),nval= ',/,8i8)
	endif
	ngsub=ngsub+1		!number of good bits written
	igval(ngsub)=int2(ig)	!needed in PSIN1
      OPEN(unit=16,file=ndev//'\NOISPAR.DAT',status='UNKNOWN',
     &	access='TRANSPARENT')
c New format for disc write [size in bytes]:
c (1) In record 1 write current ngsub [4] each time; rests starts at rec 5
c (2) write the following each time, starting at record # 5+(ngsub-1)*116
c nmod [4], ncomp [4] ,theta [80], itim1 [4], igval [4], modified istart [4],
c and iend [4], nomit [4], ibstart[4],ibend [4], =116 bytes
c (nmod not used at present, but could be in future!)
	irec=5 + (ngsub-1)*116
	write(16,rec=1) ngsub
	write(16,rec=irec) ig,istart(ig),iend(ig),nmod,ncomp,theta,itim1,
     & nomit,ibstart,ibend
	CLOSE(unit=16)
      print 326,ngsub,irec
      if(pon()) write(7,326) ngsub,irec
      if(discprt) write(8,326) ngsub,irec
326	format(' Data for fit # ',i4,' stored from disc record ',i8)
	if(debug()) then	!print values written to disc to check read-back
        print 332,itim1,ig,istart(ig),iend(ig),nomit,ibstart,ibend
        if(discprt) write(8,332) itim1,ig,istart(ig),iend(ig),nomit,
     &	ibstart,ibend
332	  format(' Time of 1st point rel to t=0 for fit = ',i8,/,
     &  ' From good bit ',i4,', istart, iend now = ',2i10,/,
     &  ' nomit,ibstart,ibend = ',i3,2i10)
	  kmax=2*ncomp+1
	  do 331 i=1,kmax
	   print 327,i,titlep(i),theta(i)
	   if(pon()) write(7,327) i,titlep(i),theta(i)
         if(discprt) write(8,327) i,titlep(i),theta(i)
327	   format(' Parameter ',i2,': ',a10,'= ',g13.6)
331	  continue
	  print 709
709	  format(' Stop here and calculate spectrum [N] ? ')
	  read 101,ans
	  if(UC(ans).eq.'Y') goto 999
	endif
c
c      OPEN(unit=16,file=ndev//'\NOISFIT.DAT',status='UNKNOWN',
c     &	access='TRANSPARENT')
c	if(ngsub.le.1) then
c	   irec=4097		!first one
c	else
c	   irec=irecg(ngsub-1)+8*nvalg(ngsub-1)
c	endif
c	irecg(ngsub)=irec
c	nvalg(ngsub)=int2(nval)
c	igval(ngsub)=int2(ig)
c	write(16,rec=1) IRECG,NVALG,IGVAL
c	write(16,rec=irec) (Yval1(i,2),i=i3,i4),(Yval1(i,1),i=i1,i2)
c	CLOSE(unit=16)
c      print 326,ngsub,nval,irec
c      if(pon()) write(7,326) ngsub,nval,irec
c      if(discprt) write(8,326) ngsub,nval,irec
c326	format(' Good section # ',i4,' stored on disc: ',/,
c     & 1x,i5,' values starting at record # ',i6)
c	if(debug()) then	!print values written to disc to check read-back
c        print 332
c        if(discprt) write(8,332)
c332	  format(' i   Yval1(i,2)=subtracted  Yval1(i,1)=unsubtracted')
c	  do 331 i=1,nval
c        print 327,i,Yval1(i3+i-1,2),Yval1(i1+i-1,1)
c        if(discprt) write(8,327) i,Yval1(i3+i-1,2),Yval1(i1+i-1,1)
c327	  format(' i,Yval1(i,2),Yval1(i,1) = ',i8,3x,g13.6,5x,g13.6)
c331	  continue
c	endif
cc Save parameters; up to 5 (=2 exponentials) at present -NOT USED NOW
cc	   do 316 i=1,kmax
cc		thetsav(i,ig)=theta(i)
cc316	   continue
	   goto 1			!next good bit
	endif
c End of bit done when fitting completed
c
c Section done after initial display -fit it?
	call LOCATE(0,0)		!start typing at top of page
	print 300,ig
300	format('&Fit this display (event #',i4,') [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   i=1
	   print 296
296	   format(
     &   ' (1) Go on to next event',/,
     &   ' (2) Stop now and calculate spectrum',/,
     &   '  Option number [1] = ')
	   call INPUTi(i)
c	   read 4,i
c	   if(i.le.0) i=1
	   if(i.eq.2) goto 999
c
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   refit=.false.
	   nfix=nfixsav
	   fixtau=fixtau1
	   fixamp=fixamp1
	   do 64 i=1,20
64	   jfix(i)=jfixsav(i)
	   goto 1			!next good bit
c	   RETURN
	endif
298	continue
c Fit required so now use cursors to mark start and end of region to be fitted
c LINES BETWEEN POINTS- 0=solid; 1=dotted;',/,
c 2-6=inc length dashes; 7=dash-dot; 8=dash-dot-dot.',/,
37	call LOCATE(0,0)		!top of page
	print 301
301	format(
     & '&Mark first X value be used with cursor; hit enter')
	ix1=ISCRX(dx)
	call PUTCUR(ix1,7000)		!start with cursor left
      ch=FNDCUR(ix1,iy1)		!define start point
	call BELL(1)
      x1=XWORLD(ix1)
	if(x1.lt.0.) x1=0.
	x1=dx*float(ifixr(x1/dx))	!so x1 corresponds to the nearest point
	iltype=3		!short dash
	call MOVETO(x1,ymin,0,iltype)
	call MOVETO(x1,ymax,1,iltype)
	call LOCATE(0,0)		!top of page
	print 302
302	format(
     & '&Mark last X value be used with cursor; hit enter ')
	ix2=ISCRX(xval1(ndat1(1),1))
	call PUTCUR(ix2,7000)		!start with cursor right
      ch=FNDCUR(ix2,iy2)		!define end of arrow
	call BELL(1)
      x2=XWORLD(ix2)
	if(x2.gt.xval1(ndat1(1),1)) x2=xval1(ndat1(1),1)
	call MOVETO(x2,ymin,0,iltype)
	call MOVETO(x2,ymax,1,iltype)
	call LOCATE(0,0)		!top of page
	print 299		!blank line so previous top line deleted
299	format(
     & '&                                                             ')
	call LOCATE(0,0)		!top of page
	print 303,x1,x2
303	format(
     & '&Fit from X = ',g13.6,' to ',g13.6,' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
c           If not OK delete cursors and try again
	      call MOVETO(x1,ymin,0,iltype)
	      call MOVETO(x1,ymax,-1,iltype)
	      call MOVETO(x2,ymin,0,iltype)
	      call MOVETO(x2,ymax,-1,iltype)
		goto 37
	endif
	if(pon()) write(7,3031) x1,x2
	if(discprt) write(8,3031) x1,x2
3031	format(/,
     & ' Fitted from X = ',g13.6,' to ',g13.6)
c Omit a section?
	nomit=0
	call LOCATE(0,0)		!top of page
	print 299		!blank line so previous top line deleted
	call LOCATE(0,0)		!top of page
	print 304
304	format(
     & '&Omit a section from the fit [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 305
38	call LOCATE(0,0)		!top of page
	print 306
306	format(
     & '&Mark first point to be omitted with cursor; hit enter')
	call PUTCUR(5000,7000)		!start with cursor central
      ch=FNDCUR(ix1,iy1)		!define start point
	call BELL(1)
      x11=XWORLD(ix1)
	if(x11.lt.x1+dx) x11=x1+dx
	iltype=1		!dotted
	call MOVETO(x11,ymin,0,iltype)
	call MOVETO(x11,ymax,1,iltype)
	call LOCATE(0,0)		!top of page
	print 311
311	format(
     & '&Mark last point to be omitted with cursor; hit enter ')
	ix2=ISCRX(x11)+500
	call PUTCUR(ix2,7000)		!start with cursor just right of last pos
      ch=FNDCUR(ix2,iy2)		!define end of arrow
	call BELL(1)
      x21=XWORLD(ix2)
	if(x21.gt.x2-dx) x21=x2-dx
	call MOVETO(x21,ymin,0,iltype)
	call MOVETO(x21,ymax,1,iltype)
	call LOCATE(0,0)		!top of page
	print 299		!blank line so previous top line deleted
	call LOCATE(0,0)		!top of page
	print 308,x11,x21
308	format(
     & '&Omit from X = ',g13.6,' to ',g13.6,' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
c           If not OK delete cursors and try again
	      call MOVETO(x11,ymin,0,iltype)
	      call MOVETO(x11,ymax,-1,iltype)
	      call MOVETO(x12,ymin,0,iltype)
	      call MOVETO(x21,ymax,-1,iltype)
		goto 38
	endif
	nomit=1		!signals that a bit omitted
	if(pon()) write(7,3032) x11,x21
	if(discprt) write(8,3032) x11,x21
3032	format(/,
     & ' Omit points between X = ',g13.6,' and ',g13.6)
305	continue
c	pause		!for now
c Construct special 1-dimensional Yfit,Xfit arrays for fitting that contain
c only those points that are to be fitted
c  ALSO move t=0 to position of first cursor=x1 (its position doesn't matter
c much for this purpose) -also makes default guess for Y(0) easy; change
c xmax,xmin similarly
	t0=x1
	x1=x1-t0    !=0; NB Xval1 compared with x1,x2,x11,x21 AFTER x1 subtracted!
	xmin=xmin-t0
	xmax=xmax-t0
	xcross=xcross-t0
	x2=x2-t0
	x11=x11-t0
	x21=x21-t0
	iscal=0		!so xmin etc just defined are used
c NB 1st point fitted is the first point with t=>0 (on new scale), and
c this is Yval1(ifirst,1),Xval1(ifirst,1)
	ifirst=-1		!index of 1st point fitted
	ilast=-1		!index of last point fitted
	n=0
	do 310 i=1,ndat1(1)	!loop over all plotted points (NB not ndat(1))
c	   x=Xval1(i,1)
	   Xval1(i,1)=Xval1(i,1)-t0	!alter Xval1 so fit shown with new t=0
	   x=Xval1(i,1)
c	   if(debug()) print 703,i,n,x,x1,x2
c703	   format(' i,n,x,x1,x2= ',2i6,3g13.6)
	   if(x.lt.x1.or.x.gt.x2) goto 310			!omit from fit
	   if(nomit.eq.1) then
		if(x.ge.x11.and.x.le.x21) goto 310		!omit from fit
	   endif
	   n=n+1
c...	   xfit(n)=x
	   xfit(n)=float(n-1)*dx	!so exactly 0 for Yfit(1)=Yval1(ifirst)
	   yfit(n)=Yval1(i,1)
	   if(ifirst.eq.-1) ifirst=i		!1st point fitted
	   ilast=i					!last point fitted
310	continue
	nfit=n
	call LOCATE(0,0)		!top of page
	print 299		!blank line so previous top line deleted
	call LOCATE(0,0)		!top of page
c Now do the fit (stay in graphics until initial guesses done).
	nmod=0
53	continue
	if(ncomp1.eq.-1.or.refit) then
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 2,ncomp1
2	   FORMAT(
     &   '&Number of exponential components to be fitted [',i2,'] = ')
	   i=0
	   call INPUTi(i)
c	   read 4,i
	   if(i.ne.0) then
		ncomp=i
	   else
		ncomp=ncomp1
	   endif
	   kmax=2*ncomp+1		!no of parameters
	else
	   ncomp=ncomp1
	endif
c	negpar=.false.
c Must give explicit guesses if (1) refitting after unsatisfactory fit
c or (2) if more than 1 component, unless fixamp=true AND fixtau=true, in
c which case there is only one amplitude + Yinf to be fitted however
c many components there are.
c Thus internally calc guesses are theta(1)=tau(1) (unless ncomp=1 and
c fixtau=true), theta(2)=amp(1) and theta(kmax)=Y(inf)
	if(refit.or.(.not.(fixamp.and.fixtau).and.ncomp.gt.1)) then
	   specg=.true.
	else
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 611
611	   format('&Specify initial guesses explicitly [N] ? ')
	   read 101,ans
	   specg=UC(ans).eq.'Y'
	endif
	if(refit.and.fixtau) then
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 617
617	   format('&Fix the time constants as before [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		fixtau=.false.
	      do 231 i=1,ncomp
		jfix(2*i-1)=0		!tau=theta 1,3,5,7,9 (1 set above)
		nfix=nfix-1
231	      continue
	   endif
	endif
	if(refit.and.fixamp) then
	   call LOCATE(0,0)		!top of page
	   print 299		!blank line so previous top line deleted
	   call LOCATE(0,0)		!top of page
	   print 618
618	   format('&Fix relative amplitudes as before [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		fixamp=.false.
		do 232 i=1,ncomp
		jfix(2*i)=0		!unfix amplitudes 2,3,...,ncomp
		nfix=nfix-1
232		continue
	   endif
	endif
	if(.not.specg) then		!calc guesses here th(1)=tau(1), th(2)=amp(1)
c	   if(ncomp.eq.1) then
c	  Use mean of last 15% of points for Yinf
		i1=ilast-ifixr(0.15*float(ilast-ifirst))
		n=ilast-i1+1
		if(debug()) print 6122,istart(ig),iendig,is1,is2,
     &		ifirst,ilast,i1,n
6122		format(' istart(ig),iendig,is1,is2,ifirst,ilast,i1,n= ',
     &	2i10,6i8)
		s=0.
		do 612 i=i1,ilast
		s=s+Yval1(i,1)
c		if(debug()) print 6121,i,Yval1(i,1),s
c6121		format(' i,Yval1(i,1),s= ',i8,2g13.6)
612		continue
		theta(kmax)=s/float(n)			!Y(inf)
		if(debug()) then
		   print 6123,n,s,theta(kmax)
6123		   format(' n,s,th(kmax)= ',i8,2g13.6)
		   pause
	      endif
		if(abs(theta(kmax)).lt.0.01) theta(kmax)=0.01
		theta(2)=Yval1(ifirst,1)-theta(kmax)	!amplitude(1)
c		Tau estimate (very crude!):
		if(.not.fixtau) theta(1)=0.2*abs(Xval1(ilast,1)-
     &							Xval1(ifirst,1))
		if(ncomp.gt.1.and.fixamp) then
		   do 616 i=2,ncomp
616		   theta(2*i)=relamp(i-1)*theta(2)	!i-1=1,2,...
		   theta(2)=theta(2)/sumamp	!amp(1) calc as fraction of total amp
		endif
c		do 615 i=1,ncomp
c615		if(theta(i).lt.0.) negpar=.true.
c	   endif		!add here 'else if ncomp.eq.2' etc
	else					!ask for param guesses
	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
	   call LOCATE(lrow-1,0)		!row lrow-1 as have 2 lines to print
	   print 61
61	   FORMAT('&Initial guesses:')
	   if(.not.fixtau) then
		print 6
6		format(' (Param 1): tau(1) (ms)= ')
c		read 8,theta(1)
	      call INPUTr(theta(1))
8		format(g13.6)
c		titlep(1)='  tau(1) '
	   endif
c amplitude(1) always needed
	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 9,2,1
c	   read 8,theta(2)	 !amps= theta 2,4,6,8,10
	   call INPUTr(theta(2))
c	   if(theta(2).lt.0.) negpar=.true.
	   if(ncomp.gt.1) then
		do 50 i=2,ncomp
		j=2*i-1		!=3,5,7,9...
		if(.not.fixtau) then
	         call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
	         call LOCATE(lrow,0)		!row lrow,col 0
		   print 7,j,i
7		   format('&(Param',i2,'): tau(',i2,') (ms)= ')
c		   read 8,theta(j)	!tau=theta (1,) 3,5,7,9 (1 set above)
	 	   call INPUTr(theta(j))
		endif
		j=j+1			!=4,6,8,... for amps
		if(fixamp) then
		   theta(j)=relamp(i-1)*theta(2)	!i-1=1,2,...
		else
	         call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
	         call LOCATE(lrow,0)		!row lrow,col 0
		   print 9,j,i
9		   format(
     &        '&(Param',i2,'): Amplitude of component ',i2,' = ')
c		   read 8,theta(j) 	!amps= theta (2,) 4,6,8,10
	 	   call INPUTr(theta(j))
		endif
50		continue
	   endif
c Now get asymptote
40	   continue
	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 10,kmax
10	   format(
     &   '&(Param',i2,'): asymptotic value of Y = ')
	   read 8,theta(kmax)
	   call INPUTr(theta(kmax))
	   titlep(kmax)='  Y(inf) '
c	   if(theta(kmax).lt.0.) negpar=.true.
	endif
c Print model
	if(pon()) write(7,54) ncomp
      if(discprt) write(8,54) ncomp
54	format(/,' Exponentials fitted (',i3,' components)')
c
c Model number and initial guesses now in
	fitted=.true.
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
c	if(debug()) then
c	 do 700 i=1,nfit
c	 print 701,i,xfit(i),yfit(i)
c	 if(mod(i+1,20).eq.0) pause
c700	 continue
c701	 format(i8,2g13.6)
c	endif
	errfac=1.e-4
	delmin=-1.		!do not use delmin for convergence
	stpfac=0.1
c
c	print 525
c525	format('&Relative error [1.e-4]= ')
c	read 8,x
c	if(x.gt.1.e-10) errfac=x
cc	if(errfac.lt.0.) delmin=-errfac	!use delmin criterion
c
	if((.not.fixamp).and.(.not.fixtau)) then
	   nfix=0
	   do 1151 i=1,20
1151	   jfix(i)=0		!initialise
	   print 116
116	   FORMAT('&Number of parameters to be fixed [0]= ')
	   call INPUTi(nfix)
c	   read 4,i
c	   if(i.gt.0) NFIX=i
1161	   kfit=KMAX-NFIX
	   if(nfix.gt.0) then
		do 119 i=1,nfix
		print 118,i
118		format('&(',i2,'):   parameter #= ')
c		read 4,itemp
	      call INPUTi(itemp)
		JFIX(itemp)=1		!ELEMENT OF JFIX=1 IF PARAM FIXED
119		continue
	   endif
	endif
C
c  Check initial guesses: if any are neg make default to NOT constrain
c to be positive (SSDNOIS ignores CONSTR)
c	if(negpar) then
c	   constr=.false.
c	   ans='N'
c	else
c	   constr=.true.
c	   ans='Y'
c	endif
c	print 134,ans
c134	FORMAT('&Constrain parameters to be positive [',a1,'] ? ')
c	read 101,ans1
c	if(UC(ans1).eq.'N'.or.UC(ans1).eq.'Y') ans=ans1
c	constr=ans.eq.'Y'
c	ndisp=-1		!prints every iteration, only if debug on
	ndisp=40
c	print 523
c523	FORMAT(
c     & ' Print every Nth estimate [20] :  N= ')
c	read 4,i
c	if(i.ge.1) NDISP=i
c
	print 322
322	format(' Fitting . . . ')
	nevm=15000
	smin=SSDNOIS(kmax,THETA)		!for init guesses
	if(kmax.ge.7) print 5231,1,smin,(theta(I),I=1,KMAX)
	if(kmax.lt.7) print 5232,1,smin,(theta(I),I=1,KMAX)
5231	format(i8,g10.3,/,10g10.3,/,10g10.3,/)
5232	format(i6,7g11.4,/)
c
	call SIMPLEX(kmax,THETA,stpfac,errfac,nev,nevm,
     & smin,SSDNOIS,Ndisp,jfix,delmin)
C
	SRES=SQRT(SMIN/FLOAT(NFIT-kfit))	!ERROR S.D.
	print 5233
5233	FORMAT( ' END OF FITTING')
	IF(KMAX.GE.7) print 5231,NEV,SMIN,(THETA(I),I=1,KMAX)
	IF(KMAX.LT.7) print 5232,NEV,SMIN,(THETA(I),I=1,KMAX)
	if(pon()) write(7,5234)NFIT,SRES
      if(discprt) write(8,5234)NFIT,SRES
5234	FORMAT(' Number of points= ',I8,/,' Error S.D.= ',G13.6)
	do 39 i=1,kmax
	if(jfix(i).eq.1.or.(.not.fitted)) then
	   print 412,i,titlep(i),theta(i)
	   if(pon()) write(7,412) i,titlep(i),theta(i)
         if(discprt) write(8,412) i,titlep(i),theta(i)
412	   format(' Parameter ',I2,': ',a10,'= ',G13.6,' (fixed)')
	else
	   print 414,i,titlep(i),theta(i)
	   if(pon()) write(7,414) i,titlep(i),theta(i)
         if(discprt) write(8,414) i,titlep(i),theta(i)
414	   FORMAT(' Parameter ',I2,': ',a10,'= ',G13.6)
	endif
39	continue
c
	if(pon()) write(7,42)NEV,SMIN
      if(discprt) write(8,42)NEV,SMIN
	print 42,NEV,SMIN
42	FORMAT(' No of function eval= ',i8,/,' Minimum SSD=',G13.6,/,
     & ' Hit ENTER for display')
	read 4,i		!so waits for enter
c
c Now display fitted curve (and subtracted data?)
c Data sets; as last display
c Cannot plot residuals too on same graph, since data also around y=0 so
c points overlap too much- show later when spectra calc, or option to show
c now (for points in fitted range)
c	ncurvd=2
c	icurvd(2)=2
c -define ncurvd,icurvd later, when decided which curve to show
c	ncurvd=1    !show orig data + fit only
c	icurvd(1)=1
c But define residuals here, as j=2, in case they are needed for display
c NB data for curve #2 are put into Xval1(i,2),Yval1(i,2), i=1,2,...,8192
c and Xval1(i,2) is equivalent to Xval(i,j=5,6,7,8), i=1,2,...,2048
c so ndat(j) must be defined for j=5,6,7,8 even if curve #1 was less than
c 2048 points. Of course if curve #2 is <=2048 points could just put data
c into Xval,Yval in ordinary way but simpler to stick to Xval1, in case
c more than 2048 points needed.
	ndat(5)=ilast-ifirst+1		!if <=2048, reotherwise defined below
	nar2=1				!if <=2048, reotherwise defined below
	ndat1(2)=ilast-ifirst+1		!for Xval1 -up to 8192 at present
	isym(5)=0		!point
	ijoin(5)=-1		!points not joined
	symsiz(5)=0.3*cbig
	if(ndat1(2).gt.2048) then
	   nar2=1 + (ndat1(2)-1)/2048	!number of contiguous 2048 point arrays
	   do 1131 i=5,nar2+4
		isym(i)=0
		ijoin(i)=-1
		symsiz(i)=0.3*cbig
		ndat(i)=2048		!nar2-1 'full' arrays -last one fixed below
1131	   continue
	   ndat(nar2+4)=ndat1(2)-(nar2-1)*2048   !plus the odd points in last one
	endif
	if(debug()) then
         print 706,ndat1(1),ndat1(2),nar2
         if(discprt) write(8,706) ndat1(1),ndat1(2),nar2
706	   format(' ndat1(1),ndat1(2),nar2 = ',3i8)
	   do 707 i=1,8
         print 708,i,ndat(i)
         if(discprt) write(8,708) i,ndat(i)
708	   format(' ndat(i) = ',2i8)
707	   continue
	endif
	if(debug()) then
         print 124
         if(discprt) write(8,124)
124	   format(
     &'   i1     i   Xval1(i,2)    Yval1(i1,1)  Ycalc       Yval1(1,2)')
	endif
c NB Yval1(i,2) corresponds to Yval1(i+ifirst-1,1) and corresponding
c values of latter are needed for calc of mean current in main prog
	yminr=1.e32
	ymaxr=-1.e32
	do 312 i=1,ndat1(2)
	   i1=i+ifirst-1		!=ifirst,ifirst+1,...,ilast
	   Xval1(i,2)=Xval1(i1,1)
	   xv=Xval1(i,2)
	   Yc=Ycalcn(kmax,theta,-1)
	   Y=Yval1(i1,1)-Yc		!Yobs-Ycalc=resid
	   Yval1(i,2)=Y			!Yobs-Ycalc=resid
	   if(Y.lt.yminr) yminr=y
	   if(Y.gt.ymaxr) ymaxr=y
	   if(debug()) then
      	print 125,i1,i,Xval1(i,2),Yval1(i1,1),yc,Yval1(i,2)
	      if(discprt) write(8,125)i1,i,Xval1(i,2),Yval1(i1,1),
     &	yc,Yval1(i,2)
125		format(2i6,4g13.6)
	   endif
312	continue
c
c calc curves:
	ncalc=301
c NB calc curve must go from t=0 (the new origin fixed above) to the new
c xmax fixed above (or to the new upper limit of fit=x2?), so set xminc=0
	xminc=0.0
	xmaxc=x2
	DXcalc=(XMAXc-XMINc)/FLOAT(ncalc-1)
	ncurvc=1          !fitted curve only 1st time
	icurvc(1)=1
c	do 51 j1=1,ncurvc
c	icurvc(j1)=j1
c	j=icurvc(j1)
c	iline(j)=0		!continuous
c	ncal(j)=ncalc
	do 51 i=1,2
	iline(i)=0		!continuous
	ncal(i)=ncalc
51	continue	!end of j loop
c
	do 307 i=1,ncalc
	Xcal(i,1)=xminc+float(i-1)*dxcalc
	Xcal(i,2)=Xcal(i,1)
	i1=-1		!so Ycalcn uses Xv not X(i)
	Xv=Xcal(i,1)	!value of X (in COMMON) for YcalCN
	Ycal(i,1)=Ycalcn(kmax,theta,i1)
	Ycal(i,2)=0.0		!zero line
c	if(.not.debug()) goto 307
c	print 20,i,1,Xcal(i,1),Ycal(i,1)
c20	format(' i,1,xc(i,1),yc(i,1)= ',2i8,2g13.6)
307	CONTINUE	!end of i loop
c51	continue	!end of j loop
c	iask=-2	!do not ask before leaving display; erase graph
	ifitype=4		!display of exponential parameters
	goto 30		!plot it

c
1	continue		!end of loop for each good bit
c
999 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	RETURN
	end


