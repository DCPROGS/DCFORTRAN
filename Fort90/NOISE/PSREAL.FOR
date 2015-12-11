	program PSREAL
c 09/16/93 06:17pm Lahey V5 version.
c 08/30/90 01:25pm-Heidelberg. Lahey version
c To be done
c =====test ibcor=1 properly -esp good bits that overlap sections
c =====incorporate Hanning option
c Modified 02/07/91 08:17am to read simulated EPSCs (ibcor=3)
c Changes from PDP version include
c (1) Maximum number of points/spectrum doubled to 2048 (NB use VPLOTR
c	rather than VPLOT1, to take 2048 points!). Up to 512 bad bits
c	definable. Upto 1024 spectra averaged
c (2) option added to subtract fitted curve ( rather
c	than just the mean) from each good bit (eg for Stern's EPSC decays)
c	Case where FITTED EXPONENTIALS TO BE SUBTRACTED requires special
c consideration; in this case there will rarely, if ever, be any chance
c to concatenate 2 or more good bits separated by a few masked bad points,
c and there is one fit for each good bit; but it is possible that the
c good bit is long enough to provide more than one spectrum and can
c mask a short bad bit in the middle of a single EPSC, bad bit is marked in
c NOISFIT and set to 1.e20. and positions noted in NOISPAR.DAT.
c
c Prog to calc spectra (and do variance/mean plots?- for latter
c we need a number for the background variance and for baseline
c current, to subtract from those in presence of agonist!)
c ISTART,IEND contain indices, in whole data array, of start and end of each
c GOOD bit. Individual points located by their index=IN in the total
c data set on disc (in=1,2,...,ndata), though this has to be read in in sections.
c The point with index=IS, idata(is), in section number ISEC has index
c	in = is + (isec-1)*npsec
c Conversely point with index=IN in the whole data set occurs in section
c	isec = 1 + (in-1)/npsec
c and has index, within this section,
c	is = in - (isec-1)*npsec
c Display starts at point with index is=isfst in current section so point
c with index=ID on display (1st displayed point=1) has index in section of
c	is = id + isfst - 1
c Similarly incurs,iscurs,idcurs= index in whole array, section, display resp
c of the data point nearest to the cursor.
c	For 2-channel data ADC0 is in idata(1,is), ADC1 is in idata1(2,is) for
c is=1,2,..,npsec where npsec is half the value it would have for nchan=1, ie
c nmax/2; total number of ADC0 data points=nint=ndata/2, and index of a
c data point among all data points, in=1,2,..,nint will be given same eqns
c as above (as long as appropriate npsec used, and we still read nmax points
c for each section). Same is true for ADC1 values.
	integer istart(512),iend(512),npnt(512)
c	integer*4 irecg(512)			!to keep subtracted bits on disc
c	integer*2 nvalg(512)			!to keep subtracted bits on disc
	integer*2 igval(512)
c	integer thetsav(5,512)		!keep parameters
	real*4 PS(2048)
	real*4 varsav(1024),ybsav(1024),timsav(1024)
	real*4 ymsav1(1024),ymsav2(1024)
c	real*4 varsv1(1024),ybsav1(1024)
c	real*4 xcal(2048,3),ycal(2048,3)
c	real*4 freq(2048)
	real*4 YSPEC(4096)	!for spectrum
	real*4 theta(20)
	real*4 Ymc(4096)	!for mean current display
	real*4 Yraw(8192)		!to read data for fitted bits
c	real*4 Ysub(4096),Yraw(4096)	!to read back subtracted & unsubtracted data
	character*1 ans,UC
	character CONDAT*30,ndev*2
	character*11 cdate,ctime
	character*32 filnam
	character*74 mtitle
	character*76 titdat		!data title
	logical madc1,restart,contin,psprt,reject
	logical discprt,pon,slock,debug,caplock,mono
	integer*2 idata(81920)
	integer*2 idata1(2,40960)     !equivalence for sep of 2-channel data
C For DCMENU
c	integer Lb(18)
c	character*78 text(18)
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,fixamp,relamp(10)
	EQUIVALENCE (idata,idata1)
c
c define function
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c
	filnam='PSREAL.PRT'
	OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT	!print file control
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' PSREAL: To calculate raw noise spectrum',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
	mono=jcol.eq.0	!monochrome screen
c defs for DCMENU
c	nbox=10	!number of boxes for DCMENU
c	icol1=14	!yellow text/border for boxes in DCMENU
c	icol2=8	!grey background for boxes
c	icol3=12	!red text/border
c	icol4=15	!white fill
c	if(mono) then
c	   icol3=15		!white text/border
c	   icol4=0		!black fill
c	endif
c	ifonb=0
c	csizb=1.7
c	ifont=4
c	csize=2.
c	ivxlo=0		! for VIEWPORT
c	ivxhi=10000
c	ivylo=0
c	ivyhi=7000
c	ixlo=0		! for graphboundary
c	ixhi=10000
c	iylo=0
c	iyhi=7000
c===
c	nbox=15	!number of boxes for DCMENU
c	call SETLBOX(nbox,Lb,1)
c	call NUMSET			!set num lock on ready for response
c	call DCMENU(0,4,Lb,text,0,0)		!delete all
c	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c
c
c Initialisation
c	NBUF=10240	!=DIMENSION OF ARRAY,IBUF,TO BE DISPLAYED
c	nmax=4096	!max nsize=dimension of YSPEC
c	nmax2=nmax/2	!max no of points in PS
c	nmax=81920		!dimension of idata()
c	npsmax=4096		!dimension of Yspec
c	npsec=nmax/nchan
	idiskq=-1	!disc for queue (until defined in VPLOT)
	nmax=81920
	npfmax=4096		!dimension of Yspec (was nmax in PDP version)
	npsmax=2048		!max number of points in PS (nmax2 in PDP version)
	do 30 i=1,npsmax
30	PS(i)=0.0	!initialise
c	do 31 i=1,512
c31	irecg(i)=-1		!no longer used
	ngsub=0
	nspec=0		!no of spectra calc
	VARV=0.0	!ACCUMULATES VARIANCES
	cmean=0.0	!accum mean current
	cmean1=0.0	!accum mean obs current minus asympote (ibcor=2)
	cmean2=0.0	!accum mean fitted current minus asympote (ibcor=2)
c NB IDEST reset below so use idest1 as arg for PSWRT to control read/write
c to scratch file
c Specify method to be used
	ibcor=1	!default baseline correction=subtract mean
	print 139,ibcor
139	format(
     & ' (1) Subtract mean from each record (stationary)',/,
     & ' (2) Subtract fitted curve each record (nonstationary)',/,
     & ' (3) Nonstationary analysis on simulated data (EPSCSIM)',/,
     & ' Option number [',i2,'] = ')
	read 4,i1
4	format(i8)
	if(i1.eq.1.or.i1.eq.3) ibcor=i1
c Note: for non-stationary noise, ibcor=2, the mean current will normally
c come from the same record that has been fitted since the DC record must
c be sampled on ADC0, and sampling on ADC1 not normally necessary (though
c could sample same DC record on ADC1 with a different filter). Thus, if
c nchan=2 best to ask if mean current is to come from ADC1 or not.
c Also for nonstationary case could get the CONTROL mean current, to be
c subtracted, from the fitted asymptote (or pre-EPSC record), rather
c than from a separate stretch of record used for getting control noise
c spectrum -former will be more accurate.
c Mean current record will be available always if nchan=2; if nchan=1
c then must ASK whether ADC0 is to be used for mean current or not? -not
c really necessary to ask -may as well just take it (though not useful
c if ADC0 is recorded on AC).
c	In NOISFIT usually (madc1=false) both subtracted data (for noise) and
c unsubtracted data (for mean current) taken from the same channel viz ADC0,
c assumed to have a DC-recorded EPSC etc on it. No need sample on ADC1 at all.
C NOT YET DONE:
c However it may be convenient to have Butterworth-filtered DC recording
c on ADC0 for noise, AND identical sample on ADC1, but Bessell filtered for
c easier fitting, and mean current, calculation (less ringing). Latter case
c assumed if MADC1 is true.
c
c    If curve to be fitted then final range of good bit not yet decided -the
c istart,iend from NOISED just define approx areas to be fitted, so if ibcor=2
c then do fits before deciding resolution etc
c
c Read data file and get total number of integers, ndata, and number per
c channel, nint; also read istart,iend.
 	call PSIN(istart,iend,ndata,nint,ngood,nchan,ndev,rate,filt,
     & filt1,calfac,calfac1,CONDAT,ioff,titdat,ibcor,nepsc,madc1,idest)
	if(idest1.eq.1) goto 48
	if(idest1.eq.999) goto 999
	if(ibcor.eq.2.or.ibcor.eq.3) then
	   if(pon()) write(7,155) ndev
	   if(discprt) write(8,155) ndev
155	   format(' Results of fitting put in NOISPAR.DAT on disc ',a2)
	endif
	if(ibcor.eq.3) goto 50
c
	print 20
20	format(' Print indices of good bits [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
c	if(debug()) then
	  print 705,ngood
        if(discprt)write(8,705) ngood
c705	  format(' Number of good bits = ',i8)
	  do 706 i=1,ngood
	  np=iend(i)-istart(i)+1
	  print 111,i,np,istart(i),iend(i)
        if(discprt)write(8,111) i,np,istart(i),iend(i)
111	  format(1x,i5,' :number of good points= ',i12,/,1x,
     & ' (point ',i10,' to point ',i10,')')
706	  continue
	endif
c	Calc the number of points in the last section, so do not attempt
c to read past the end of the data file.  Except for last section we
c read nmax values per section
	nsec= 1 + (ndata-1)/nmax   	!number of sections [=1+(nint-1)/npsec]
	nread=ndata - (nsec-1)*nmax	!number of idata in last section
	npsec=nmax/nchan		!number of points/section
	nlast=nread/nchan		!number of values per channel in last section
c
50	continue		!jump here for ibcor=3
	restart=.false.
	if(ibcor.eq.2.or.ibcor.eq.3) then
	   print 40
40	   format(/,
     &   ' (1) Fit exponentials to good bits (normal analysis)',/,
     &   ' (2) Continue previously started fitting',/,
     &   ' (3) Recalculate spectra from previously fitted data',/,
     &   ' Option number [1] = ')
	   read 4,iopt
	   if(iopt.lt.1.or.iopt.gt.3) iopt=1
	   contin=iopt.eq.2
	   restart=iopt.eq.3
	   if(restart) goto 41
c	If we restart then need ngsub, and altered istart,iend so PSIN1 can calc
c the	number of spectra for specified resolution -read in PSIN1
c If continuing previous fitting then read necessary things in NOISFIT
	   call NOISFIT(istart,iend,ngood,idata,idata1,nchan,
     &   nread,nmax,nsec,npsec,isec,rate,calfac,calfac1,ioff,CONDAT,
     &   npnt,ngsub,igval,ndev,madc1,contin,ibcor,nepsc,idiskq)
	endif
c
41	continue
c Get resolution etc for spectra (and define NMASK)
	call PSIN1(rate,rate0,filt,fn,nskip,res,nsize,nsizet,
     & nmask,ipmon,errfac,istart,iend,ngood,nchan,npsmax,npfmax,nsmax,
     & ibcor,ngsub,nspecbit,igval,ndev,restart,contin,psprt)
	npspec=nsize/2		!no of points in spectrum
c
	dx0=1.0/rate0		!time between points in seconds
	if(debug()) then
	  print 705,ngood
        if(discprt) write(8,705) ngood
705	  format(' Number of good bits = ',i8)
	  do 702 i=1,ngood
        if(discprt) write (8,701)i,istart(i),iend(i)
702	  print 701,i,istart(i),iend(i)
701	  format(i5,2i10)
	endif
c Now the spectra. Count (as IBAD) the number of bad points
c in the current spectrum. If this is below 2 percent of points
c mask them out.
c NB NSIZET=no of contiguous good (or maskable) points needed to get
c NSIZE points in YSPEC so nsizet=nsize*nskip (set in PSIN1)
c Go through good bits in turn, starting with the first, and check whether
c NSIZET consecutive points can be found that contain few enough bad bits
c to be masked out
	tsec=float(npsec)*dx0		!seconds per section
c NB if nskip>1 then RATE is the reduced value; RATE0 is orig value
c Read spectrum and set bad points to 10001
c
	j=0	!index for Yspec
	ibad=0	!number of bad (but maskable) points in Yspec (=<nmask)
c Mean current for current during each period used for a spectrum (and
c a variance value) is recorded (any bad=masked points are excluded).
	ymsum=0.0	!to accumulate mean current (exc bad=masked points)
	nmsum=0     !n for ditto
c Mean Time at which each mean current occurs is recorded. Time is
c measured from 1st point in CONSAM for regular (stationary) noise. For case
c non-stationary case, time from start of the fitted period is recorded.
	tsum=0.0	!to accumulate time for mean current
	ntsum=0     !n for ditto
c
c
	if(ibcor.eq.2.or.ibcor.eq.3) goto 300	!DO SUBTRACTED FITS SEPARATELY
c
	do ig=1,ngood
c Read each good bit
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
	isec1=1+(istart(ig)-1)/npsec		!section for start point
	isec2=1+(iend(ig)-1)/npsec		!section for end point
	npnt(ig)=iend(ig)-istart(ig)+1
	do 5 is=isec1,isec2
c read in section # is, if not already in
	   if(is.ne.isec) then
		ipsec=(is-1)*npsec		!to avoid repeated recalc
		call READSEC1(is,idata,nread,nsec,ioff,nmax,CONDAT)
		isec=is
	      tsect=tsec*float(is-1)	!time to 1st point of section
	   endif
	   if(is.eq.isec1) then
		is1=istart(ig)-ipsec		!index of start point within section
	   else
		is1=ilast+nskip-npsec		!ditto after 1st sec (=1 if nskip=1)
	   endif
	   if(is.eq.isec2) then
	      is2=iend(ig)-ipsec	!index within section of last point
	   else
		is2=npsec		!more section(s) to be read
	   endif
	   ichan=1		!use ADC0
c
	   nlast=is1+nskip*((is2-is1)/nskip)	!last i value actaully used
	   do 11 i=is1,is2,nskip
	    j=j+1
	    if(j.eq.1) then
		isec0=is		!keep section and point # for print out
		ip0=i
	    endif
c	  Record time from start of CONSAM sample
	    tsum=tsum + tsect + float(i)*dx0
	    ntsum=ntsum+1
c	  Record values for spectrum  and mean current
c 	NOTE that YMC(j) is value of 'mean current' corresponding to each
c	value used for spectrum (in YSPEC): if nchan=1 so 'mean current'
c	is taken from same signal as used for calc of spectrum then YMC and
c	YSPEC are identical
	    call GETCUR(ys,ym,i,madc1,nchan,idata,idata1,calfac,calfac1)
	    Yspec(j)=ys
	    ymc(j)=ym
	    ymsum=ymsum + ym
	    nmsum=nmsum+1
c		In original version only every nskip'th current value averaged
c		but might as well average all of them,for mean current.
c		Ymc(j) is used only for display of mean current and contains, at
c		present, only every nskip'th value of current
c		NB if nchan=1 then madc1 is false so mean current taken from ADC0
	    if(nskip.gt.1) then	  !add all points to ymsum (point #i already in)
		do 309 k=i+1,i+nskip-1
		call GETCUR(ys,ym,k,madc1,nchan,idata,idata1,calfac,calfac1)
		ymsum=ymsum + ym
		nmsum=nmsum+1
309		continue
	    endif
c
	    if(j.eq.nsize) then		!enough for 1 spectrum completed
		isecn=is		!keep last section,point for printout
		ipn=i
		nspec=nspec+1	!not in CALCPS now
		call CALCPS(Yspec,Ymc,nspec,nsize,npspec,nchan,ibad,
     & 	ymsum,nmsum,tsum,ntsum,isec0,ip0,isecn,ipn,ig,PS,
     & 	varv,cmean,varsav,ybsav,timsav,calfac,calfac1,
     &	ipmon,errfac,ibcor,ym1,ym2,reject,psprt)
	      if(reject) nspec=nspec-1		!reverse increment
		if(nspec.eq.nsmax) goto 400		!display averaged spectrum
		j=0		!carry on with current good bit, starting new Yspec
	    endif
	    ilast=i		!in case another section needed, and nskip not 1
11	   continue
5	   continue		!read next section, if nec
c All of good bit #ig now read in.  0,1,2,... spectra may have been calc
c from this good bit already, but unless j=0 here there are more good
c points (but fewer than nsizet) that have not yet been used. Check
c whether the next bad bit is short enough to be masked
	   if(j.gt.0.and.ig.lt.ngood) then
		nbad=(istart(ig+1)-1)-(iend(ig)+1) + 1
		ib1=iend(ig)+1	!1st point of next bad bit
		ib2=istart(ig+1)-1	!last point of next bad bit
		nbad=(ib2-ib1+1)
		nbad=(nbad+nskip-1)/nskip	!if nskip>1 need this
		ibad=ibad+nbad
		if(ibad.le.nmask) then     !continue with present spectrum
c		NB actual values not needed, so don't need to read in sections
c		just allocate nbad points to Yspec
	   	   do 12 i=1,nbad,nskip
	    		j=j+1
			Yspec(j)=1.e20		!to signify bad points
	    		if(j.eq.nsize) then		!enough for 1 spectrum completed
			  isecn=is		!keep last section,point for printout
			  ipn=i
			  nspec=nspec+1	!not in CALCPS now
			 call CALCPS(Yspec,Ymc,nspec,nsize,npspec,nchan,ibad,
     & 		  ymsum,nmsum,tsum,ntsum,isec0,ip0,isecn,ipn,ig,PS,
     & 		  varv,cmean,varsav,ybsav,timsav,calfac,calfac1,
     &		  ipmon,errfac,ibcor,ym1,ym2,reject,psprt)
	      	  if(reject) nspec=nspec-1		!reverse increment
			  if(nspec.eq.nsmax) goto 400		!display averaged spectrum
			  j=0		!carry on with next good bit, starting new Yspec
	    		endif
c		   NB tsum incremented here, but not ymsum (which should not include
c		   bad values, so need sep nsum for each
			tsum=tsum + tsect + float(i)*dx0
			ntsum=ntsum+1
12		   continue
		else		!too many bad points -start new spectrum
		   j=0			!start new Yspec
		   ibad=0         	! at next good bit
		   ymsum=0.0
		   nmsum=0
		   tsum=0.0
		   ntsum=0
		endif
	   endif
	enddo		!end of loop for each good bit
c1	continue		!end of loop for each good bit
c Spectra all finished: display average spectrum
	goto 400		!display averaged spectrum
c
c
c SECTION TO GET SPECTRA FROM EPSCs etc FROM WHICH FITTED TIME-COURSE HAS
c been subtracted, the subtracted records, ngsub in number, having been
c written to disc in NOISFIT
c For now assume (a) separate good bits are so far separated in data that
c they cannot be combined, (b) any bad points have been set to 1.e20 (not
c more than one bad section can be defined in NOISFIT).  Thus need to get
c NSIZE points in Yspec (skipping some points if nskip>1, in which case
c need NSIZET original data points).
c    Mean current, in ymc(), -can get via modified istart(ig),iend(ig)
c where ig= # of the original good bit from which subtracted data derives,
c stored in IGVAL(); for non-stationary records have nchan=1 normally and
c mean current comes from the record of the EPSC itself (though allow
c for nchan=2 because same EPSC might be recorded on ADC1, eg with
c different filtering). However calc of mean current this way is hard work
c if data overlaps sections, so corresponding unsubtracted (mean current)
c data has been written to disk along with subtracted data in NOISFIT
c and is read back into Yraw() below
c	 If subtracted data rejected before writing to disc
c will have ngsub<ngood. [ig=1,2,...,ngood corresponds to the current
c subtracted record, in=1,2,...,ngsub]
300	continue
	dx01=dx0*1000.	!time in MILLISECONDS for ibcor=2 section
      OPEN(unit=16,file=ndev//'\NOISPAR.DAT',status='UNKNOWN',
     &	access='TRANSPARENT')
c Loop through subtracted bits
	do 301 in=1,ngsub
	irec=5 + (in-1)*116
c	write(16,rec=1) ngsub
	read(16,rec=irec) ig,istart2,iend2,nmod,ncomp,theta,itim1,
     & nomit,ibstart,ibend
c	if(debug()) then
c	   print 46,ig,istart2,iend2,ncomp,itim1,
c     &	nomit,ibstart,ibend,(theta(i),i=1,5)
c	   if(discprt) write(8,46)ig,istart2,iend2,ncomp,itim1,
c     &	nomit,ibstart,ibend,(theta(i),i=1,5)
c46	   format(' ig,istart2,iend2,ncomp,itim1,nomit,ibstart,ibend=',
c     &   /,8i8,/,	' theta(1:5) = ',5g13.6)
c	endif
	kmax=2*ncomp+1
	nval=iend2-istart2+1	!modified istart,iend
	if(nval.lt.nsizet) then
	   call BELL(1)
	   print 303,in,ig,nval
	   if(debug().and.discprt) write(8,303) in,ig,nval
303	   format(
     &   ' Subtracted record # ',i4,' from good bit # ',i4,/,
     &   ' has ',i5,' points; too short for a spectrum')
	   goto 301		!next good bit
	endif
c
c Next read entire subtracted bit (some of which may have been set bad)
c from CONSAM into Yraw(8192) (all points -deal with nskip later), and set
c bad points, if any, to 1.e20.
c	In case of simulated data (ibcor=3) read back each EPSC separately
c from CONSIM.DAT; read EPSC #ig and use points istart2 to iend2
	if(ibcor.eq.3) then
         OPEN(unit=17,file=CONDAT,status='UNKNOWN',
     &   access='TRANSPARENT')		!open CONSIM.DAT
c 	 For CONSIM 'header' = 12 bytes, and each EPSC= kchan (number of
c      channels added) + nepsc data values, takes 4*(1+nepsc) bytes
c      so event #iepsc starts at irec=13 + (iepsc-1)*4*(1+nepsc)
	   irec=13+(ig-1)*4*(1+nepsc)
	   read(17,rec=irec) kchan,(Yraw(i),i=1,nepsc)	!read whole EPSC
	   CLOSE(unit=17)
	   j=0	!index for Yraw
	   do 81 i=istart2,iend2
	   j=j+1
	   Yraw(j)=Yraw(i)	!move good bit to start of Yraw()
81	   continue
c Note that ibstart,ibend are defined relative to modified istart,iend
c so must get good bit in Yraw BEFORE setting omitted bit to 1.e20
	   if(nomit.eq.1) then
		do 80 i=ibstart,ibend
80		Yraw(i)=1.e20
	   endif
	   goto 82
	endif
c
c Now normal (ibcor=2) section
	isec1=1+(istart2-1)/npsec		!section for start point
	isec2=1+(iend2-1)/npsec		!section for end point
	if(nomit.eq.1) then
	   ibsec1=1+(ibstart-1)/npsec		!section for start of bad bit
	   ib1=ibstart-(ibsec1-1)*npsec	!index of 1st bad point in this sec
	   ibsec2=1+(ibend-1)/npsec		!section for end of bad bit
	   ib2=ibend-(ibsec2-1)*npsec		!index of last bad point in this sec
	endif
	j=0					!index for Yraw
	do 75 isec=isec1,isec2
c read in section # is, if not already in
	   ipsec=(isec-1)*npsec		!to avoid repeated recalc
	   call READSEC1(isec,idata,nread,nsec,ioff,nmax,CONDAT)
	   if(isec.eq.isec1) then
		is1=istart2-ipsec		!index of start point within section
	   else
		is1=1				!ditto after 1st sec
	   endif
	   if(isec.eq.isec2) then
	      is2=iend2-ipsec		!index within section of last point
	   else
		is2=npsec		!more section(s) to be read
	   endif
c
	   do 711 i=is1,is2
	    j=j+1
	    if(j.eq.1) then
		isec0=isec		!keep section and point # for print out
		ip0=i
	    endif
c	  Record values for spectrum  and mean current; in this case (madc1=false)
c	  GETCUR returns ys=ym=current from ADC0
	    call GETCUR(Ys,Ym,i,madc1,nchan,idata,idata1,calfac,calfac1)
	    Yraw(j)=Ys
c If there is an omission set it to 1.e20 here (this assumes that start
c and end of a bad bit are in same or adjacent sections)
	    if(nomit.eq.1) then		!check if any of these points bad
		if(ibsec1.eq.ibsec2) then !all bad in current sec
		  if(isec.eq.ibsec1.and.i.ge.ib1.and.i.le.ib2) Yraw(j)=1.e20
		else if(isec.eq.ibsec1.and.i.ge.ib1) then
			Yraw(j)=1.e20		!set bad up to end if ibsec1
		else if(isec.eq.ibsec2.and.i.le.ib2) then
			Yraw(j)=1.e20		!set bad up to point #ib2
		endif
	    endif
711	   continue
75	continue
c
82	continue	!jump here for ibcor=3 with Yraw() defined
c YRAW(i) now in, so see how many spectra can be got from it. Also calculate
c for each spectrum the following -have 2 more sorts of mean current than
c in the ibcor=1 method
c (1) Mean current as mean of the observed currents (in Yraw()) used for
c	spectrum (exc an masked bad points)
c (2) Corrected ditto, from which the asymptote, in theta(kmax), has
c	been subtracted (kept in ymsav1(nspec), cmean1)
c (3) The mean of the FITTED current values over the range t1 to t2
c	where t1,t2 are the times (from t=0 used for fit) for the first and
c	last point used for the spectrum (no special allowance for bad bits
c	included in this at present).  This is integral, from t1 to t2
c	of the fitted function, divided by T=t2-t1, minus the asymptote
c	ie sum (over 1,..,ncomp) of w(i)*tau(i)*[exp(-t1/tau(i)-exp(-t2/tau(i)]
c	for numerator (kept in ymsav2(nspec), cmean2)
c
c (4) The mean time, from t=0 for fit, of each spectrum for mean or var
c	vs time plots, as bfore  Can just calc as (t1+t2)/2. in this case (tsum,ntsum
c	not used here; set ntsum=0 so mean not calc in CALCPS)
c
	ycrit=1.e19		!bad values set to 1.e20
	inext=1	!next point to look at
	nend=inext+nsizet-1	!up to here gives nsizet points
	ispecbit=0	!to count # of spectra from current fitted bit
307	continue	!return to here to look for more spectra in same good bit
c Get mean current from Yraw(i) [could also take the elements of idata(), that
c correspond to inext,nend; the first value in the subtracted record (inext=1)
c corresponds to the (modified) istart(ig) -but harder work!]
c Initialise things that start afresh for each spectrum
	j=0
	ibad=0
	ymsum=0.0	!to accumulate mean current (exc bad=masked points)
	nmsum=0     !n for ditto
	tsum=0.0	!to accumulate time for mean current
	ntsum=0     !n for ditto
	ilast=inext+nskip*((nend-inext)/nskip)	!last i value actaully used
	if(debug())then
	   print 713
	   if(discprt) write(8,713)
713	   format(
     &'    i       j      time       Yraw(i)      Ycalc       Yspec(j)')
	endif
	do 302 i=inext,nend,nskip
	   j=j+1			!index for point in spectrum
	   if(j.eq.1) ip0=i   !keep 1st point # for print out
	   ipn=i		   !keep last point number for print out
c Record current for spectrum. Must first subtract calc value to
c get Yspec. First point in this good bit (Yraw(in)) was at time t=itim1,
c relative to t=0 for the fit so point #i is at time=xv
	   t=float(i-1)*dx01	!in msec
	   xv=itim1+t		!time in common for Ycalcn
	   Yc=Ycalcn(kmax,theta,-1)
	   Yspec(j)=Yraw(i)-Yc
	   if(debug()) then
		print 712,i,j,xv,Yraw(i),Yc,Yspec(j)
	      if(discprt) write(8,712)i,j,xv,Yraw(i),Yc,Yspec(j)
712	      format(2i8,4g13.6)
	   endif
	   if(j.eq.1) tstart=xv	!time from t=0 for 1st point in spectrum
	   if(j.eq.nsize) tend=xv	!time from t=0 for last point in spectrum
c	  Record sum of times from start of subtracted bit (to get mean time)
	   tsum=tsum + t
	   ntsum=ntsum+1
c	  Record current for calc of mean current (and Ymc for display)
	   if(yraw(i).gt.ycrit) then       !count bad values
		ibad=ibad+1
	   else		!get mean current from good values
		if(nskip.eq.1) then
		  y=Yraw(i)		!mean current
		  ymc(j)=y
		  ymsum=ymsum + y
		  nmsum=nmsum+1
		else
		  ymc(j)=Yraw(i)
		  do 310 j1=i,i+nskip-1		!add in all nskip values
		  if(j1.ge.ilast) goto 310     !but only up to Yraw(ilast)
		  y=Yraw(j1)	!mean current
		  ymsum=ymsum + y
310		  nmsum=nmsum+1
		endif
	   endif
c
	   if(ibad.gt.nmask) then
c	too many bad points- look for next good point and start again from it
c	(a 'good' section can contain not more than one omitted=bad bit, so
c	there can be no more omitted points between here and end of this good bit)
		do 304 i1=i,nsizet	!from present posn to end
304		if(Yraw(i1).lt.ycrit) goto 305	!good point found, #i1
		goto 301	!no good point found so start on next fitted bit
305		inext=i1
		nleft=nval-inext+1	!number of points left (all good)
		if(nleft.lt.nsizet) then
		   call BELL(1)
	   	   print 303,in,ig,nleft
		   goto 301		!next good bit
		endif
		nend=inext+nsizet-1	!new nend value
		goto 307		!look for more spectra
	   endif
302	continue          !end of i loop
c Now have data in Yspec for one spectrum, so calculate it
	nspec=nspec+1	!not in CALCPS now
c Get mean current minus fitted asympote
c	ymsav1(nspec)=ybsav(nspec)-theta(kmax)	!mean cur minus fitted asympote
c NB ybsav(nspec) not calc until CALCPS called, so calc here as ymsum/nmsum
c -keep it as positive number (?)
	ymsav1(nspec)=abs(ymsum/float(nmsum)-theta(kmax))
	ym1=ymsav1(nspec)		!for print out
	cmean1=cmean1+ymsav1(nspec)			!total (to calc overall mean)
c Get mean of fitted points between tstart,tend
c	sum (over 1,..,ncomp) of w(i)*tau(i)*[exp(-t1/tau(i)-exp(-t2/tau(i)]
	y=0.0
	do 201 i=1,2*ncomp-1,2
	 tau=theta(i)	!1,3,5,...
	 w=theta(i+1)	!2,4,6,...
	 y=y + w*tau*(exp(-tstart/tau)-exp(-tend/tau))
	if(debug()) then
         print 714,i,w,tau,y,tstart,tend
         if(discprt) write(8,714) i,w,tau,y,tstart,tend
714	   format(' i,w,tau,y,tstart,tend = ',/,i3,5g13.6)
	endif
201	continue
	ymsav2(nspec)=abs(y/(tstart-tend))	!keep as pos number
	ym2=ymsav2(nspec)		!for print out
	cmean2=cmean2+ymsav2(nspec)			!total (to calc overall mean)
	call CALCPS(Yspec,Ymc,nspec,nsize,npspec,nchan,ibad,
     & ymsum,nmsum,tsum,ntsum,isec0,ip0,isecn,ipn,ig,PS,
     & varv,cmean,varsav,ybsav,timsav,calfac,calfac1,
     & ipmon,errfac,ibcor,ym1,ym2,reject,psprt)
c If spectrum rejected in CALCPS then ymsav1(nspec) and ymsav2(nspec) will
c be overwritten by next good values, but must remove them from cmean1,cmean2:
	if(reject) then
	   cmean1=cmean1-ymsav1(nspec)
	   cmean2=cmean2-ymsav2(nspec)
	   nspec=nspec-1		!reverse increment
	endif
	if(nspec.eq.nsmax) goto 400		!display averaged spectrum
	ispecbit=ispecbit+1	!# of spectra from current fitted bit
	if(nspecbit.ne.0.and.ispecbit.eq.nspecbit) goto 301	!next fitted bit
c
c Any more spectra from current good bit? All points up to Yraw(ipn) have
c been used; next usable point is i=ipn+1. If there are nsizet points left
c there could be another spectrum (though not nec, if some points are bad)
	inext=ipn+1
	nleft=nval-inext+1
	if(nleft.lt.nsizet) goto 301	!no more spectra
	nend=inext+nsizet-1
	goto 307		!look for more spectra
301	continue		!end of loop through all subtracted sections
	CLOSE(unit=16)
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
c
c
c Now the bit done when all spectra completed. Complete the calculations
c and display spectrum.
400	continue
c
c Display the spectrum
 	call VIDEOMOD(3)	 !leave graphics used in CALCPS
	idest=0		!so get spectrum first time
49	continue		!return for var/mean etc
	call PSDISP(PS,npspec,nspec,nsize,rate,res,
     & varv,nchan,cmean,varsav,ybsav,timsav,ymsav1,cmean1,
     & ymsav2,cmean2,ibcor,idiskq,idest)
c	if(idest.eq.1) goto 48
c
c Average scaled spectrum can now be written to disc and directory
c updated
c NB npspec,nspec are converted to integer*2 within PSWRT for storage
48	continue
	call PSWRT(PS,npspec,nspec,res,filt,calfac,
     & varv,cmean,cmean1,cmean2,filt1,calfac1,titdat,idest1)
c
999	continue
	call ENDPRINT
	END


	subroutine GETCUR(ys,ym,is,madc1,nchan,idata,idata1,
     & calfac,calfac1)
c To get a single value of the current with index =is in the current section,
c from the data arrays, idata/idata1, for the current section).
c Returns
c Ys=scaled current for spectrum, and
c Ym=current for calculation of mean current
	integer*2 idata(81920)
	integer*2 idata1(2,40960)     !equivalence for sep of 2-channel data
	logical madc1
c
	if(nchan.eq.1) then
	   ys=float(idata(is))*calfac
	   ym=ys
	else if(nchan.eq.2) then
	   ys=float(idata1(1,is))*calfac
	   if(madc1) then
		ym=float(idata1(2,is))*calfac1	!mean current from ADC1
	   else
		ym=ys						!mean current from ADC0
	   endif
	endif
	RETURN
	end


