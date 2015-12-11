	subroutine DISKIN2(idatyp,tint0,iampl0,ampl0,iprops0,nintt,
     & nfile,kfile,pfile,calfacs2,calfac2,nval,irecs,nopoint,
     & ifreq1,ifreq2,ifreq3,imin,imax,jfreq1,jfreq2,jfreq3,
     & cjump,nsweep,nswps,jumps,kjumps0,jumpomit,nomit,iscan,
     & iexstrt0,iexend0,nfits,timsav,ifits,ibaselin,
     & nset,nmax,nswpmax,nfitsmax,jumporig)
c To read data in EKDIST, after files specified in DISKIN1
c
c Modif 02/25/04 12:13pm for .ede and .evl files
c	idatyp=1	data from normal .SCN file
c	idatyp=2	data from old scandat.dat file
c	idatyp=3	internal simulated data (autosim=T) -does not use hjcdat1.for
c	idatyp=4	data from Dempster *.EDE file
c	idatyp=5	data from Axon *.EVL file

c Modif 09/11/00 11:56am for 2D arrays for sim fitting of >1 data sets
c TO D0
c (1) jfreq etc are still 1D
c (2) calfac2=calfacs2(1,1) -still needs to be fixed
c
c Modif 12/29/98 08:21pm to read new format scan.dat files (idatyp=1,
c   iscan=>103).  The extra arrays, timsav(), ifits(), ibaselin() are
c   written last in scanout.for, and are not read here because not needed.
c
c Modif 09/10/97 09:00pm, so can use diff tcrits for each expt pooled
c Define index (in raw data, tint0 etc) of 1st and last interval
c in each expt, for use if tcrit is not same for each of the pooled expts
c -define here iexstrt0(i)=j1; ,iexend0(i)=j2
c
c For cjump case all intervals are usable but for steady state records
c have to decide how to treat gap between expt files.
c Modified 02/03/97 10:05pm Now assume that
c (a) if last interval is shut, it has not been completed and should
c	be set unusable
c (b) if last interval is open, it is a properly measured open time
c	and should be left as such.  However the first interval
c	of the next file will also be an opening, so an unusable
c	shut time must be inserted after the last opening.
c
c Modif 11/14/96 04:25pm to fix errors in reading jump data from more than
c one file
c	kjumps(ijump)=kt, ijump=1,2,...,nsweep =index in tint() of 1st latency
c	  for sweep #ijump (could get this from iprops, but simpler to use kjumps)
c
c Point amplitudes: if more than 1 file read then frequencies must
c be summed. Each file may have different ifmin,ifmax, so read data
c from each file into temporary arays, jfreq(); sum frequencies in ifreq()
c and deallocate jfreq after leaving diskin2.  The values of ifmin,ifmax
c need not be calc here for the new ifreq() arrays, since they are
c calculated at time point amp histos are displayed in SCVDU.
c PROBLEM: cannot pool frequencies in this way unless calfac2 is same
c for each of the pooled experiment files (otherwise 'bin width' for
c one element of ifreq not same for each) -BUT COULD BE FIXED TO DO SO!
c
c (1) last gap in every expt is not defined so set iprops=8 for it
c	If channel was open at end (length undefined) then remove this
c	opening and set length of the gap preceding it as unusable
c (2) all amplitudes now kept in pA: ampl().
c
c	real*4 tint0(nd),ampl0(nd)
c	integer*2 iampl0(nd)
c	integer*1 iprops0(nd),ip0
c	integer kfile(50)
c	character*33 pfile(50)	!path names for SCANDAT files
c	integer nval(50),irecs(50),nswps(50)
c	integer iexstrt0(50),iexend0(50)
c	real*4 calfacs2(50)
c===
	real*4 tint0(nmax,nset),ampl0(nmax,nset)
	integer*2 iampl0(nmax,nset)
	integer*1 iprops0(nmax,nset)
	integer kfile(20,10),nfile(10),nintt(10)
	integer iexstrt0(20,10),iexend0(20,10)
	integer nval(20,10),irecs(20,10)
	integer nswps(20,10)
	character*40 pfile(20,10)		!path names for SCANDAT files
	real*4 calfacs2(20,10)
c===
	character*1 ans
	logical discprt,btest,nopoint,debug,caplock
	logical simulat
c for point amp histos
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 jfreq1(imin:imax),jfreq2(imin:imax),jfreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
	integer*4 ifdum(3)	!======for temp fix
c for cjump
	logical cjump
	logical addj
c	integer*2 jumps(nsweep),jumpomit(nsweep)
c	integer*4 kjumps0(nsweep)
c	integer*2 kjumps2(:)
	integer nsweep(10),nomit(10),nfits(10)
	integer*2 jumps(nswpmax,nset),jumpomit(nswpmax,nset)
	integer*2 jumporig(nswpmax,nset)
	integer*4 kjumps0(nswpmax,nset)
	allocatable::kjumps2	!local integer*2 for reading from scandat
	integer*2 kjumps2(:,:)
c For display of consam data
	real*8 timsav(nfitsmax)
	integer*2 ibaselin(nfitsmax)
	integer*4 ifits(nfitsmax)
c
c
c  Declarations for read of .EDE files
	integer*4 ChannelState,ZeroLevel,StartAt,Endat
	real*8    ExactStart,ExactEnd,Duration
	real*4    Average,Variance
	integer*4 Next,Previous
	logical*1 ignore,available
c
c Declarations for.EVL data record
	integer*2	eventepi
	integer*4	LevelStart
	real*4	Amplitude
      integer*4	LevelLength
	real*4	EventSD
	integer*2	EventLevel
	integer*2 	EventProps
	integer*1 ip0,ip1,ip2
	logical present,clampex
	common/evl/clampex	!for read_evl and diskin2
c
c	allocatable::isame,jsame
c	integer*4 isame(:),jsame(:)
c
	common/dp/discprt
c
	debug()=caplock()
c
c=======need sexp,sres?
c
c	pon()=slock()
c
c	pon1=pon()
c
	simulat=iscan.eq.-103.or.iscan.eq.-3001
	if(idatyp.eq.4) goto 201	!read .ede file
	if(idatyp.eq.5) goto 301	!read .evl file
c
	allocate(kjumps2(nswpmax,nset))
c
	print 1
1	format(' Reading data . . .')
c  Initialise ifreq1() etc
	do i=imin,imax
	   ifreq1(i)=0
	   ifreq2(i)=0
	   ifreq3(i)=0
	   jfreq1(i)=0
	   jfreq2(i)=0
	   jfreq3(i)=0
	enddo
c Accumulate total frequencies for all files in ifreq() (OK only if
c calfac2 is same for each expt pooled. COULD BE FIXED TO DO SO!)
c===fix this!
	   calfac2=calfacs2(1,1)		!needed in SCVDU
c
c Now read and pool the expts
c (NB on entry nsweep(j)=total # of jumps in all files (calc in ekdist)
	OPEN(unit=18,file=pfile(1,1),status='UNKNOWN',
     &	access='TRANSPARENT')
c Restart index for tint() etc for each set
c START FILE LOOP
	do j=1,nset
	 index=1	!i=1,...,nintt(j) -index for concatenated tint()
	 indswp=1	!i=1,...,nsweep(j) =index in concatenated jumps,kjumps
	 do i=1,nfile(j)
	   irec1=irecs(i,j)
c	   if((i.gt.1.or.j.gt.1).and.(pfile(i,j).ne.pfile(1,1))) then
	   if(pfile(i,j).ne.pfile(1,1)) then
		CLOSE(unit=18)
		OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
	   endif
c=	   j1=index
c=	   j2=index+nval(i,j)-1
	   is1=index		!s=steady state data
	   is2=index+nval(i,j)-1
	   if(cjump) then
c=		n1=indswp
c=		n2=indswp+nswps(i,j)-1
		ij1=indswp		!j=jump
		ij2=indswp+nswps(i,j)-1
	   endif
c
c Define index (in raw data, tint0 etc) of 1st and last interval
c in each expt, for use if tcrit is not same for each of the pooled expts
	   iexstrt0(i,j)=is1
	   iexend0(i,j)=is2
c For all simulated data (cjump or not), nopoint=true and only tint, iampl
c  and iprops written as data, but if cjump then need to construct
c  the jumps, kjumps here
	   if(nopoint) then
	      read(18,rec=irec1)
     &       (tint0(is,j),is=is1,is2),
     &	 (iampl0(is,j),is=is1,is2),
     &	 (iprops0(is,j),is=is1,is2)
c     &      (tint0(j),j=j1,j2),(iampl0(j),j=j1,j2),(iprops0(j),j=j1,j2)
	   else
c====temp fix for crazy values in ifmin,ifmax
	      read(18,rec=irec1)
     &      (tint0(is,j),is=is1,is2),
     &	(iampl0(is,j),is=is1,is2),
     &	(iprops0(is,j),is=is1,is2),
     &	 (ifmin(is),is=1,3),(ifmax(is),is=1,3)
		do is=1,3
		   if(ifmin(is).lt.imin.or.ifmin(is).gt.imax) then
			print 21,is,ifmin(is),imin
21			format(' ifmin(',i2,') changed from ',i8,' to ',i8)
			ifmin(is)=imin
		   endif
		   if(ifmax(is).lt.imin.or.ifmax(is).gt.imax) then
			print 22,is,ifmax(is),imax
22			format(' ifmax(',i2,') changed from ',i8,' to ',i8)
			ifmax(is)=imax
		   endif
		enddo
		if(.not.cjump) then
		   if(nfile(j).eq.1) then	!can read timsav etc only if nfile=1
			read(18,rec=irec1) (tint0(is,j),is=is1,is2),
     &		(iampl0(is,j),is=is1,is2),(iprops0(is,j),is=is1,is2),
     &		(ifdum(is),is=1,3),(ifdum(is),is=1,3),
     &		(jfreq1(is),is=ifmin(1),ifmax(1)),
     &		(jfreq2(is),is=ifmin(2),ifmax(2)),
     &		(jfreq3(is),is=ifmin(3),ifmax(3)),
     &		(timsav(is),is=1,nfits(j)),(ifits(is),is=1,nfits(j)),
     &		(ibaselin(is),is=1,nfits(j))
		   else if(nfile(j).gt.1) then	!can read timsav etc only if nfile=1
			read(18,rec=irec1) (tint0(is,j),is=is1,is2),
     &		(iampl0(is,j),is=is1,is2),
     &		(iprops0(is,j),is=is1,is2),
     &		(ifdum(is),is=1,3),(ifdum(is),is=1,3),
     &		(jfreq1(is),is=ifmin(1),ifmax(1)),
     &		(jfreq2(is),is=ifmin(2),ifmax(2)),
     &		(jfreq3(is),is=ifmin(3),ifmax(3))
		   endif
		else if(cjump) then
		   if(iscan.ge.102) then
			if(nfile(j).eq.1) then
	              read(18,rec=irec1) (tint0(is,j),is=is1,is2),
     & 	        (iampl0(is,j),is=is1,is2),
     &		  (iprops0(is,j),is=is1,is2),
     &	        (ifdum(is),is=1,3),(ifdum(is),is=1,3),
     &	        (jfreq1(is),is=ifmin(1),ifmax(1)),
     &	        (jfreq2(is),is=ifmin(2),ifmax(2)),
     &	        (jfreq3(is),is=ifmin(3),ifmax(3)),
     &  	        (jumps(ij,j),ij=ij1,ij2),(kjumps0(ij,j),ij=ij1,ij2),
     &	       (timsav(is),is=1,nfits(j)),(ifits(is),is=1,nfits(j)),
     &	        (ibaselin(is),is=1,nfits(j))
			else
	              read(18,rec=irec1) (tint0(is,j),is=is1,is2),
     & 	        (iampl0(is,j),is=is1,is2),
     &		  (iprops0(is,j),is=is1,is2),
     &	        (ifdum(is),is=1,3),(ifdum(is),is=1,3),
     &	        (jfreq1(is),is=ifmin(1),ifmax(1)),
     &	        (jfreq2(is),is=ifmin(2),ifmax(2)),
     &	        (jfreq3(is),is=ifmin(3),ifmax(3)),
     &  	        (jumps(ij,j),ij=ij1,ij2),(kjumps0(ij,j),ij=ij1,ij2)
			endif
		   else if(.not.simulat.and.iscan.le.101) then
			if(nfile(j).eq.1) then
			   read(18,rec=irec1) (tint0(is,j),is=is1,is2),
     &		   (iampl0(is,j),is=is1,is2),
     &		   (iprops0(is,j),is=is1,is2),
     &		   (ifdum(is),is=1,3),(ifdum(is),is=1,3),
     &		   (jfreq1(is),is=ifmin(1),ifmax(1)),
     &		   (jfreq2(is),is=ifmin(2),ifmax(2)),
     &		   (jfreq3(is),is=ifmin(3),ifmax(3)),
     & 		  (jumps(ij,j),ij=ij1,ij2),(kjumps2(ij,j),ij=ij1,ij2),
     &		 (timsav(is),is=1,nfits(j)),(ifits(is),is=1,nfits(j)),
     &		   (ibaselin(is),is=1,nfits(j))
			else if(nfile(j).gt.1) then
			   read(18,rec=irec1) (tint0(is,j),is=is1,is2),
     &		   (iampl0(is,j),is=is1,is2),
     &		   (iprops0(is,j),is=is1,is2),
     &		   (ifdum(is),is=1,3),(ifdum(is),is=1,3),
     &		   (jfreq1(is),is=ifmin(1),ifmax(1)),
     &		   (jfreq2(is),is=ifmin(2),ifmax(2)),
     &		   (jfreq3(is),is=ifmin(3),ifmax(3)),
     & 		   (jumps(ij,j),ij=ij1,ij2),(kjumps2(ij,j),ij=ij1,ij2)
			endif
c kjumps(ij,j)=kt, ijump=1,2,...,nsweep(j); ij=1,nset =index in tint() of 1st latency
c   for sweep #ijump
c Convert kjumps to integer*4 here
		     do ij=ij1,ij2
			kj=int4(kjumps2(ij,j))
	      	if(kj.le.-1.and.kj.ge.-32768) then
	               kj=kj+65536
	      	else if(kj.lt.-32768) then
	               print*,' KJUMPS() IS TOO BIG!!'
	               STOP
	      	endif
	      	kjumps0(ij,j)=kj
		     enddo
		   endif
c Correct kjumps by adding to index the number of intervals that were
c read up to end of previous file = j1-1
		   if(i.gt.1) then
			do ij=ij1,ij2
			   kjumps0(ij,j)=kjumps0(ij,j)+(is1-1)
			enddo
		   endif
c
		   if(debug()) then
		      call DCASK('Alter any values','n',ans)
			if(ans.eq.'Y') then
			   print 700
700			   format(
     &			' (1) No more changes',/,
     &			' (2) Change iprops(i) value',/,
     &			' Option number [1] = ')
			   iopt=1
			   call INPUTi(iopt)
			   if(iopt.eq.2) then
				print 701
701				format(' i = ')
				call INPUTi(is)
				ip0=iprops0(is,j)
c========================debug
				call GETBITS(iprops0(is,j),0)
				call GETBITS(ip0,0)
c========================end debug
				print 702,i,ip0
702				format(' iprops(',i5,') [at present ',i3,
     &			 ']; new value [0] = ')
				ip=0
				call INPUTi(ip)
				iprops0(is,j)=ip
c========================debug
				call GETBITS(iprops0(is,j),ip)
				pause
c========================end debug
				write(18,rec=irec1) (tint0(is,j),is=is1,is2),
     &			(iampl0(is,j),is=is1,is2),
     &			(iprops0(is,j),is=is1,is2)
c			      write(18,rec=irec1)
c     &      		(tint0(j),j=j1,j2),(iampl0(j),j=j1,j2),
c     &			(iprops0(j),j=j1,j2),
c     & 			 (ifmin(j),j=1,3),(ifmax(j),j=1,3),
c     &			 (jfreq1(j),j=ifmin(1),ifmax(1)),
c     &  			 (jfreq2(j),j=ifmin(2),ifmax(2)),
c     & 			 (jfreq3(j),j=ifmin(3),ifmax(3)),
c     &    			 (jumps(j),j=n1,n2),(kjumps2(j),j=n1,n2)
			      if(discprt) write(8,703) i,ip0,ip
703				format(
     &              ' *********************************************',/,
     &              ' iprops(',i5,') changed from ',i3,' to ',i3,/,
     &              ' *********************************************')
			   endif
			endif
		   endif
		endif
	   endif
c
c For simulated cjumps now make the jumps, kjumps arrays here (NB may have
c more than 1 file! -so need to define jumps(j),j=n1,n2 and kjumps0(j),j=n1,n2)
	   if(simulat.and.cjump) then
		do n=ij1,ij2
		   jumps(n,j)=n
		enddo
		nj1=0
		do n=1,nval(i,j)				!nint here =kt in scsim
		   if(BTEST(iprops0(n,j),5)) then	!first in jump
			nj1=nj1+1
			kjumps0(ij1+nj1-1,j)=n
		   endif
		enddo
		if(nj1.ne.nswps(i,j)) then
		   call BELL(3)
		   print 76,ij1,nswps(i,j)
76		   format(' ERROR: ij1,njump=nswps(i,j), i,j are: ',4i8)
		endif
	   endif

	   if(nopoint) goto 18
c
c Check for crazy values!
	   if(debug()) then
		jmax=-2147483647
		jmin=2147483647
		do k=ifmin(1),ifmax(1)
		   if(jfreq1(k).lt.jmin) jmin=jfreq1(k)
		   if(jfreq1(k).gt.jmax) jmax=jfreq1(k)
		enddo
		if(idatyp.eq.1) then
		   n=i
		else if(idatyp.eq.2) then
		   n=kfile(i,j)	!old
		endif
		print 70,n,jmin,jmax
70		format(' File # ',i5,/,
     &	'  min and max frequencies for all point histo  = ',2i12)
c
		jmax=-2147483647
		jmin=2147483647
		do k=ifmin(2),ifmax(2)
		   if(jfreq2(k).lt.jmin) jmin=jfreq2(k)
		   if(jfreq2(k).gt.jmax) jmax=jfreq2(k)
		enddo
		print 71,n,jmin,jmax
71		format(' File # ',i5,/,
     &	'  min and max frequencies for shut point histo = ',2i12)
		jmax=-2147483647
		jmin=2147483647
		do is=ifmin(3),ifmax(3)
		   if(jfreq3(is).lt.jmin) jmin=jfreq3(is)
		   if(jfreq3(is).gt.jmax) jmax=jfreq3(is)
		enddo
		print 72,n,jmin,jmax
72		format(' File # ',i5,/,
     &	'  min and max frequencies for open point histo = ',2i12)
c Temp fix for crazy values of ifmax(3) (resulted from error, now fixed)
c Version below doesn't work if have large values at end of
c jfreqn(), but with a value less than 1000 among them; probably
c safer to search for first zero value! TRY THIS
c Is 1000 a reasonable value? For large file, say 50m points, might have
c most points near baseline, i.e. in say 1/20 of full range=4096/20=200
c bins = 50m/200=250000 points/bin!
		if(jmax.gt.250000) then
		   i1=ifmax(3)
		   i2=ifmin(3)
		   do is=i1,i2,-1			!look back from end
			if=iabs(jfreq3(is))
c			if(if.le.1000.and.if.ne.0) then	!'reasonable' value found
			if(if.eq.0) then	!'reasonable' value found
			   ifmax(3)=is
			   goto 73
			endif
		   enddo
		   call BELL(1)
73		   print 74,jmax,i1,ifmax(3)
	     	   if(discprt) write(8,74) jmax,i1,ifmax(3)
74		   format(
     &     ' WARNING: crazy value (',i12,') for max freq. TEMP FIX:',/,
     &     ' Largest element of jfreq3 used reduced from ',i8,' to ',i8)
		endif
	   endif
c
	   if(i.gt.1.and.calfacs2(i,j).ne.calfacs2(1,j)) then
		if(idatyp.eq.1) then
		   nsc=i
		else if(idatyp.eq.2) then
	         nsc=kfile(i,j)
		endif
		call BELL(1)
		print 17,nsc
17		format(/,
     &	' Cannot pool point amplitude frequencies for file ',i4,/,
     &	' because calibration differs from earlier files')
		goto 18
	   endif
c
	   do is=ifmin(1),ifmax(1)
		ifreq1(is)=ifreq1(is)+jfreq1(is)
	   enddo
	   do is=ifmin(2),ifmax(2)
		ifreq2(is)=ifreq2(is)+jfreq2(is)
	   enddo
	   do is=ifmin(3),ifmax(3)
		ifreq3(is)=ifreq3(is)+jfreq3(is)
	   enddo
c
18	   continue
c Now convert amplitudes to picoamps with calfac for this particular file
	   do is=is1,is2
		ampl0(is,j)=calfacs2(i,j)*float(iampl0(is,j))
	   enddo
c
c NOW UPDATE index and indswp
c
c Set last gap for each individual file unusable automatically
c For cjump case all intervals are usable but for steady state records
c have to decide how to treat gap between expt files.
c Modified 02/03/97 10:05pm Now assume that
c (a) if last interval is shut, it has not been completed and should
c	be set unusable
c (b) if last interval is open, it is a properly measured open time
c	and should be left as such.  However the first interval
c	of the next file will also be an opening, so an unusable
c	shut time must be inserted after the last opening.
c	   ntsav=nintt
	   if(cjump) then
	      index=is2+1	!index in tint for 1st value in next expt file
	      indswp=ij2+1	!index in kjump for 1st value in next expt file
c		goto 10		!all intervals usable
	   endif
	   if(.not.cjump) then
		if(iampl0(is2,j).eq.0) then	!last interval shut
		   if(BTEST(iprops0(is2,j),3))then  !already set unusable (bit 3='8' set)
			index=is2+1	!index in tint for 1st value in next expt file
		      print 200,i,j
		     	if(discprt) write(8,200) i,j
200			format(
     & ' Last interval in file ',i3,' (set ',i3,') was shut and was',/,
     & '   already set as unusable')
		   else
			iprops0(is2,j)=IBSET(iprops0(is2,j),3) !bit 3='8' set last gap unusable
			index=is2+1	!index in tint for 1st value in next expt file
		      print 20,i,j
		     	if(discprt) write(8,20) i,j
20			format(
     & ' Last interval in file ',i3,' (set ',i3,') was shut so assumed',
     & /,'    to be of undefined length and was therefore set unusable')
		   endif
c
		else if(i.lt.nfile(j)) then     !last interval open (and not last file)
		   is2=is2+1		!add unusable gap
		   tint0(is2,j)=-1.
		   iampl0(is2,j)=0
		   iprops0(is2,j)=IBSET(iprops0(is2,j),3) !bit 3='8' set it unusable
		   nintt=nintt+1		!extra (unusable) gap
		   index=is2+1	!index in tint for 1st value in next expt file
		   if(BTEST(iprops0(is2-1,j),3))then  !already set unusable (bit 3='8' set)
	            print 302,i,j,i+1,nintt(j)
	     	      if(discprt) write(8,302) i,j,i+1,nintt(j)
302		      format(
     & ' Last interval in file ',i3,' (set ',i3,') was open; it was',/,
     & '   already,set unusable, and an unusable shut time was',/,
     & '   inserted after it to separate it from first opening of',i3,/,
     & '   file (total number of intervals increased to ',i5,')')
		   else
	            print 30,i,j,i+1,nintt(j)
	     	      if(discprt) write(8,30) i,j,i+1,nintt(j)
30		      format(
     & ' Last interval in file ',i3,' (set ',i3,') was open; assumed',/,
     & '   to be of defined length, and an unusable shut time was',/,
     & '   inserted after it to separate it from first opening',i3,/,
     & '   of file (total number of intervals increased to ',i5,')')
		   endif
		else if(i.eq.nfile(j)) then     !last interval open in last file
		   is2=is2+1		!add unusable gap
		   tint0(is2,j)=-1.
		   iampl0(is2,j)=0
		   iprops0(is2,j)=IBSET(iprops0(is2,j),3) !bit 3='8' set it unusable
		   nintt(j)=nintt(j)+1		!extra (unusable) gap
		   if(BTEST(iprops0(is2-1,j),3)) then  !already set unusable (bit 3='8' set)
	      	print 303,i,j,nintt(j)
		     	if(discprt) write(8,303) i,j,nintt(j)
303			format(
     & ' Last interval in last file ',i3,' (set ',i3,') was open; it',/,
     & '   was already set unusable, and an unusable shut time was',/,
     & '   inserted after it to indicate that a burst may be',/,
     & '   truncated (total number of intervals increased to ',i5,')')
		   else
	      	print 300,i,j,nintt(j)
		     	if(discprt) write(8,300) i,j,nintt(j)
300			format(
     & ' Last interval in last file ',i3,' (set ',i3,') was open;',/,
     & '  assumed to be of defined length, and an unusable shut time',/,
     & '  was inserted after it to indicate that a burst may be',/,
     & '  truncated (total number of intervals increased to ',i5,')')
		   endif
		endif
	   endif
c        File #i (set j) now read in.  Check on the sweep numbers in
c	   jumps(ij1:ij2,j). If files from different expts have been pooled
c	   the same sweep number may be repeated.  Check later to see whether
c	   an error in SCANning has lead to same sweep # occurring twice in one
c	   file (in JUMPCHK).  At this stage just ensure that sweep numbers
c	   are not overlapping.
c	   (1) Check whether ANY of the sweep numbers in the current file
c		 have already occurred in earlier files
c	   (2) if so, find maximum sweep # in all earlier files and add this
c		to current file (safe even if jumps=1 occurs in current file)
c        (3) print what was done
	   if(cjump) then
	     do i1=ij1,ij2	!go through current file
		  j1=jumps(i1,j)
		  jumporig(i1,j)=j1
	     enddo
	     if(i.gt.1) then	!not for 1st file!
		addj=.false.
		do i1=ij1,ij2	!go through current file
		   j1=jumps(i1,j)
		   j2max=0
		   do i2=1,ij1-1 	!go through all previous files
			j2=jumps(i2,j)
			if(j2.gt.j2max) j2max=j2
			if(j2.eq.j1) then	!sweep number already occurred
			   addj=.true.
			endif
		   enddo
		enddo
		if(addj) then
		   do i1=ij1,ij2	!go through current file
			j1=jumps(i1,j)
			jumps(i1,j)=j1+j2max
		   enddo
		   print 93,i,j,ij1,ij2,j2max
		   if(discprt) write (8,93) i,j,ij1,ij2,j2max
93		   format(/,
     & ' File # ',i3,' in set ',i2,' (jumps ',i4,' to ',i4,'):',/,
     & ' some sweep numbers from CJUMP are the same as',/,
     & ' as in earlier files so ', i4,' added to all sweep numbers')
c            Can get 18 numbers of 3 digits per line, and have ij2-ij1+1
c            numbers to print so number of lines needed is
		   np=ij2-ij1+1
		   nl=1+(np-1)/18
		   nleft=np - (nl-1)*18
		   ii1=ij1
		   ii2=ii1+18-1
		   if(nl.eq.1) ii2=ii1+nleft-1
		   do n=1,nl
			print 90,(i1,i1=ii1,ii2)
			print 91,(jumporig(i1,j),i1=ii1,ii2)
	 		print 92,(jumps(i1,j),i1=ii1,ii2)
			if(discprt) write(8,90) (i1,i1=ii1,ii2)
			if(discprt) write(8,91)(jumporig(i1,j),i1=ii1,ii2)
	 		if(discprt) write(8,92)(jumps(i1,j),i1=ii1,ii2)
90			format(/,' jump ',18(1x,i3))
91			format(' orig ',18(1x,i3))
92			format(' new  ',18(1x,i3))
			ii1=ii2+1
			ii2=ii1+18-1
			if(n.eq.nl) ii2=ii1+nleft-1
		   enddo
		endif
	    endif
	   endif
c10	   continue		!not used now
	 enddo		!end of i=1,nfile(j)
	enddo		!end of j=1,nset
	CLOSE(unit=18)
c
	DEALLOCATE(kjumps2)
c
c For cjump case, check kjump() vs iprops(), for consistency, in case of
c errors in SCAN!
	if(debug().and.cjump) then
	   do j=1,nset
		do is=1,nsweep(j)
		   if(discprt) write(8,26)is,j,kjumps0(is,j)
26		   format(3i8)
		enddo
	   enddo
	endif
c  Now do checks on jumps in separate subroutine JUMPCHK.
	goto 999	!end of SCAN.SCN
c
c Read a Dempster *.ede file
201	continue
	do j=1,nset		!read for each set
         print 71,j
         if(discprt) write(8,71) j
c71	   format(' Set ',i3)
	   OPEN(unit=18,file=pfile(1,j),status='UNKNOWN',
     & 	access='TRANSPARENT')
	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
		if(i.gt.1.and.pfile(i,j).ne.pfile(i-1,j)) then
		   CLOSE(unit=18)
		   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
		endif
		irec1=5	!first 60-byte data record
		n1=index
		n2=index+nval(i,j)-1
c read nval records, and set tint0,ampl0,iprops0
		do n=1,nval(i,j)
		   read(unit=18,rec=irec1) ChannelState,ZeroLevel,
     &	 	StartAt,Endat,ExactStart,ExactEnd,Duration,
     &		Average,Variance,
     &		Next,Previous,
     &		ignore,available
c
		   tint0(n1,j)=sngl(Duration*1.d3)	!ms: Duration is in seconds
c
		   if(ChannelState.eq.0) then
			ampl0(n1,j)=0
			iampl0(n1,j)=0
		   else
			ampl0(n1,j)=Average
			iampl0(n1,j)=1
		   endif
c
		   if(Ignore) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),3) !bit 3='8' set to set as unusable
		   else
			iprops0(n1,j)=0
		   endif
		   n1=n1+1			!next element of tint0 etc02/23/04 08:11am
	         irec1=irec1+60		!next record
		enddo
		ilast=n2		!last value read
c Set last gap for each individual file unusable automatically (except for
c simulated data, iscan=-3001 or -103)
	      ntsav=nintt(j)
c
		if(iampl0(n2,j).eq.0) then
		   iprops0(n2,j)=IBSET(iprops0(n2,j),3) !bit 3='8' set last gap unusable
		   ilast=n2
		else
		   do n=n2-1,1,-1			!look back for 1st shut time
			nintt(j)=nintt(j)-1			!decrement total number
			if(iampl0(n,j).eq.0) then	!shut time found
			   iprops0(n,j)=IBSET(iprops0(n,j),3) !bit 3='8' set it unusable
			   ilast=n
			   goto 202	!jump out of loop
			endif
		   enddo
		endif
202		continue
		if(nintt(j).lt.ntsav) then
	         print 20,i,nintt(j)
	     	   if(discprt) write(8,20) i,nintt(j)
c20		   format(
c     &	  ' Channel open at end of file #',i3,
c     &	  ': number of intervals reduced to ',i8)
		endif
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		call bell(1)
		print 1051,tint0(nintt(j),j)
1051		format(
     &	' Last interval is ',g13.6,' :reset as unusable [Y] ? ')
		ans='Y'	!default
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   print 105
     		   if(discprt) write (8,105)
105		   format(' Last interval set unusable.',/)
		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
	goto 999	!end of *.ede
c
c Read an Axon *.evl file
301	continue
	do j=1,nset		!read for each set
         print 71,j
         if(discprt) write(8,71) j
c71	   format(' Set ',i3)
	   OPEN(unit=18,file=pfile(1,j),status='UNKNOWN',
     & 	access='TRANSPARENT')
	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
		if(i.gt.1.and.pfile(i,j).ne.pfile(i-1,j)) then
		   CLOSE(unit=18)
		   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
		endif
		irec1=256 + 1		!first data record
		n1=index
		n2=index+nval(i,j)-1
c read nval records, and set tint0,ampl0,iprops0
		do n=1,nval(i,j)
c
		   if(clampex) then
	            read(unit=18,rec=irec1) eventepi,LevelStart,Amplitude,
     &		LevelLength,EventSD,EventLevel,EventProps
			if(EventLevel.eq.0) then
			   irec1=irec1 + 26
c			   irec2=irec1 + 22	!start byte for read after next
			else
			   irec1=irec1 + 22
c			   irec2=irec1 + 26	!start byte for read after next
			endif
		   else
	            read(unit=18,rec=irec1) LevelStart,Amplitude,
     &	 	LevelLength,EventSD,EventLevel,EventProps
			if(EventLevel.eq.0) then
			   irec1=irec1 + 24
c			   irec2=irec1 + 20	!start byte for read after next
			else
			   irec1=irec1 + 20
c			   irec2=irec1 + 24	!start byte for read after next
			endif
		   endif
c
		   tint0(n1,j)=float(LevelLength)*calfacs2(i,j)
c
		   if(EventLevel.eq.0) then
			ampl0(n1,j)=0
			iampl0(n1,j)=0
		   else
			ampl0(n1,j)=Amplitude
			iampl0(n1,j)=1
		   endif
c Meaning of EventProps
c the least significant bit:  Bit 0, event too short ('too short' means
c	that, after points that are affected by filtering are removed, there
c	are no points left to average for amplitude);
c Bit 1, amplitude manually adjusted by user;
c Bit 2, event rejected.
c Compare IPROPS
c	 0=all OK;
c	+1=amplitude dubious = bit 0;
c	+2=amplitude fixed = bit 1;
c	+4 amplitude of opening constrained (see fixamp) = bit 2;
c	+8=duration unusable = bit 3;
		   ip0=0
		   ip1=0
		   ip2=0
		   if(btest(EventProps,0)) ip0=1
		   if(btest(EventProps,1)) ip1=1
		   if(btest(EventProps,2)) ip2=1
c         set 'amplitude dubious'(bit 0) if ip0=1 ('too short')
		   if(ip0.ne.0) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),0) !set  bit 0='1' amp dubious
		   else
		      iprops0(n1,j)=IBCLR(iprops0(n1,j),0) !clear bit 0='1' amp dubious
		   endif
c         set 'amplitude fixed'(bit 1) if ip1=1 ('amp manually adjusted')
		   if(ip1.ne.0) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),1) !set bit 1='2' amp fixed
		   else
		      iprops0(n1,j)=IBCLR(iprops0(n1,j),1) !clear bit 0='1' amp dubious
		   endif
c         set 'duration unusable' (bit 3) if ip2=1 ('rejected')
		   if(ip2.ne.0) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),3) !bit 3='8' set to set as unusable
		   else
		      iprops0(n1,j)=IBCLR(iprops0(n1,j),3) !clear 3='8' set to set as unusable
		   endif
c
		   n1=n1+1			!next element of tint0 etc
		enddo
		ilast=n2		!last value read
c Set last gap for each individual file unusable automatically (except for
c simulated data, iscan=-3001 or -103)
	      ntsav=nintt(j)
c
		if(iampl0(n2,j).eq.0) then
		   iprops0(n2,j)=IBSET(iprops0(n2,j),3) !bit 3='8' set last gap unusable
		   ilast=n2
		else
		   do n=n2-1,1,-1			!look back for 1st shut time
			nintt(j)=nintt(j)-1			!decrement total number
			if(iampl0(n,j).eq.0) then	!shut time found
			   iprops0(n,j)=IBSET(iprops0(n,j),3) !bit 3='8' set it unusable
			   ilast=n
			   goto 304	!jump out of loop
			endif
		   enddo
		endif
304		continue
		if(nintt(j).lt.ntsav) then
	         print 20,i,nintt(j)
	     	   if(discprt) write(8,20) i,nintt(j)
c20		   format(
c     &	  ' Channel open at end of file #',i3,
c     &	  ': number of intervals reduced to ',i8)
		endif
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		call bell(1)
		print 1051,tint0(nintt(j),j)
c1051		format(
c     &	' Last interval is ',g13.6,' :reset as unusable [Y] ? ')
		ans='Y'	!default
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   print 105
     		   if(discprt) write (8,105)
c105		   format(' Last interval set unusable.',/)
		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
c	goto 999	!end of *.evl
c
999	continue

	RETURN
	END

