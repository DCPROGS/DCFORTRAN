	subroutine CDISTJ(tint,ampl,iprops,nint,nintt,ndimy,
     & yval,nyval,ohist,bhist,IDtype,ghist,ahist,pahist,obhist,
     & treso,tresg,tres,idest,idsav,shutav,openav,sdopen,tcrit,
     & nsweep,jumps,kjumps,jumpomit,nomit,nth,yvalt,nfitt,nnull,
     & ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,toffset,thist,
     & ameanval,sdval,ndimm,nthmax,idiskq,jset)
c
c=====rest to be fixed!
c====ONLY modif for 2D arrays so far is to replace call to getjump() with
c====call to getjumps(), to allow compilation, and addition of jset to call
c
c To calculate histogram values for various quantities after a jump in EKDIST
c 01/04/97 05:59pm P(open) per activation added
c 11/14/95 03:42pm Bursts added (-code is based round that for finding
c all open periods, and shut times between them, in this subroutine
c rather than that for burst length etc in CDIST2)
c
c Start of jump #1 is marked by BTEST(iprops(i),5) (='32'), and is also,
c more simply, is transition #kt=kjumps(i)
c
c note: ID=IDTYPE
c
	real*4 TINT(nintt),ampl(nintt),yval(ndimy)
	real*4 yvalt(5120)
	real*4 ameanval(0:ndimm),sdval(0:ndimm)
	integer*1 iprops(nintt)
	LOGICAL OHIST,BHIST,GHIST,AHIST,pahist,OBHIST,thist
	logical conam1,excsub
	logical pon,exass,exass1,debug,slock,caplock
	logical excnull,exczero,exclast,null,zero,last,debon
	logical exczero1,exclast1,excflat,fstlat,noprint
	logical sbin,shist,sres,sexp
	logical open1,open2,shut1,setbad,setbad1,setbad2,bad1,bad2
	logical btest,dubious,badint,bad,OK0,OK1
	logical endbad,badgap,endbst
	character*1 ans,UC
	character*11 string,cnum
	logical discprt,prt
	real*4 yloop(10),yhiop(10),ylogap(10),yhigap(10)	!for .ini
	ALLOCATABLE::yval1	!for SORTR
	real*4 yval1(:)
c
	ALLOCATABLE::ygap		!for new mean open period|adj gap calcs
	real*4 ygap(:)
	dimension ylo(10),yhi(10)
	ALLOCATABLE::ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,sx0,sdp
	real*4 ym0(:,:),sd0(:,:),ym1(:,:),sd1(:,:),pm0(:,:),pm1(:,:)
	real*4 sx0(:,:),sdp(:,:)
	integer ns0(:,:),ns1(:,:)
	real*4 sy0(10),syy0(10),sy1(10),syy1(10)
	integer ny0(10),ny1(10)
	ALLOCATABLE::index,indexg
	integer index(:)	!define so index(i)=jump # in which yval(i) occurs
	integer indexg(:)	!define so index(i)=jump # in which ygap(i) occurs
	logical allocated
c For ASCWRITE
	ALLOCATABLE::data
	real*4 data(:,:)
	character*20 colhead(30)
	character*70 text		!for ASCWRITE
c for cjump
	logical flag,lastop
	integer*2 jumps(nsweep),jumpomit(nsweep)
	integer*4 kjumps(nsweep)

	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	COMMON/OVLBLK/OBMAX,Jth,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
C
c Define functions
	pon()=slock()
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
	prt=pon()
	ndeb=1	!check if caplock on every ndeb'th jump
c
	if(shist) id=idsav
	if(idest.eq.5) then
	 if(tcrit.lt.0.) then
	   print 321
321	   format(' Critical gap length (ms) = ')
	   call INPUTr(tcrit)
	 else
	   print 320,tcrit
320	   format(' Critical gap length [',f7.2,'ms] = ')
	   call INPUTr(tcrit)
	 endif
	endif
c
C NOW GET PARAMETERS FOR HISTOGRAMS
	do i=1,nint
	   yval(i)=0.0	!needs to be initiallised for some burst distns
	enddo
C
101	format(a1)
C
	OHIST=.FALSE.
	BHIST=.FALSE.
	GHIST=.FALSE.
	AHIST=.FALSE.
	pahist=.false.		!point amp histo
	OBHIST=.FALSE.		!DIST=OPENINGS/BURST
	nfitt=0	!to count truncated values
	if(.not.shist) then
	   conam1=.false.
	   exass=.false.
	endif
c
	print 201,toffset
201	format(' Time offset (ms from command to c-jump) [',f10.3,
     &    ' ms] = ')
   	call INPUTr(toffset)
c	print 201,toffset
c201	format(' Time offset (ms from command to c-jump) = ',
c     &    f10.3,' ms: O.K. [Y] ? ')
c	read 101,ans
c      if(UC(ans).eq.'N') then
c   	   print 202
c202	   format(' Time offset (ms from command to c-jump) = ')
c   	   call INPUTr(toffset)
c      endif
      if(pon()) write(7,203) toffset
      if(discprt) write(8,203) toffset
203	format(/,' Time offset (ms from command to c-jump) = ',g12.5)
c
c Count null sweeps etc
	nnull=0	!count null sweeps
	nop1=0	!count number of sweeps that start with channel open
	nop2=0	!count number of sweeps with latency =<0 after offset subtracted
	nopn=0	!count number of sweeps that end with channel open
	nord=0	!counts those for which none of above are true
	do i=1,nsweep
	   flag=.false.	!set if any of above is true
	   ifirst=kjumps(i)	!index in tint() of 1st interval in ith jump
	   if(i.lt.nsweep) then
		ilast=kjumps(i+1)-1	!index of last transition in ith jump
	   else
		ilast=nint
	   endif
	   if(ifirst.eq.ilast) then
		nnull=nnull+1
		flag=.true.
	   endif
	   if(ampl(ifirst).ne.0.) then
		nop1=nop1+1
		flag=.true.
	   endif
	   if(tint(ifirst)-toffset.le.0.0) then
		nop2=nop2+1
		flag=.true.
	   endif
	   if(ampl(ilast).ne.0.) then
		nopn=nopn+1
		flag=.true.
	   endif
	   if(.not.flag) then
		nord=nord+1
	   endif
	enddo
      print 52,nsweep,nnull,nop1,nopn,nop2,nord
      if(pon()) write(7,52) nsweep,nnull,nop1,nopn,nop2,nord
      if(discprt) write(8,52) nsweep,nnull,nop1,nopn,nop2,nord
52	format(
     & ' Number of sweeps = ',i6,/,/,
     & ' Number of null sweeps = ',i6,/,
     & ' Number of sweeps with channel open at t=0 = ',i6,/,
     & ' Number of sweeps with channel open at end = ',i6,/,
     & ' Number of sweeps with latency =< 0 (after offset) = ',i6,/,
     & ' Number of for which none of these above are true = ',i6,/)
c
      print 90
90	format(/,
     & ' Now get values (''Yval'') for the distribution',/,
     & ' To see what is being done, put CAP LOCK on now',/,
     & ' (press any key to continue)')
	call ANYKEY()
c
	if(idest.eq.2) goto 20	!shut times
	if(idest.eq.3) goto 30	!open times
	if(idest.eq.4) goto 40	!total activation etc
	if(idest.eq.5) goto 73	!bursts
c
c First SHUT times (idest=2)
20	continue
	GHIST=.TRUE.
	tres=tresg
c
c	if(shist) goto 309
c
	print 21
21	format(
     & ' (1) Latency to the first opening',/,
     & ' (2) Length of nth shut time after t=0',/,
     & ' (3) Length of all shut times',/,
     & '     (and shut-shut correlation)',/,
     & ' (4) Mean length of nth shut time for nth = 0, 1,...,n',/,
     & ' (5) Length of the last shut time',/,
     & ' Option number = ')
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.5) goto 20
	ID=100+iopt			!101,102,103,104,105
	thist=.true.
	ghist=.true.
	if(id.eq.104.and.shutav.lt.0.0) then
	   call BELL(1)
	   print 211
211	   format(' Do option 3 first to define mean of all shut times')
	   iopt=3
	   goto 20
	endif
	if(id.eq.101) then
         print 22
         if(pon()) write(7,22)
         if(discprt) write(8,22)
22	   format(/,' Latency to the first opening')
	else if(id.eq.102) then
         print 23
         if(pon()) write(7,23)
         if(discprt) write(8,23)
23	   format(/,
     &   ' Length of the nth shut time after t=0 (n=0 for latency)')
	else if(id.eq.103) then
         print 24
         if(pon()) write(7,24)
         if(discprt) write(8,24)
24	   format(/,' Length of all shut times')
	else if(id.eq.104) then
         print 241
         if(pon()) write(7,241)
         if(discprt) write(8,241)
241	   format(/,' Mean length of nth shut time')
	else if(id.eq.105) then
         print 242
         if(discprt) write(8,242)
242	   format(/,' Length of the last shut time')
	endif
      print 27
      if(pon()) write(7,27)
      if(discprt) write(8,27)
27	format(' ==================================================',/)
	if(id.eq.101) then
	   nth=0
	else if(id.eq.105) then
	   nth=-1		!signal for last shut time
	else if(id.eq.102) then
26	   nth=1
	   print 25
25	   format(' Length of nth shut time (0=latency) (n=1,2,..); n = ')
	   call INPUTi(nth)
c==	   if(nth.lt.1) goto 26
	   nthsav=nth		!loop may change value
         if(pon()) write(7,28) nth
         if(discprt) write(8,28) nth
28	   format(' ( n = ',i3,')')
	endif
	if(id.eq.104) then
261	   nthmax=20
	   print 251
251	   format(' Calculate means for N up to Nmax: Nmax [20] = ')
	   call INPUTi(nthmax)
	endif
c
	excnull=.false.
c Next question not nec (nulls used in PTYP1; not needed in EXPLIK?)
c#	if(id.eq.101.or.id.eq.103) then
c	   call DCASK('Exclude null sweeps','N',ans)
c	   if(ans.eq.'Y') excnull=.true.
c	   if(excnull) then
c      	if(pon()) write(7,306)
c	      if(discprt) write(8,306)
306		format(
     &	' Null sweeps are omitted',/)
c	   else
c      	if(pon()) write(7,3061)
c	      if(discprt) write(8,3061)
c3061		format(
c     &	' Null sweeps are included (and used in fitting)',/)
c	   endif
c	endif
	exczero=.true.
	excflat=.false.
	if(id.eq.103) then
	   call DCASK('Exclude all first latencies','Y',ans)
	   if(ans.eq.'Y') excflat=.true.
	endif
	if(id.eq.101.or.id.eq.103) then
	   if(.not.excflat) then
		call DCASK(
     &	'Exclude zero latencies (channel open at t=0)','Y',ans)
		if(ans.eq.'N') exczero=.false.
	   endif
	   if(.not.excflat.and.exczero) then
      	if(pon()) write(7,307) nz1+nz2
	      if(discprt) write(8,307) nz1+nz2
307		format(i5,
     &' zero first latencies (channel open at t=0) are omitted',/)
	   else if(.not.exczero) then
      	if(pon()) write(7,3052) nz1+nz2
	      if(discprt) write(8,3052) nz1+nz2
3052		format(i5,
     &' zero first latencies (channel open at t=0) are included',/)
	   endif
	endif
	if(id.eq.103) then
	   if(excflat) then
      	if(pon()) write(7,305)
	      if(discprt) write(8,305)
305		format(
     &' All first latencies omitted',/)
	   else
      	if(pon()) write(7,3051)
	      if(discprt) write(8,3051)
3051		format(
     &' All first latencies are included',/)
	   endif
	endif
c
	exclast=.false.
	if(id.eq.102.or.id.eq.103.or.id.eq.104.or.id.eq.105) then
	   print 310
310	   format(
     &' If the channel is shut at the end of the sweep, should this',/,
     &' (truncated) value be excluded? (This will also exclude null ',
     &'sweeps)')
	   call DCASK('Exclude last value','Y',ans)
	   if(ans.eq.'Y') exclast=.true.
	   if(exclast) then
      	if(pon()) write(7,304)
	      if(discprt) write(8,304)
304		format(
     &' Shut times at the end of a sweep (truncated) are omitted',/,
     &'  (so null sweeps also omitted)',/)
	   else
      	if(pon()) write(7,3041)
	      if(discprt) write(8,3041)
3041		format(
     &' Shut times at the end of a sweep (truncated) are included',/,
     &'  (and used for fitting)',/)
	   endif
	endif
	exass=.false.
	print 301
301	format(
     &' Exclude gaps bordered by ''dubious'' amplitude openings [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	exass=UC(ans).ne.'N'
	if(exass.and.pon()) write(7,302)
	if(exass.and.discprt) write(8,302)
302	format(
     &' Gaps that are bordered by openings with ''dubious'' amplitudes',
     &' are excluded')
c
c Now get Yval
	debon=debug()
	if(id.eq.103) goto 398		!for 'all shut times'
	if(id.eq.104) goto 399		!for means
c
c LENGTH OF nTH SHUT TIME
c Next section for id=101 (first latency, nth=0) or for nth shut time
c (id=102) (nth=1,2..), or last shut time (id=105, nth=-1)
c nyval=number of jumps, at most
c Want to print mean value of nth open in ms, and also as % of mean
c length of all openings (exc latency and last) -this is defined as
c shutav when 'all shut times' is done (must be done before 'mean for nth')
	j=0 		!to count good gaps
	nfitt=0	!to count truncated values
	nbad=0	!to count number of sweeps with no nth gap
	nz1=0		!count zero first latencies
	nz2=0		!count zero first latencies
c
	do 200 ij=1,nsweep
c
	if(mod(ij-1,ndeb).eq.0) then	!check if debug on every ndebth jump
	   debon=debug()
	endif
	if(debon) then
	   print 505,jumps(ij)
         if(discprt) write(8,505) jumps(ij)
505	   format(' Sweep # ',i4)
	endif
c Add bit to deal with jumps marked bad by setting jumps() to be negative
c in SCAN (not yet done)
	if(jumps(ij).lt.0) then
	   print 5051,jumps(ij)
         if(discprt) write(8,5051) jumps(ij)
5051	   format(' Sweep # ',i4,' has been omitted')
	   goto 200   !omit this sweep altogether
	endif
	if(nomit.gt.0) then
	   do i=1,nomit
		if(jumpomit(i).eq.ij) then
		   print 5051,jumps(ij)
      	   if(discprt) write(8,5051) jumps(ij)
c5051		   format(' Sweep # ',i4,' has been omitted')
		   goto 200   !omit this sweep altogether
		endif
	   enddo
	endif
c
	ifirst=kjumps(ij)	!index in tint() of 1st interval in jth jump
	if(ij.lt.nsweep) then
	   ilast=kjumps(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nint
	endif
c
	if(ifirst.eq.ilast) then
	   null=BTEST(iprops(ifirst),5)
	else
	   null=BTEST(iprops(ifirst),5).and.BTEST(iprops(ifirst+1),5)
	endif
	if(null.and.excnull) then	!print if req and jump to next sweep
	   if(debon) then
		print 503,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,503) k,tint(k),ampl(k),iprops(k)
503		format(i6,g12.5,g12.5,i5,' (null sweep)')
	   endif
	   goto 200
	endif
c
	if(nth.eq.0) then		!first latency
	   k=ifirst			!for brevity
	   if(debon) then
		print 501,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
	   endif
	   zero=.false.
	   if(BTEST(iprops(k),5).and.ampl(k).ne.0) then
		zero=.true.
		nz1=nz1+1
	   endif
	   if(.not.null) then
		if(tint(k)-toffset.le.0.0) then
		   zero=.true.
		   nz2=nz2+1
		endif
	   endif
c
	   last=null.or.BTEST(iprops(k),6)
c
	   if(exass) then
		k2=ifirst+1
		if(k2.le.ilast.and.ampl(k2).ne.0) then
		   if(dubious(k2)) goto 200
		endif
	   endif
c
c     -if we reach here, then define a valid value
	   if(null) then
		nfitt=nfitt+1
		yvalt(nfitt)=tint(k)-toffset
		if(debon) then
		   print 504,nfitt,yvalt(nfitt),jumps(ij)
      	   if(discprt) write(8,504) nfitt,yvalt(nfitt),jumps(ij)
504		   format(36x,' Yvalt(',i4,') = ',g12.5,' (jump ',i5,')')
	      endif
	   else
	      j=j+1
	      YVAL(j)=tint(k)-toffset
		if(zero) YVAL(j)=0.0
		if(debon) then
		   print 502,j,yval(j),jumps(ij)
      	   if(discprt) write(8,502) j,yval(j),jumps(ij)
502		   format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
	      endif
	   endif
c
	else if(nth.eq.-1) then		!last shut time
	   k=ilast			!for brevity
	   if(.not.null) then
		n=-1
		do k=ilast-1,ifirst,-1
		   if(ampl(k).eq.0) then
			n=k
			goto 600
		   endif
		enddo
600		continue
		if(debon) then
		   k1=n-2
		   if(k1.lt.ifirst) k1=ifirst
		   do k=k1,ilast
			print 501,k,tint(k),ampl(k),iprops(k)
      		if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
		   enddo
		endif
		if(n.gt.ifirst.and.n.lt.ilast) then 	!valid value -assign yval
		   if(exass) then
			if(ampl(n-1).ne.0) then
			   if(dubious(n-1)) goto 200
			endif
			if(ampl(n+1).ne.0) then
			   if(dubious(n+1)) goto 200
			endif
		   endif
		   j=j+1
	         YVAL(j)=tint(n)
		   if(debon) then
			print 502,j,yval(j),jumps(ij)
      		if(discprt) write(8,502) j,yval(j),jumps(ij)
		   endif
		endif
	   else	!if null
		nfitt=nfitt+1
		yvalt(nfitt)=tint(k)-toffset
		if(debon) then
		   print 504,nfitt,yvalt(nfitt),jumps(ij)
      	   if(discprt) write(8,504) nfitt,yvalt(nfitt),jumps(ij)
	      endif
	   endif
c===
	else if(nth.gt.0) then
c 	   Now nth>0. First locate the nth shut time.
c	   Skip the first latency, if it is shut at t=0
	   k=ifirst				!start looking at tint(k)
	   fstlat=BTEST(iprops(k),5)
c	   zero=BTEST(iprops(ifirst),5).and.ampl(ifirst).ne.0
	   if(fstlat) then
		zero=.false.
		if(ampl(k).ne.0) then
		   zero=.true.
		   nz1=nz1+1
		endif
		if(.not.null) then
		   if(tint(k)-toffset.le.0.0) then
			zero=.true.
			nz2=nz2+1
		   endif
		endif
	   endif
	   if(zero) then
		if(debon) then	!print first for completeness
		   print 501,k,tint(k),ampl(k),iprops(k)
      	   if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
		endif
	   endif
c
c   Start with n=-1 so first shut time corresponds to n=0 =latency
c    unless zero=true in which case the first shut time follows an
c    opening and counts as n=1
	   n=-1
	   if(zero) n=0
	   do while(n.lt.nth.and.k.le.ilast)
		if(debon) then
		   print 501,k,tint(k),ampl(k),iprops(k)
      	   if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
		endif
		if(ampl(k).eq.0) n=n+1
		k=k+1
	   enddo
	   k=k-1
	   fstlat=BTEST(iprops(k),5)
c       At this point n=nth and tint(k) is the required nth shut time (unless
c	  loop was exited because k.gt.ilast, in which case may have n<nth)
	   if(n.lt.nth) then
		nbad=nbad+1
		goto 200	!no nth gap
	   endif
c
	   last=null.or.BTEST(iprops(k),6)
c
	   if(exass) then
		k0=k-1
		k2=k+1
		if(.not.BTEST(iprops(k),5)) then		!no k-1 for 1st interval
		   if(k0.gt.0.and.ampl(k0).ne.0) then
			if(dubious(k0)) goto 200
		   endif
		endif
		if(k2.le.ilast.and.ampl(k2).ne.0) then
		   if(dubious(k2)) goto 200
		endif
	   endif
c
	   if(zero.and.exczero) goto 200
	   if(k.eq.ilast.and.exclast) goto 200
	   if(badint(k)) goto 200
	   if(last.and.exclast) goto 200
c
c     -if we reach here, then define a valid value
	   if(last) then
		nfitt=nfitt+1
		yvalt(nfitt)=tint(k)
		if(debon) then
		   print 504,nfitt,yvalt(j),jumps(ij)
      	   if(discprt) write(8,504) nfitt,yval(j),jumps(ij)
	      endif
	   else
	      j=j+1
	      YVAL(j)=tint(k)
c==		if(zero) YVAL(j)=0.0
		if(debon) then	!print interval that ends nth shut time
		   k1=k+1
		   print 501,k1,tint(k1),ampl(k1),iprops(k1)
      	   if(discprt) write(8,501) k1,tint(k1),ampl(k1),iprops(k1)
		endif
		if(debon) then
		   print 502,j,yval(j),jumps(ij)
      	   if(discprt) write(8,502) j,yval(j),jumps(ij)
	      endif
	   endif
	   if(fstlat) Yval(j)=tint(k)-toffset
	endif		!end of if(nth.eq.0)...else if(nth.gt.0) ...
c
200	continue	!end of k loop
	NYVAL=j     !no of usable gaps
	goto 207	!Yval() now defined
c
c ALL SHUT TIMES
c (and do shut time correlations here)
398	continue
c must allocate arrays for corcalj (index not needed but mus declare it anyway)
	if(allocated(ygap)) DEALLOCATE(ygap,index,indexg)
	ALLOCATE(ygap(nint),index(nint),indexg(nint))
	j=0	!to count good gaps
	nj1=1		!initialise for GETJUMP
	nz1=0		!count zero first latencies
	nz2=0		!count zero first latencies
c
	do 205 k=1,nint
	if(mod(k-1,100).eq.0) then	!check if debug on every 100th transition
	   debon=debug()
	endif
	if(debon) then
	   print 501,k,tint(k),ampl(k),iprops(k)
         if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
501	   format(i6,g12.5,g12.5,i5)
	endif
C  -exclude if not shut, or if unusable
c====ONLY modif for 2D arrays so far is to replace call to getjump() with
c====call to getjumps(), to allow compilation
	call GETJUMPs(k,ijump,njump,nj1,jumps,kjumps,nsweep,
     & jset,nswpmax,nset)

	ij=ijump		!for brevity
	if(nomit.gt.0) then
	   do i=1,nomit
		if(jumpomit(i).eq.ijump) goto 205   !omit this sweep altogether
	   enddo
	endif
	ifirst=kjumps(ijump)	!index in tint() of 1st interval in jth jump
	if(ij.lt.nsweep) then
	   ilast=kjumps(ijump+1)-1	!index of last transition in ijth jump
	else
	   ilast=nint
	endif
	if(ampl(k).ne.0.or.badint(k)) goto 205
	if(exass) then
	   k0=k-1
	   k2=k+1
	   if(.not.BTEST(iprops(k),5)) then		!no k-1 for 1st interval
		if(k0.gt.0.and.ampl(k0).ne.0) then
		   if(dubious(k0)) goto 205
		endif
	   endif
	   if(k2.le.ilast.and.ampl(k2).ne.0) then
		if(dubious(k2)) goto 205
	   endif
	endif
c
	if(k.eq.nint) then
	   null=BTEST(iprops(k),5)
	else
	   null=BTEST(iprops(k),5).and.BTEST(iprops(k+1),5)
	endif
	if(null.and.excnull) goto 205
c
	fstlat=BTEST(iprops(k),5)
	if(fstlat.and.excflat) goto 205
c
c	zero=BTEST(iprops(k),5).and.ampl(k).ne.0
c	zero=fstlat.and.ampl(k).ne.0
	if(fstlat) then
	   zero=.false.
	   if(ampl(k).ne.0) then
		zero=.true.
		nz1=nz1+1
	   endif
	   if(.not.null) then
		if(tint(k)-toffset.le.0.0) then
		   zero=.true.
		   nz2=nz2+1
		endif
	   endif
	endif
c
	if(zero.and.exczero) goto 205

	last=null.or.BTEST(iprops(k),6)
	if(last.and.exclast) goto 205
c
c     -if we reach here, then define a valid value
	if(last) then
	   nfitt=nfitt+1
	   yvalt(nfitt)=tint(k)
	   if(debon) then
		print 504,nfitt,yvalt(j),jumps(ij)
      	if(discprt) write(8,504) nfitt,yval(j),jumps(ij)
	   endif
	else
	   j=j+1
	   YVAL(j)=tint(k)
	   if(zero) YVAL(j)=0.0
	   ygap(j)=yval(j)	!define for corcalj
	   indexg(j)=jumps(ij)	!record jump in which ygap(j) occurs
	   if(debon) then
		print 502,j,yval(j),jumps(ij)
      	if(discprt) write(8,502) j,yval(j),jumps(ij)
	   endif
	endif
c
205	continue	!end of k loop
c
	NYVAL=j     !no of usable gaps
c Calc mean of all shut times, to be used for normalising nth shut time
	sum=0.0
	do i=1,nyval
	   sum=sum+yval(i)
	enddo
	shutav=sum/float(nyval)
c Do shut-shut correlations
	ans='N'
	call DCASK(
     &  'Calculate autocorrelations between shut times',ans,ans)
	if(ans.eq.'Y') then
	  call CORCALJ(yval,ygap,index,indexg,nsweep,nyval,ndimy,nint,
     &    idiskq,3)
	endif
c
	DEALLOCATE(ygap,index,indexg)
c
	goto 207	!Yval() now defined
c
c MEANS FOR nTH SHUT TIME
399	continue
      print 403,shutav
      if(pon()) write(7,403) shutav
      if(discprt) write(8,403) shutav
403	format(' Mean of all shut times = ',g13.6,' ms',/,
     &' interval #    n     mean',7x,'SD(mean)',9x,'range',17x,'mean as'
     &,/,'                         ',9x,'  ',13x,'     ',17x,'% of all')
c
	do 401 nth=0,nthmax	!loop for values of nth
	j=0 		!to count good gaps
	nfitt=0	!to count truncated values
	nbad=0	!to count number of sweeps with no nth gap
	nz1=0		!count zero first latencies
	nz2=0		!count zero first latencies

	do 402 ij=1,nsweep
c
	if(nomit.gt.0) then
	   do i=1,nomit
		if(jumpomit(i).eq.ij) goto 402   !omit this sweep altogether
	   enddo
	endif
c
	ifirst=kjumps(ij)	!index in tint() of 1st interval in jth jump
	if(ij.lt.nsweep) then
	   ilast=kjumps(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nint
	endif
c
	if(ifirst.eq.ilast) then
	   null=BTEST(iprops(ifirst),5)
	else
	   null=BTEST(iprops(ifirst),5).and.BTEST(iprops(ifirst+1),5)
	endif
	if(null.and.excnull) then	!print if req and jump to next sweep
	   goto 402
	endif
c
	if(nth.eq.0) then		!first latency
	   k=ifirst			!for brevity
c	   zero=BTEST(iprops(ifirst),5).and.ampl(ifirst).ne.0
	   zero=.false.
	   if(BTEST(iprops(k),5).and.ampl(k).ne.0) then	!=32 = first op in jump
		zero=.true.
		nz1=nz1+1
	   endif
	   if(.not.null) then
		if(tint(k)-toffset.le.0.0) then
		   zero=.true.
		   nz2=nz2+1
		endif
	   endif
c
	   if(zero.and.exczero) goto 402
c
	   if(badint(k)) goto 402
c
	   last=null.or.BTEST(iprops(k),6)
	   if(last.and.exclast) goto 402
c
	   if(exass) then
		k2=ifirst+1
		if(k2.le.ilast.and.ampl(k2).ne.0) then
		   if(dubious(k2)) goto 402
		endif
	   endif
c
c     -if we reach here, then define a valid value
	   if(null) then
		nfitt=nfitt+1
		yvalt(nfitt)=tint(k)-toffset
	   else
	      j=j+1
	      YVAL(j)=tint(k)-toffset
c		if(zero) YVAL(j)=0.0
	   endif
c
	else if(nth.gt.0) then		!end of nth=0
c 	   Now nth>0. First locate the nth shut time.
c	   Skip the first latency, if it is shut at t=0
	   k=ifirst				!start looking at tint(k)
c
	   fstlat=BTEST(iprops(k),5)
c
c	   zero=BTEST(iprops(ifirst),5).and.ampl(ifirst).ne.0
	   if(fstlat) then
		zero=.false.
		if(ampl(k).ne.0) then
		   zero=.true.
		   nz1=nz1+1
		endif
		if(.not.null) then
		   if(tint(k)-toffset.le.0.0) then
			zero=.true.
			nz2=nz2+1
		   endif
		endif
	   endif
c
c=	   if((fstlat.and.excflat).or.(zero.and.exczero)) k=ifirst+1
c (start looking at tint(k)=2nd interval in jump in above cases)
c=	   n=0
c   Start with n=-1 so first shut time corresponds to n=0 =latency
c    unless zero=true in which case the first shut time follows an
c    opening and counts as n=1
	   n=-1
	   if(zero) n=0
	   do while(n.lt.nth.and.k.le.ilast)
		if(ampl(k).eq.0) n=n+1
		k=k+1
	   enddo
	   k=k-1
c       At this point n=nth and tint(k) is the required nth shut time (unless
c	  loop was exited because k.gt.ilast, in which case may have n<nth)
	   if(n.lt.nth) then
		nbad=nbad+1
		goto 402	!no nth gap
	   endif
	   if(k.eq.ilast.and.exclast) goto 402
c
	   if(badint(k)) goto 402
c
	   last=null.or.BTEST(iprops(k),6)
	   if(last.and.exclast) goto 402
c
	   if(exass) then
		k0=k-1
		k2=k+1
		if(.not.BTEST(iprops(k),5)) then		!no k-1 for 1st interval
		   if(k0.gt.0.and.ampl(k0).ne.0) then
			if(dubious(k0)) goto 402
		   endif
		endif
		if(k2.le.ilast.and.ampl(k2).ne.0) then
		   if(dubious(k2)) goto 402
		endif
	   endif
c
c     -if we reach here, then define a valid value
	   fstlat=BTEST(iprops(k),5)
	   if(last) then
		nfitt=nfitt+1
		yvalt(nfitt)=tint(k)
	   else
	      j=j+1
	      YVAL(j)=tint(k)
		if(fstlat) Yval(j)=tint(ifirst)-toffset
c==		if(zero) YVAL(j)=0.0
	   endif
	endif
c
402	continue	!end of loop for each sweep
	NYVAL=j     !no of usable gaps
	if(nyval.ge.2) then
	   call VARV1(Yval,Nyval,YBAR,VARY,ymin,ymax)
	   sd=sqrt(vary/float(nyval))	!now SD of mean
	else if(nyval.eq.1) then
	   ybar=yval(1)
	   vary=-1.
	   sd=-1.
	endif
	if(nth.eq.0) then
	   string='1st latency'
	else
	   call INTCONV(nth,string)
	endif
	if(nyval.ge.2) then
	   pm=100.*ybar/shutav		!as % of all shut times
	   print 404,string,NYVAL,YBAR,sd,ymin,ymax,pm
	   if(discprt) write(8,404) string,NYVAL,YBAR,sd,ymin,ymax,pm
404	   FORMAT(1x,a11,i5,g13.6,g12.5,'(',g11.4,' to 'g11.4,')',1x,f8.2)
	   ameanval(nth)=ybar
	   sdval(nth)=sd
	else if(nyval.eq.1) then
	   print 4041,string,NYVAL,yval(1)
	   if(discprt) write(8,4041) string,NYVAL,Yval(1)
4041	   FORMAT(1x,a11,i5,g13.6)
	endif
c
401	continue	!end of loop for nth
	goto 999	!means only -no graph
c
c
c OPEN TIME OPTIONS
30	continue
	tres=treso
c
	if(allocated(ygap)) DEALLOCATE(ygap,index,indexg)
	if(.not.conam1) then
	   ALLOCATE(ygap(nint),index(nint),indexg(nint))
	endif
c
	print 31
31	format(
     & ' (1) Length of nth open period in the jump',/,
     & ' (2) Length of all open periods',/,
     & ' (3) Mean length of nth open period for nth = 0, 1,...,n',/,
     & ' (4) Number of open periods per jump',/,
     & ' (5) Mean open period adjacent to shut time in spec range',/,
     & ' (6) Mean open period adjacent to open period in spec range',/,
     & ' (7) Length of last open period in the jump',/,
     & ' Option number = ')
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.7) goto 30
	ID=110+iopt			!iopt=111,112,113,114,115,116,117
	OHIST=.true.
	thist=.true.
	if(id.eq.114) then
	   ohist=.false.
	   thist=.false.
	   obhist=.true.
	endif
	if(id.eq.111) then
         print 32
         if(pon()) write(7,32)
         if(discprt) write(8,32)
32	   format(/,' Length of the nth open period after t=0')
	else if(id.eq.112) then
         print 33
         if(pon()) write(7,33)
         if(discprt) write(8,33)
33	   format(/,' Length of all open periods')
	else if(id.eq.113) then
	   if(openav.lt.0.0) then
	      print 212
212 		format(
     &	' Do option 2 first to define mean of all open periods')
		iopt=2
		goto 30
	   endif
         print 36
         if(pon()) write(7,36)
         if(discprt) write(8,36)
36	   format(/,' Mean length of nth open period')
	else if(id.eq.114) then
         print 44
         if(pon()) write(7,44)
         if(discprt) write(8,44)
44	   format(/,' Number of open periods per jump')
	else if(id.eq.115) then
         print 45
         if(pon()) write(7,45)
         if(discprt) write(8,45)
45	   format(/,
     &' Mean lengths of open periods adjacent to shut times'
     &' in specified range')
	else if(id.eq.116) then
         print 452
         if(pon()) write(7,452)
         if(discprt) write(8,452)
452	   format(/,
     &' Mean lengths of open periods adjacent to open periods'
     &' in specified range')
	else if(id.eq.117) then
         print 453
         if(pon()) write(7,453)
         if(discprt) write(8,453)
453	   format(/,' Length of the last open period in jump')
	endif
      print 27          !underline
      if(pon()) write(7,27)
      if(discprt) write(8,27)
c27	format(' ================================================',/)
	nth=0
	if(id.eq.111) then
34	   nth=1
	   print 35
35	   format(
     &   ' Length of the nth open period after t=0 (n=1,2,..); n = ')
	   call INPUTi(nth)
	   if(nth.lt.1) goto 34
	   nthsav=nth		!loop may change value
         if(pon()) write(7,28) nth
         if(discprt) write(8,28) nth
c28	   format(' ( n = ',i3,')')
	endif
	if(id.eq.113) then
	   nthmax=20
	   print 311
311	   format(' Calculate means for n up to n [20] = ')
	   call INPUTi(nthmax)
c        For id=113 allocate arrays to keep nth, mean(%), sd in sx0,pm0,sdp
	   n1=nthmax   	!for brevity
	   n2=1
	   if(allocated(sx0)) then
		DEALLOCATE(sx0,pm0,sdp)
	   endif
	   ALLOCATE(sx0(n1,n2),pm0(n1,n2),sdp(n1,n2))
	endif
c
	excnull=.true.	!nulls have no open periods
	exczero=.true.
	exclast=.true.
	exass=.true.
	if(id.eq.112) then	!option for 'all open periods' only
	   call DCASK('Specify amplitude range for openings','n',ans)
	   conam1=ans.eq.'Y'
	endif
	if(conam1) then
	   print 2271
2271	   format(' Define amplitude range:',/,
     &   ' (Must be in this range throughout a contiguous open period)')
	   print 227
227	   format('  Low amp, high amp (pA with sign) = ')
	   call INPUT2r(amplo,amphi)
	   if(amphi.lt.amplo) then
		a1=amphi	!swap
		amphi=amplo
		amplo=a1
	   endif
	endif
c
	ans='Y'
	if(id.eq.114) ans='N'	!better include for ops/bst
	call DCASK(
     & 'Exclude open periods with ''dubious'' amplitude openings',
     &  ans,ans)
	exass=UC(ans).ne.'N'
	if(exass) then
         if(pon()) write(7,56)
	   if(discprt) write(8,56)
56	   format(
     &  ' Open periods that have openings with ''dubious'' amplitudes',
     &  ' are excluded')
	else
         if(pon()) write(7,55)
	   if(discprt) write(8,55)
55	   format(
     &  ' Open periods that have openings with ''dubious'' amplitudes',
     &  ' are included')
	endif
c
	if(conam1) then
	   if(pon()) write(7,29) amplo,amphi
	   if(discprt) write(8,29) amplo,amphi
	   if(shist) print 29,amplo,amphi
29	   format(' Openings with amplitudes beween ',f7.3,' and ',
     &	 f7.3,' pA only',/,
     & ' (must be within this range throughout contiguous open period)')
	endif
	ans='Y'
	if(id.eq.114) ans='N'	!better include for ops/bst
	call DCASK(
     & 'Exclude open periods that are open at t=0',ans,ans)
	if(ans.eq.'N') exczero=.false.
	if(exczero) then
         if(pon()) write(7,57)
	   if(discprt) write(8,57)
57	   format(
     &' Open periods that are open at t=0 are excluded',/)
	 else
	   print 58
         if(pon()) write(7,58)
	   if(discprt) write(8,58)
58	   format(
     &' Open periods that are open at t=0 are included',/)
	   print 58
	 endif
	ans='Y'
	if(id.eq.114) ans='N'	!better include for ops/bst
	call DCASK('Exclude open periods that are open at end',ans,ans)
	exclast=ans.eq.'Y'
	if(exclast) then
         if(pon()) write(7,60)
	   if(discprt) write(8,60)
60	   format(
     &' Open periods that are open at the end of a sweep (truncated)' ,/,
     &'  are omitted',/)
	else
         if(pon()) write(7,61)
	   if(discprt) write(8,61)
61	   format(
     &' Open periods that are open at the end of a sweep (truncated)' ,
     &'  are INCLUDED',/)
	endif
c
	if(id.eq.113) then
         print 4031,openav,sdopen
         if(pon()) write(7,4031) openav,sdopen
         if(discprt) write(8,4031) openav,sdopen
4031	   format(
     &' Mean of all open periods = ',g13.6,' ms: SD(mean) = ',g13.6,/,
     &' interval #    n     mean',7x,'SD(mean)',9x,'range',17x,'mean as'
     &,/,'                         ',9x,'  ',13x,'     ',17x,'% of all')
	   n1=1		!so 'nth=' loop done for nth = 1,...,nthmax
	else
	   n1=1
	   nthmax=1		!so 'nth=' loop done once only!
	   debon=.false.
	endif
c
c===	do 405 nth=n1,nthmax	!loop for values of nth, for calc of means
	do 405 nt1=n1,nthmax	!loop for values of nth, for calc of means
c
	j=0 		!to count good open periods (all open periods)
	j1=0		!to count nth open periods
	j2=0		!to count number of open periods per sweep
	nfitt=0	!to count truncated values (open at end)
	nbad=0	!to count number of sweeps with no nth open period
	nbad1=0	!to count number of open periods abandoned because
c			!of bad interval or dubious amplitude
c
c
	do 38 ij=1,nsweep
c
	if(mod(ij-1,ndeb).eq.0) then	!check if debug on every ndebth jump
	   debon=debug()
	endif
	if(debon) then
	   print 505,jumps(ij)
         if(discprt) write(8,505) jumps(ij)
c505	   format(' Sweep # ',i4)
	endif
	if(nomit.gt.0) then
	   do i=1,nomit
		if(jumpomit(i).eq.ij) goto 38   !omit this sweep altogether
	   enddo
	endif
c
	ifirst=kjumps(ij)	!index in tint() of 1st interval in jth jump
	if(ij.lt.nsweep) then
	   ilast=kjumps(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nint
	endif
c
	if(ifirst.eq.ilast) then
	   null=BTEST(iprops(ifirst),5)
	else
	   null=BTEST(iprops(ifirst),5).and.BTEST(iprops(ifirst+1),5)
	endif
c
	if(null.and.excnull) then	!print if req and jump to next sweep
	   if(debon) then
		print 503,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,503) k,tint(k),ampl(k),iprops(k)
c503		format(i6,g12.5,g12.5,i5,' (null sweep)')
	   endif
	   goto 38
	endif
c
	if(exclast) then
	   a1=ampl(ilast)
	   if(a1.ne.0) ilast=ilast-1	!omit last (truncated) opening
	endif
c
	if(zero.and.exczero) goto 38
c
c Find start of a group (open period) -valid start must have a good shut
c time followed by a good opening -if a bad opening is found as first (or
c any later) opening then the open period is abandoned altogether, and the
c next good shut time sought as start for next open period, but for the
c purposes of identifying the nth open period, rejected ones must be counted
c as an open period even though their length is undefined.
c This version requires a shut time first, so cannot include shut periods
c for which channel is open at t=0
	ifst=ifirst
	n=0			!counts number of open periods
	sum=0.0
c If open period that is open at t=0 is to be included, then an alternative
c bit of code is needed to find the start of the first open period in a jump
c (but this could have more openings concat with it)
	if(.not.exczero) then
	   a1=ampl(ifst)
	   open1=a1.ne.0
	   setbad1=badint(ifst)		!tint(i) was unusable
	   bad1=setbad1.or.(dubious(ifst).and.exass)
	   if(bad1) then
		n=n+1      		!count this as nth open period, although not usable
		goto 39  		!look for next good regular start
	   else
		n=n+1 		!count this as nth open period
		ifst=ifst-1		!so ifst set correctly after 53
		goto 53		!valid 'start' now found -look for more openings
	   endif
	endif
c Look for normal (not open at t=0) 'start of open period'
39	continue
	do while (ifst.le.ilast-1)
	   a1=ampl(ifst)
	   a2=ampl(ifst+1)
	   if(.not.conam1) then
	      open1=a1.ne.0
	      open2=a2.ne.0
	   else
	      open1=(A1.ge.amplo).and.(A1.le.amphi)
	      open2=(A2.ge.amplo).and.(A2.le.amphi)
	   endif
	   shut1=.not.open1
	   setbad1=badint(ifst)		!tint(i) was unusable
	   setbad2=badint(ifst+1)		!tint(i) was unusable
	   bad1=setbad1.or.(dubious(ifst).and.exass)
	   bad2=setbad2.or.(dubious(ifst+1).and.exass)
	   bad=.false.
	   if(bad1.or.bad2) then
		ifst=ifst+1
		n=n+1      		!count this as nth open period, although not usable
		bad=.true.
		goto 39  		!look for next good start
	   else if(shut1.and.open2) then
		n=n+1 		!count this as nth open period
		goto 53		!valid 'start of open period' now found
	   endif
	   ifst=ifst+1
	enddo
c if this point reached, all intervals inspected
	j=j-1
	goto 381	!all intervals in this jump inspected
c valid 'start of open period' found
53	continue
	j=j+1		!count number of open periods found (reversed if bad)
	ifst=ifst+1		!index of first opening in open period just started
c==	Yval(j)=tint(ifst)	!length of this opening
	sum=tint(ifst)	!length of this opening
	if(debon) then	!print first in group
	   print 501,ifst,tint(ifst),ampl(ifst),iprops(ifst)
         if(discprt) write(8,501)ifst,tint(ifst),ampl(ifst),iprops(ifst)
	endif
	bad=.false.
c if last interval is open, and not excluded, there are no more intervals
c to find, and do not want the next 'do while' to decrement j, so skip to end.
	if(.not.exclast.and.ifst.eq.ilast) then
	   k=ifst	!so do not return to 39 to look for new group
	   goto 62
	endif
	k=ifst+1	!index of interval that follows the first opening
c Now find the rest of the openings in this open period
	do while (k.le.ilast)
	   if(debon) then		!print rest in group (inc shutting that ends it)
		print 501,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
	   endif
	   a1=ampl(k)
	   open1=a1.ne.0
	   if(conam1) open1=(A1.ge.amplo).and.(A1.le.amphi)	!use amp constraint?
c Go to 62 when end of group found.
c (1) If no amplitude constraint group ends when ANY shut period found
c    (whether usable or unusable)
c (2) If amplitude constrained group ends when any amplitude outside
c    the specified range is found.
c Whole group is set bad, and so rejected, if (1) contains any unusable
c opening (2) when, if exass=true, it contains any opening with dubious
c amplitude OR if the interval that ends the group has 'dubious'amp.
	   if(exass.and.dubious(k)) bad=.true.
	   if(.not.open1) goto 62		!end of group found
c==	   YVAL(j)=YVAL(j)+tint(k)	!add any subsequent openings to same YVAL
	   sum=sum + tint(k)	!add any subsequent openings to same YVAL
	   setbad=badint(k)		!tint(k) was unusable
	   if(setbad) bad=.true.	!this group bad, but finish anyway
	   k=k+1
	enddo
	j=j-1
	goto 381	!all intervals in this jump inspected
c
c Group completed
62	continue
	if(bad) nbad1=nbad1+1	!count # of above case
c
c Print result (if good)
c Allocate yval for 'all open periods' (and means)
	if(.not.bad.and.id.eq.112.or.id.eq.115.or.id.eq.116) then
	   yval(j)=sum
	   index(j)=jumps(ij)	!record jump in which yval(j) occurs
	   sum=0.0
	   if(debon) then
		print 502,j,yval(j),jumps(ij)
	      if(discprt) write(8,502) j,yval(j),jumps(ij)
c502		format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
	   endif
	endif
c Allocate yval for 'nth open period'
	if(id.eq.111.and.(.not.bad)) then
	   if(n.eq.nth) then
		j1=j1+1		!counter for nth open period values
		Yval(j1)=sum
		index(j1)=jumps(ij)	!record jump in which yval(j1) occurs
		sum=0.0
		if(debon) then
		   print 502,j1,yval(j1),jumps(ij)
	         if(discprt) write(8,502) j1,yval(j1),jumps(ij)
c502		   format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
		endif
		goto 38		!next jump
	   endif
	endif
	if(id.eq.113.and.(.not.bad)) then
	   if(n.eq.nt1) then
		j1=j1+1		!counter for nth open period values
		Yval(j1)=sum
		index(j1)=jumps(ij)	!record jump in which yval(j1) occurs
		sum=0.0
		if(debon) then
		   print 502,j1,yval(j1),jumps(ij)
	         if(discprt) write(8,502) j1,yval(j1),jumps(ij)
c502		   format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
		endif
		goto 38		!next jump
	   endif
	endif
c==	ifst=k+1	!another group completed (j updated above)
	ifst=k	!so do not skip past shut time needed to start new group
	last=null.or.BTEST(iprops(k),6)
	if(conam1) then	!can't define ygap() when amp constrained
	   if(bad) j=j-1	!if bad do not use. YVAL(j) gets overwritten
	   if(ifst.lt.ilast) goto 39	!look for start of new group
	else
	   ygap(j)=TINT(k)   !record length of shut time following open period #j
	   indexg(j)=jumps(ij)	!record jump in which ygap(j) occurs
c NB ygap() is used only for id=115 and id=116 so bad value does not
c matter here e.g. for open periods per jump (id=114) for which last
c opening is used (if open at end there is no following gap -see next bit)
	   if(ampl(k).ne.0) then
		ygap(j)=-1.e20		!error value!
		if(BTEST(iprops(k),6)) then
	         print 702,k
	         if(discprt) write(8,702) k
702	         format(' Event #',i5,' is open at end of jump')
		else
	         print 700,k,tint(k),ampl(k),iprops(k)
	         if(discprt) write(8,700) k,tint(k),ampl(k),iprops(k)
700	         format(' YGAP ERROR:',i5,2g13.6,i3)
		endif
	   endif
	   if(debon) then
		print 701,j,ygap(j)
		if(discprt) write(8,701) j,ygap(j)
701		format(16x,'  Ygap(',i4,') = ',g13.6,/)
	   endif
	   if(bad) j=j-1	!if bad do not use. YVAL(j) gets overwritten
	   if(ifst.lt.ilast) goto 39	!look for start of new group
	endif
C N.B. does not return if ifst=nint, as if tint(nint) is start of a
C group there is no way to tell if this group has been completed-
C (the next interval might have been open too)
C  NOTE that NYVAL=naset calc here may not be identical with that
C found in calc of burst distributions because latter includes
C only those open periods that are part of valid bursts (no bad
C openings and proper end)
c
c If no new group started, the last open period must have been the last
c in the current jump, so record it when id=117
	if((id.eq.117).and.(.not.bad)) then
	   j1=j1+1		!counter for nth open period values
	   Yval(j1)=sum
	   index(j1)=jumps(ij)	!record jump in which yval(j1) occurs
	   sum=0.0
	   if(debon) then
		print 502,j1,yval(j1),jumps(ij)
	      if(discprt) write(8,502) j1,yval(j1),jumps(ij)
c502		format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
	   endif
	   goto 38		!next jump
	endif
c
381	continue
	if(id.eq.111.and.n.lt.nth) then
	   nbad=nbad+1
	   if(debon) then
		print 63,jumps(ij)
	      if(discprt) write(8,63) jumps(ij)
63		format(' No nth open period in jump ',i5)
	   endif
	endif
c Allocate yval for number of open periods per jump
	if(id.eq.114) then
	   j2=j2+1
	   yval(j2)=float(n)
	   index(j2)=jumps(ij)	!record jump in which yval occurs
	   if(debon) then
		print 502,j2,yval(j2),jumps(ij)
	      if(discprt) write(8,502) j2,yval(j2),jumps(ij)
c502		format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
	   endif
	endif
38	continue 	!end of loop for each jump
c
	if(id.eq.111.or.id.eq.113.or.id.eq.117) then
	   NYVAL=j1		!no of nth open periods
	else if(id.eq.112.or.id.eq.115.or.id.eq.116) then
	   NYVAL=j		!no of usable open periods
	   if(nyval.ge.2) then
	      call VARV1(Yval,nyval,openav,VARY,ymin,ymax)
		sdopen=sqrt(vary/float(nyval))	!sd of mean open
	   else if(nyval.eq.1) then
		openav=yval(1)
		vary=-1.
		sd=-1.
	   endif
c=	   ameanval(nth)=ybar
c=	   sdval(nth)=sd
	   ameanval(nt1)=ybar
	   sdval(nt1)=sd
	else if(id.eq.114) then
	   NYVAL=j2		!no of open periods/jump values
	endif
c
	if(id.eq.113) then
	   if(nyval.ge.2) then
		call VARV1(Yval,Nyval,YBAR,VARY,ymin,ymax)
	      sd=sqrt(vary/float(nyval))	!now SD of mean
	   else if(nyval.eq.1) then
		openav=yval(1)
		vary=-1.
		sd=-1.
	   endif
	   if(nyval.ge.2) then
		call INTCONV(nt1,string)
		pm=100.*ybar/openav		!as % of all open period times
		print 404,string,NYVAL,YBAR,sd,ymin,ymax,pm
		if(discprt) write(8,404) string,NYVAL,YBAR,sd,ymin,ymax,pm
		ameanval(nt1)=ybar
		sdval(nt1)=sd
c404		FORMAT(1x,a11,i5,g13.6,g12.5,'(',g11.4,' to 'g11.4,')',1x,f8.2)
	   else if(nyval.eq.1) then
		print 4041,string,NYVAL,yval(1)
		if(discprt) write(8,4041) string,NYVAL,Yval(1)
c4041		FORMAT(1x,a11,i5,g13.6)
	   endif
c
c Keep data for ascii output at end
c Approx SD of m=ybar/openav if g<<1 and indep is
c (1/openav)*sqrt[sd**2 + (m*sdopen)**2], where sdopen=SD of openav and
c sd is sd of ybar.
	   sx0(nt1,1)=float(nth)
	   pm0(nt1,1)=pm
c*	   sdp(nt1,1)=100.*sd/openav    !approx SD of pm0 (if denom exact)
	   em=ybar/openav
	   sdp(nt1,1)=100.*sqrt((sd*sd) + (em*em*sdopen*sdopen))/openav
	endif
c
405	continue	!end of loop for nth=1,2,...,nthmax
c
	if(id.eq.113) then
	   ans='N'
	   call DCASK('Save values as ASCII file',ans,ans)
	   if(ans.eq.'Y') then
c Want alternating columns of pm0,sx0 both of which are declared (nlag,nval)
c ie have nval columns
		ALLOCATE(data(nthmax,3))
		ndata=nthmax	!number of lines of data
		ncol=3		!nval entries in each line
		icolwid=13		!the minimum col width
		colhead(1)='nth'
		colhead(2)='mean open'
		colhead(3)='SD(mean)'
		ndim1=nthmax
		ndim2=3
		text=
     &	'Mean OPEN period (%) for nth opening in JUMP'
		lentext=nblank(text)
		j1=0
		do i=1,nthmax
		   data(i,1)=sx0(i,1)
		   data(i,2)=pm0(i,1)
		   data(i,3)=sdp(i,1)
		enddo
		call ASCWRITE(data,ndata,ncol,text,ndim1,ndim2,lentext,
     &		icolwid,colhead,1)
		DEALLOCATE(data)
	   endif
	   DEALLOCATE(sx0,pm0,sdp)
	endif		!end of ascwrite for id=113
c
c Insert here the correlation stuff -first open|shut
c NB ygap not defined when amplitude constrained so omit bits that need ygap
	if(conam1) goto 207
c
	if(id.eq.115) then
	  nlag=5
	  print 5,nlag
5	  format(
     & '  Calculate for lags (lag=1 is adjacent) up to n [',i2,'] = ')
	  call INPUTi(nlag)
c=	  call DCASK('Print the values','N',ans)
	  print 511
511	  format(' Print the values [N] ? ')
	  ans='N'
	  call INPUTa(ans)
	  noprint=UC(ans).eq.'N'
	  yhi(10)=-1.	!to indicate not set yet
	  nval=0	!for default
c	  print 66
c66	  format(
c     & ' Number of shut duration ranges to be specified (up to 10)',/,
c     & ' [default=7 ranges: tres-0.3, 0.3-1, 1-3, 3-30, 30-300,'
c     & ' 300-3000, >3000] = ')
c	  call INPUTi(nval)
	  if(nval.eq.0) then
c		nval=7
c		ylo(1)=treso
c		yhi(1)=0.3
c		ylo(2)=0.3
c		yhi(2)=1.0
c		ylo(3)=1.0
c		yhi(3)=3.
c		ylo(4)=3.
c		yhi(4)=30.
c		ylo(5)=30.
c		yhi(5)=300.
c		ylo(6)=300.
c		yhi(6)=3000.
c		ylo(7)=3000.
c		yhi(7)=1000000.
c		yhi(10)=3000.	!so default used
c Another version -divide the shut times more or less equally between
c ranges (ignore exclusions for this calc!)
c NB Must keep original order in YVAL, so copy to Yval1 to sort
		nval=7
		npr=ifixr(float(nyval)/7.)	!number per range
		ALLOCATE(yval1(ndimy))
		do i=1,nyval
		   yval1(i)=ygap(i)
		enddo
		call SORTr(yval1,nyval,ndimy,.true.)
		ylo(1)=tres
		yhi(1)=yval1(npr)
		ylo(2)=yhi(1)
		yhi(2)=yval1(2*npr)
		ylo(3)=yhi(2)
		yhi(3)=yval1(3*npr)
		ylo(4)=yhi(3)
		yhi(4)=yval1(4*npr)
		ylo(5)=yhi(4)
		yhi(5)=yval1(5*npr)
		ylo(6)=yhi(5)
		yhi(6)=yval1(6*npr)
		ylo(7)=yhi(6)
		yhi(7)=yval1(nyval)
		yhi(10)=10000.	!so default used
		DEALLOCATE(yval1)
	  endif
	  print 303,nval,(ylo(i),yhi(i),i=1,nval)
303	  format(' The ',i2,' default ranges (ms) are: ',/,
     &  3(f9.5,'-',f9.5,', '),/,3(f9.5,'-',f9.5,', '),/,
     &  3(f9.5,'-',f9.5,', '))
	  print 3032,(yloop(i),yhiop(i),i=1,nvalop)
3032	  format(' The ranges used last time (ms) were: ',/,
     &  3(f9.5,'-',f9.5,', '),/,3(f9.5,'-',f9.5,', '),/,
     &  3(f9.5,'-',f9.5,', '))
	  iropt=2
	  print 3033,iropt
3033	  format(
     & ' (1) Use default ranges ',/,
     & ' (2) Use same ranges as before',/,
     & ' (3) Define new ranges',/,
     & '  Option number [',i2,'] = ')
	  call INPUTi(iropt)
	  if(iropt.eq.1) then
	     nval=7
	  else if(iropt.eq.2) then
		nval=nvalgap
		do i=1,nval
		   ylo(i)=ylogap(i)
		   yhi(i)=yhigap(i)
		enddo
	  endif
	  if(iropt.eq.3) then
	     print 3035
3035	     format(
     & ' Number of shut duration ranges to be specified (up to 10) = ')
	     call INPUTi(nval)
	  endif
c
	  print 371
	  if(pon()) write(7,371)
	  if(discprt) write(8,371)
371	  format(/,
     & ' Mean durations of apparent open periods that are adjacent ',/,
     & '  to a shut periods with durations in following ranges (ms)')
c
	  do i=1,nval
		 sy0(i)=0.		!initialise
		 syy0(i)=0.		!initialise
		 ny0(i)=0		!initialise
		 sy1(i)=0.		!initialise
		 syy1(i)=0.		!initialise
		 ny1(i)=0		!initialise
		if(iropt.eq.3) then
		   print 67,i,i
67		   format(1x,' (',i2,
     & ') Shut times (ms) for range',i2,': low time, high time= ')
		   call INPUT2r(ylo(i),yhi(i))
		   if(ylo(i).gt.yhi(i)) then
		      a1=ylo(i)
		      ylo(i)=yhi(i)
		      yhi(i)=a1
		   endif
		endif
	  enddo
c end of setting of shut time ranges
c Keep the ranges used for conditional means (because if we want to
c average over experiments, best if same ranges used in all experiments
c that are averaged)
	  nvalgap=nval
	  do i=1,nval
		ylogap(i)=ylo(i)
		yhigap(i)=yhi(i)
c	 	print 703,i,ylo(i),yhi(i)
	 	if(prt) write (7,703) i,ylo(i),yhi(i)
	 	if(discprt) write (8,703) i,ylo(i),yhi(i)
703		format(' Shut time range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms')
	  enddo
c Allocate arrays
c	  n1=nval   	!for brevity
c	  n2=nlag
c reverse order for declaration of ns0 etc (better for acsii write)
	  n1=nlag   	!for brevity
	  n2=nval
	  if(allocated(ns0)) then
		DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,sx0,sdp)
	  endif
	  ALLOCATE(ns0(n1,n2),ym0(n1,n2),sd0(n1,n2),pm0(n1,n2),
     &	sx0(n1,n2),sdp(n1,n2))
	  ALLOCATE(ns1(n1,n2),ym1(n1,n2),sd1(n1,n2),pm1(n1,n2))
c For each gap, (a) find which shut time range it is in, (b) identify
c preceding and following open time (or open time which is n=lag away)
c and average these open times in the average appropriate to the shut
c time range found)
c If first latency excluded, then 1st open period in a jump has no
c preceding gap.  And last open period in a jump has no following
c gap when truncated last shut time excluded.
	  sum0=0.0
	  ss0=0.0
	  na0=0
	  sum1=0.0
	  na1=0
	  lag=1
	  do while(lag.le.nlag)
	   do k=1,nval
		sx0(lag,k)=0.0
	   enddo
c	   do i=1+lag,nyval-lag
	   i=1+lag		!initial value
	   do while(i.le.nyval-lag)
		if(mod(i,25).eq.0) debon=debug()
		tin=ygap(i)
		yv0=yval(i-lag+1)		!preceding open period
		yv1=yval(i+lag)	!following open period
c Need to check that whether (a) preceding, (b) following are part
c of the same jump record as yval(i) -do not use them if not
		jnum=indexg(i)		!jump # in which ygap(i) occurs
		jnum0=index(i-lag+1)	!jump # in which yval(i-lag-1) occurs
		jnum1=index(i+lag)	!jump # in which yval(i+lag) occurs
		OK0=jnum0.eq.jnum		!preceding op period OK
		OK1=jnum1.eq.jnum		!following op period OK
		if(OK0.and.OK1) then
		   sum0=sum0+yv0+yv1	!for mean of all 'adjacent' values used
		   ss0=ss0+(yv0*yv0)+(yv1*yv1)
		   na0=na0+2
		endif
		if(OK1) then
		   sum1=sum1+yv1		!for mean of all 'following' values used
		   na1=na1+1
		endif
C MODIF 10/30/95 09:11am (as for last night's modif of open/open)
		do k=1,nval
		   if(tin.ge.ylo(k).and.tin.lt.yhi(k)) then	!OK- in range
			if(OK0.and.OK1) then
			  sy0(k)=sy0(k) + yv0 + yv1 	!add PREC and FOLLOWING open period lengths
			  syy0(k)=syy0(k) + yv0*yv0 + yv1*yv1  ! for SD
			  ny0(k)=ny0(k)+2			!increment number
			  sx0(lag,k)=sx0(lag,k) + tin	!mean of adj gaps used
			endif
			if(OK1) then
			  sy1(k)=sy1(k) + yv1 	!add FOLLOWING open period lengths only
			  syy1(k)=syy1(k) + yv1*yv1		! for SD
			  ny1(k)=ny1(k)+1		!increment number
c===			  sy1(k)=sy1(k)+yv0			!ditto for 'preceding gap' only
c===			  syy1(k)=syy1(k)+yv0*yv0		! for SD
			endif
			if(debon) then
			   print 120,lag,i,ygap(i),k,i-lag+1,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
			   if(prt) write(7,120) lag,i,ygap(i),k,i-lag+1,
     &			yv0,i+lag,yv1,ny0(k),ny1(k)
			   if(discprt) write(8,120) lag,i,ygap(i),k,i-lag+1,
     &			yv0,i+lag,yv1,ny0(k),ny1(k)
120			   format(
     &			' LAG = ',i4,/,
     &			' Shut time #',i5,' = ',g13.6,
     &		   ' ms is in range ',i2,/,
     &		   '    Preceding open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   ':   Following open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   1x,i5,1x,i5,' values in averages 0 and 1')
			endif
		   endif
		enddo
1		continue
		i=i+1
	   enddo	!end of i=1,nyval loop (do while(i.le.nyval-lag))
c     Print means
	   ss0=(ss0-(sum0*sum0/float(na0)))/float(na0-1)	!variance
	   ss0=sqrt(ss0/float(na0))		!=SD(mean) for overall mean
	   sum0=sum0/float(na0)
	   sum1=sum1/float(na1)
	   if(noprint.and.lag.gt.1) goto 704
	   print 68,lag
	   if(prt) write(7,68) lag
	   if(discprt) write(8,68) lag
68	   format(/,
     & ' MEAN OPEN PERIODS FOR LAG = ',i3,' FROM SPECIFIED SHUT PERIOD')
	   print 681,sum0,na0,ss0,sum1,na1
	   if(prt) write(7,681) sum0,na0,ss0,sum1,na1
	   if(discprt) write(8,681) sum0,na0,ss0,sum1,na1
681	   format(/,
     &	' Mean of all (preceding+following) open periods = ',g13.6,
     &	' ms;',/,'    n = ',i5,' SD(mean) = ',g13.6,/,
     &	' Mean of following open periods only = ',g13.6,
     &	' ms; n = ',i5)
	   print 682		!col headings
	   if(prt) write(7,682)
	   if(discprt) write(8,682)
682	   format(
     &   8x,' Shut period range (ms) ',5x,'  n   mean open',
     & '    SD(mean)      mean as',/,
     &   8x,'                        ',5x,'       period  ',
     & '                  % of all')
704	   continue
	   do j=1,2		!for 'both' and 'following gap'
	    do i=1,nval
	      if(j.eq.1) then
		   n=ny0(i)
		   ns0(lag,i)=n		!record for below
		   if(n.gt.1) then
			en=float(n)
	      	ym=sy0(i)/en
			pm=100.*ym/sum0	!as % of all open periods
	      	sd=sqrt((syy0(i)-(sy0(i)*sy0(i)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym0(lag,i)=ym	!record for print as func of lag
			pm0(lag,i)=pm	!record for print as func of lag
			sd0(lag,i)=sd
			sx0(lag,i)=sx0(lag,i)/float(n/2)	!mean x value
c Approx SD of m=ym/sum0 if g<<1 and indep is
c (1/sum0)*sqrt[sd0**2 + (m*ss0)**2], where ss0=SD of sum0 and sd0 is sd of ym.
c*			sdp(lag,i)=100.*sd/sum0    !approx SD of pm0 (if denom exact)
			em=ym/sum0
			sdp(lag,i)=100.*sqrt((sd*sd) + (em*em*ss0*ss0))/sum0
	         endif
		else
		   if(i.eq.1.and.(.not.noprint)) then
			print 69
			if(prt) write(7,69)
			if(discprt) write(8,69)
69 			format(/,' For following open period only:')
		   endif
	         n=ny1(i)
		   ns1(lag,i)=n		!record for below
		   if(n.gt.1) then
	      	ym=sy1(i)/float(n)
			pm=100.*ym/sum1	!as % of all open periods
	      	sd=sqrt((syy1(i)-(sy1(i)*sy1(i)/
     &			float(n)))/float(n-1))
			ym1(lag,i)=ym	!record for print as func of lag
			pm1(lag,i)=pm	!record for print as func of lag
			sd1(lag,i)=sd
		   endif
		endif
		if(noprint) goto 705
	      if(n.le.1) then
		   print 4261,i,ylo(i),yhi(i),n
		   if(prt) write(7,4261) i,ylo(i),yhi(i),n
		   if(discprt) write(8,4261) i,ylo(i),yhi(i),n
4261		   format(i4,1x,g13.6,' to ',g13.6,5x,i4)
		else
		   print 426,i,ylo(i),yhi(i),n,ym,sd,pm
		   if(prt) write(7,426) i,ylo(i),yhi(i),n,ym,sd,pm
		   if(discprt) write(8,426) i,ylo(i),yhi(i),n,ym,sd,pm
426		   format(i4,1x,g13.6,' to ',g13.6,2x,i4,1x,3g13.6)
		endif
705		continue
	    enddo
	   enddo
c Print mean of adjacent gaps separately
	  if(.not.noprint) then
	   do k=1,nval
	      print 290,k,sx0(lag,k)
      	if(pon()) write(7,290) k,sx0(lag,k)
	      if(discprt) write(8,290) k,sx0(lag,k)
290		format(' For range #',i3,' mean shut time = ',g13.6)
	   enddo
	  endif
c
c Increment the lag
	   lag=lag+1
c	   pause 'lag incremented'
	   debon=debug()
	   sum0=0.0
	   ss0=0.0
	   na0=0
	   sum1=0.0
	   na1=0
	   do i=1,nval
		sy0(i)=0.		!initialise
		syy0(i)=0.		!initialise
		ny0(i)=0		!initialise
		sy1(i)=0.		!initialise
		syy1(i)=0.		!initialise
		ny1(i)=0		!initialise
	   enddo
	  enddo	!end of 'do while(lag.le.nlag)'
	  print 6
	  if(prt) write(7,6)
	  if(discprt) write(8,6)
6	  format(/,/,' SAME VALUES TABULATED AS FUNCTION OF LAG')
	  do i=1,nval
	 	print 71,i,ylo(i),yhi(i)
	 	if(prt) write (7,71) i,ylo(i),yhi(i)
	 	if(discprt) write (8,71) i,ylo(i),yhi(i)
71		format(/,' Shut period range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms',/,
     &	'   lag',8x,'  n     mean open period     Std dev',
     &	'     mean as % of all ')
		do lag=1,nlag
		   n=ns0(lag,i)
		   pm=pm0(lag,i)		!as % of all open periods
		   if(noprint) goto 716
	         if(n.le.1) then
			print 8,lag,ns0(lag,i)
			if(prt) write(7,8) lag,ns0(lag,i)
			if(discprt) write(8,8) lag,ns0(lag,i)
8			format(i5,8x,i5)
		   else
			print 9,lag,ns0(lag,i),ym0(lag,i),sd0(lag,i),pm
			if(prt) write(7,9) lag,ns0(lag,i),ym0(lag,i),
     &				sd0(lag,i),pm
			if(discprt) write(8,9) lag,ns0(lag,i),
     &			ym0(lag,i),sd0(lag,i),pm
9			format(i5,8x,i5,5x,3(3x,g13.6))
		   endif
716		   continue
		enddo
		print 72
		if(prt) write(7,72)
		if(discprt) write(8,72)
72 		format(/,' For following open period only:')
		do lag=1,nlag
		   n=ns1(lag,i)
		   pm=pm1(lag,i)		!as % of all open periods
		   if(noprint) goto 717
	         if(n.le.1) then
			print 8,lag,ns1(lag,i)
			if(prt) write(7,8) lag,ns1(lag,i)
			if(discprt) write(8,8) lag,ns1(lag,i)
		   else
			print 9,lag,ns1(lag,i),ym1(lag,i),sd1(lag,i),pm
			if(prt) write(7,9) lag,ns1(lag,i),ym1(lag,i),
     &				sd1(lag,i),pm
			if(discprt) write(8,9) lag,ns1(lag,i),
     &			ym1(lag,i),sd1(lag,i),pm
		   endif
717		   continue
		enddo
	  enddo
c
	   ans='N'
	   call DCASK('Save values as ASCII file',ans,ans)
	   if(ans.eq.'Y') then
c Want alternating columns of pm0,sx0 both of which are declared (nlag,nval)
c ie have nval columns
		ALLOCATE(data(nlag,3*nval))
		ndata=nlag	!number of lines of data
		ncol=3*nval	!nval entries in each line
		icolwid=13		!the minimum col width
		do i=1,ncol,3
		   call INTCONV(1+i/3,cnum)
		   colhead(i)='X (range '//charnb(cnum)//')'
		   colhead(i+1)='Y (range '//charnb(cnum)//')'
		   colhead(i+2)='SD(mean)('//charnb(cnum)//')'
		enddo
		ndim1=nlag
		ndim2=3*nval
		text=
     &	'Mean OPEN period (%), adj. to SHUT time range in JUMP'
		lentext=nblank(text)
		j1=0
		do j=1,nval
		   j1=j1+3
		   do i=1,nlag
			data(i,j1-2)=sx0(i,j)
			data(i,j1-1)=pm0(i,j)
			data(i,j1)=sdp(i,j)
		   enddo
		enddo
		call ASCWRITE(data,ndata,ncol,text,ndim1,ndim2,lentext,
     &		icolwid,colhead,1)
		DEALLOCATE(data)
	   endif
c
	  DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1)
	  print 108
	  if(pon()) write(7,108)
	  if(discprt) write(8,108)
108	  format(/)
c Now do open/shut correlations
	  call CORCALJ(yval,ygap,index,indexg,nsweep,nyval,ndimy,nint,
     &	idiskq,1)
	endif 	!end of calc of mean open adjacent . . .(id=115)
c
c Now open-open means (id=116)
	if(id.eq.116) then
	  nlag=5
	  print 5,nlag
c5	  format(
c     & '  Calculate for lags (lag=1 is adjacent) up to n [',i2,'] = ')
	  call INPUTi(nlag)
	  call DCASK('Print the values','N',ans)
	  noprint=ans.eq.'N'
	  yhi(10)=-1.	!to indicate not set yet
	  nval=0	!for default
c	  print 303
c303	  format(
c     & ' Number of open duration ranges to be specified (up to 10)',/,
c     & ' [default=6 ranges: tres-0.3, 0.3-1, 1-3, 3-10, 10-30,'
c     & ' >30] = ')
c	  call INPUTi(nval)
	  if(nval.eq.0) then
c		nval=6
c		ylo(1)=treso
c		yhi(1)=0.3
c		ylo(2)=0.3
c		yhi(2)=1.0
c		ylo(3)=1.0
c		yhi(3)=3.
c		ylo(4)=3.
c		yhi(4)=10.
c		ylo(5)=10.
c		yhi(5)=30.
c		ylo(6)=30.
c		yhi(6)=10000.
c		yhi(10)=10000.	!so default used
c A third version -divide the openings more or less equally between
c ranges (ignore exclusions for this calc!)
c NB Must keep original order in YVAL, so copy to Yval1 to sort
		nval=6
		npr=ifixr(float(nyval)/6.)	!number per range
		ALLOCATE(yval1(ndimy))
		do i=1,nyval
		   yval1(i)=yval(i)
		enddo
		call SORTr(yval1,nyval,ndimy,.true.)
		ylo(1)=tres
		yhi(1)=yval1(npr)
		ylo(2)=yhi(1)
		yhi(2)=yval1(2*npr)
		ylo(3)=yhi(2)
		yhi(3)=yval1(3*npr)
		ylo(4)=yhi(3)
		yhi(4)=yval1(4*npr)
		ylo(5)=yhi(4)
		yhi(5)=yval1(5*npr)
		ylo(6)=yhi(5)
		yhi(6)=yval1(nyval)
		yhi(10)=10000.	!so default used
		DEALLOCATE(yval1)
	  endif
	  print 303,nval,(ylo(i),yhi(i),i=1,nval)
c303	  format(' The ',i2,' default ranges (ms) are: ',/,
c     &  3(f9.5,'-',f9.5,', '),/,3(f9.5,'-',f9.5,', '))
	  print 3032,(yloop(i),yhiop(i),i=1,nvalop)
c3032	  format(' The ranges used last time (ms) were: ',/,
c     &  3(f9.5,'-',f9.5,', '),/,3(f9.5,'-',f9.5,', '),/,
c     &  3(f9.5,'-',f9.5,', '))
	  iropt=2
	  print 3033,iropt
c3033	  format(
c     & ' (1) Use default ranges ',/,
c     & ' (2) Use same ranges as before',/,
c     & ' (3) Define new ranges',/,
c     & '  Option number [',i2,'] = ')
	  call INPUTi(iropt)
	  if(iropt.eq.1) then
	     nval=6
	  else if(iropt.eq.2) then
		nval=nvalop
		do i=1,nval
		   ylo(i)=yloop(i)
		   yhi(i)=yhiop(i)
		enddo
	  endif
	  if(iropt.eq.3) then
	     print 3031
3031	     format(
     & ' Number of open duration ranges to be specified (up to 10) = ')
	     call INPUTi(nval)
	  endif
	  print 37
	  if(pon()) write(7,37)
	  if(discprt) write(8,37)
37	  format(/,/,
     & ' Mean durations of apparent open periods that are adjacent ',/,
     & '  to a open periods with durations in following ranges (ms)')
c
	  do i=1,nval
		 sy0(i)=0.		!initialise
		 syy0(i)=0.		!initialise
		 ny0(i)=0		!initialise
		 sy1(i)=0.		!initialise
		 syy1(i)=0.		!initialise
		 ny1(i)=0		!initialise
		if(iropt.eq.3) then
		   print 312,i,i
312		   format(1x,' (',i2,
     & ') Open times (ms) for range',i2,': low time, high time= ')
		   call INPUT2r(ylo(i),yhi(i))
		   if(ylo(i).gt.yhi(i)) then
		      a1=ylo(i)
		      ylo(i)=yhi(i)
		      yhi(i)=a1
		   endif
		endif
	  enddo
c end of setting of open time ranges
c Keep the ranges used for conditional means (because if we want to
c average over experiments, best if same ranges used in all experiments
c that are averaged)
	  nvalop=nval
	  do i=1,nval
		yloop(i)=ylo(i)
		yhiop(i)=yhi(i)
c	 	print 721,i,ylo(i),yhi(i)
	 	if(prt) write (7,721) i,ylo(i),yhi(i)
	 	if(discprt) write (8,721) i,ylo(i),yhi(i)
721		format(' Open period range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms')
	  enddo
c Allocate arrays
c	  n1=nval   	!for brevity
c	  n2=nlag
c reverse order for declaration of ns0 etc (better for acsii write)
	  n1=nlag   	!for brevity
	  n2=nval
	  if(allocated(ns0)) then
		DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,sx0,sdp)
	  endif
	  ALLOCATE(ns0(n1,n2),ym0(n1,n2),sd0(n1,n2),pm0(n1,n2),
     &	sx0(n1,n2),sdp(n1,n2))
	  ALLOCATE(ns1(n1,n2),ym1(n1,n2),sd1(n1,n2),pm1(n1,n2))
	  sum0=0.0
	  ss0=0.0
	  na0=0
	  sum1=0.0
	  na1=0
	  lag=1
	  do while(lag.le.nlag)
	   do k=1,nval
		sx0(lag,k)=0.0
	   enddo
	   do i=1+lag,nyval-lag
		if(mod(i,20).eq.0) debon=debug()
		tin=yval(i)
		yv0=yval(i-lag)		!preceding open period
		yv1=yval(i+lag)	!following open period
c Need to check that whether (a) preceding, (b) following are part
c of the same jump record as yval(i) -do not use them if not
		jnum=index(i)		!jump # in which yval(i) occurs
		jnum0=index(i-lag)	!jump # in which yval(i-lag) occurs
		jnum1=index(i+lag)	!jump # in which yval(i+lag) occurs
		OK0=jnum0.eq.jnum		!preceding op period OK
		OK1=jnum1.eq.jnum		!following op period OK
c
		if(OK0.and.OK1) then
		   sum0=sum0+yv0+yv1	!for mean of all 'adjacent' values used
		   ss0=ss0+(yv0*yv0)+(yv1*yv1)
		   na0=na0+2
		endif
		if(OK1) then
		   sum1=sum1+yv1		!for mean of all 'following' values used
		   na1=na1+1
		endif
		do k=1,nval
		   if(tin.ge.ylo(k).and.tin.lt.yhi(k)) then	!OK- in range
			if(OK0.and.OK1) then
			  sy0(k)=sy0(k) + yv0 + yv1 	!add PREC and FOLLOWING open period lengths
			  syy0(k)=syy0(k) + yv0*yv0 + yv1*yv1  ! for SD
			  ny0(k)=ny0(k)+2			!increment number
			  sx0(lag,k)=sx0(lag,k) + tin	!mean of adj open used
			endif
			if(OK1) then
			  sy1(k)=sy1(k) + yv1 	!add FOLLOWING open period lengths only
			  syy1(k)=syy1(k) + yv1*yv1		! for SD
			  ny1(k)=ny1(k)+1		!increment number
c===			  sy1(k)=sy1(k)+yv0			!ditto for 'preceding gap' only
c===			  syy1(k)=syy1(k)+yv0*yv0		! for SD
			endif
			if(debon) then
			   print 123,lag,i,yval(i),k,i-lag,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
			   if(prt) write(7,123) lag,i,yval(i),k,i-lag,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
			   if(discprt) write(8,123) lag,i,yval(i),k,i-lag,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
123			   format(
     &			' LAG = ',i4,/,
     &			' Open period #',i5,' = ',g13.6,
     &		   ' ms is in range ',i2,/,
     &		   '    Preceding open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   ':   Following open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   1x,i5,1x,i5,' values in averages 0 and 1')
			endif
		   endif
		enddo
	   enddo	!end of i=1,nyval loop
c     Print means
	   ss0=(ss0-(sum0*sum0/float(na0)))/float(na0-1)	!variance
	   ss0=sqrt(ss0/float(na0))		!=SD(mean) for overall mean
	   sum0=sum0/float(na0)
	   sum1=sum1/float(na1)
	   if(noprint.and.lag.gt.1) goto 714
	   print 125,lag
	   if(prt) write(7,125) lag
	   if(discprt) write(8,125) lag
125	   format(/,
     & ' MEAN OPEN PERIODS FOR LAG = ',i3,' FROM SPECIFIED OPEN PERIOD')
	   print 681,sum0,na0,ss0,sum1,na1
	   if(prt) write(7,681) sum0,na0,ss0,sum1,na1
	   if(discprt) write(8,681) sum0,na0,ss0,sum1,na1
c681	   format(/,
c     &	' Mean of all (preceding+following) open periods = ',g13.6,
c     &	' ms;',/,'    n = ',i5,' SD(mean) = ',g13.6,/,
c     &	' Mean of following open periods only = ',g13.6,
c     &	' ms; n = ',i5)
	   print 683		!col headings
	   if(prt) write(7,683)
	   if(discprt) write(8,683)
683	   format(
     &   8x,' Open period range (ms) ',5x,'  n   mean open',
     & '    SD(mean )     mean as',/,
     &   8x,'                        ',5x,'       period  ',
     & '                  % of all')
714	   continue
	   do j=1,2		!for 'both' and 'following gap'
	    do i=1,nval
	      if(j.eq.1) then
		   n=ny0(i)
		   ns0(lag,i)=n		!record for below
		   if(n.gt.1) then
			en=float(n)
	      	ym=sy0(i)/en
			pm=100.*ym/sum0	!as % of all open periods
	      	sd=sqrt((syy0(i)-(sy0(i)*sy0(i)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym0(lag,i)=ym	!record for print as func of lag
			pm0(lag,i)=pm	!record for print as func of lag
			sd0(lag,i)=sd
			sx0(lag,i)=sx0(lag,i)/float(n/2)	!mean x value
c Approx SD of m=ym/sum0 if g<<1 and indep is
c (1/sum0)*sqrt[sd0**2 + (m*ss0)**2], where ss0=SD of sum0 and sd0 is sd of ym.
c*			sdp(lag,i)=100.*sd/sum0    !approx SD of pm0 (if denom exact)
			em=ym/sum0
			sdp(lag,i)=100.*sqrt((sd*sd) + (em*em*ss0*ss0))/sum0
	         endif
		else
		   if(i.eq.1.and.(.not.noprint)) then
			print 69
			if(prt) write(7,69)
			if(discprt) write(8,69)
c69 			format(/,' For following open period only:')
		   endif
	         n=ny1(i)
		   ns1(lag,i)=n		!record for below
		   if(n.gt.1) then
	      	ym=sy1(i)/float(n)
			pm=100.*ym/sum1	!as % of all open periods
	      	sd=sqrt((syy1(i)-(sy1(i)*sy1(i)/
     &			float(n)))/float(n-1))
			ym1(lag,i)=ym	!record for print as func of lag
			pm1(lag,i)=pm	!record for print as func of lag
			sd1(lag,i)=sd
		   endif
		endif
		if(noprint) goto 707
	      if(n.le.1) then
		   print 4261,i,ylo(i),yhi(i),n
		   if(prt) write(7,4261) i,ylo(i),yhi(i),n
		   if(discprt) write(8,4261) i,ylo(i),yhi(i),n
c4261		   format(i4,3x,g13.6,' to ',g13.6,5x,i4)
		else
		   print 426,i,ylo(i),yhi(i),n,ym,sd,pm
		   if(prt) write(7,426) i,ylo(i),yhi(i),n,ym,sd,pm
		   if(discprt) write(8,426) i,ylo(i),yhi(i),n,ym,sd,pm
c426		   format(i4,3x,g13.6,' to ',g13.6,5x,i4,2(3x,g13.6))
		endif
707		continue
	    enddo
	   enddo
c Print mean of adjacent ops separately
	   if(.not.noprint) then
	    do k=1,nval
	      print 291,k,sx0(lag,k)
      	if(pon()) write(7,291) k,sx0(lag,k)
	      if(discprt) write(8,291) k,sx0(lag,k)
291		format(' For range #',i3,' mean open period = ',g13.6)
	    enddo
	   endif
c
c Increment the lag
	   lag=lag+1
c	   pause 'lag incremented'
	   debon=debug()
	   sum0=0.0
	   ss0=0.0
	   na0=0
	   sum1=0.0
	   na1=0
	   do i=1,nval
		sy0(i)=0.		!initialise
		syy0(i)=0.		!initialise
		ny0(i)=0		!initialise
		sy1(i)=0.		!initialise
		syy1(i)=0.		!initialise
		ny1(i)=0		!initialise
	   enddo
	  enddo	!end of 'do while(lag.le.nlag)'
	  print 6
	  if(prt) write(7,6)
	  if(discprt) write(8,6)
c6	  format(/,/,' SAME VALUES TABULATED AS FUNCTION OF LAG')
	  do i=1,nval
	 	print 7,i,ylo(i),yhi(i)
	 	if(prt) write (7,7) i,ylo(i),yhi(i)
	 	if(discprt) write (8,7) i,ylo(i),yhi(i)
7		format(/,' Open period range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms',/,
     &	'   lag',8x,'  n     mean open period     Std dev',
     &	'     mean as % of all ')
		do lag=1,nlag
		   n=ns0(lag,i)
		   pm=pm0(lag,i)		!as % of all open periods
		   if(noprint) goto 708
	         if(n.le.1) then
			print 8,lag,ns0(lag,i)
			if(prt) write(7,8) lag,ns0(lag,i)
			if(discprt) write(8,8) lag,ns0(lag,i)
c8			format(i5,8x,i5)
		   else
			print 9,lag,ns0(lag,i),ym0(lag,i),sd0(lag,i),pm
			if(prt) write(7,9) lag,ns0(lag,i),ym0(lag,i),
     &				sd0(lag,i),pm
			if(discprt) write(8,9) lag,ns0(lag,i),
     &			ym0(lag,i),sd0(lag,i),pm
c9			format(i5,8x,i5,5x,3(3x,g13.6))
		   endif
708		   continue
		enddo
		print 69
		if(prt) write(7,69)
		if(discprt) write(8,69)
c69 		format(/,' For following open period only:')
		do lag=1,nlag
		   n=ns1(lag,i)
		   pm=pm1(lag,i)		!as % of all open periods
		   if(noprint) goto 709
	         if(n.le.1) then
			print 8,lag,ns1(lag,i)
			if(prt) write(7,8) lag,ns1(lag,i)
			if(discprt) write(8,8) lag,ns1(lag,i)
		   else
			print 9,lag,ns1(lag,i),ym1(lag,i),sd1(lag,i),pm
			if(prt) write(7,9) lag,ns1(lag,i),ym1(lag,i),
     &				sd1(lag,i),pm
			if(discprt) write(8,9) lag,ns1(lag,i),
     &			ym1(lag,i),sd1(lag,i),pm
		   endif
709		   continue
		enddo
	  enddo
c
	   ans='N'
	   call DCASK('Save values as ASCII file',ans,ans)
	   if(ans.eq.'Y') then
c Want alternating columns of pm0,sx0 both of which are declared (nlag,nval)
c ie have nval columns
		ALLOCATE(data(nlag,3*nval))
		ndata=nlag	!number of lines of data
		ncol=3*nval	!nval entries in each line
		icolwid=13		!the minimum col width
		do i=1,ncol,3
		   call INTCONV(1+i/3,cnum)
		   colhead(i)='X (range '//charnb(cnum)//')'
		   colhead(i+1)='Y (range '//charnb(cnum)//')'
		   colhead(i+2)='SD(mean)('//charnb(cnum)//')'
		enddo
		ndim1=nlag
		ndim2=3*nval
		text=
     &	'Mean OPEN period (%), adj. to OPEN time range in JUMP'
		lentext=nblank(text)
		j1=0
		do j=1,nval
		   j1=j1+3
		   do i=1,nlag
			data(i,j1-2)=sx0(i,j)
			data(i,j1-1)=pm0(i,j)
			data(i,j1)=sdp(i,j)
		   enddo
		enddo
		call ASCWRITE(data,ndata,ncol,text,ndim1,ndim2,lentext,
     &		icolwid,colhead,1)
		DEALLOCATE(data)
	   endif
c
	  DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,sx0,sdp)
	  print 108
	  if(pon()) write(7,108)
	  if(discprt) write(8,108)
c108	  format(/)
c Now do open/open correlations
	  call CORCALJ(yval,ygap,index,indexg,nsweep,nyval,ndimy,nint,
     &	idiskq,2)
	endif 	!end of calc of mean open adjacent . . .(id=116)
c
	DEALLOCATE(ygap,index,indexg)
c
c
	if(id.eq.113.or.id.eq.115.or.id.eq.116) then
	   goto 999	!means only -no graph
	else
	   goto 207	!Yval() now defined
	endif
c
c
c TOTAL ACTIVATION OPTIONS
40	continue
	BHIST=.true.
	tres=treso
	print 41
41	format(
     & ' (1) Time from t=0 to end of last opening',/,
     & ' (2) Time from start of first opening to end of last',/,
     & ' (3) Total open time per jump',/,
     & ' (4) P(open) within activation (exc latency)',/,
     & ' (5) P(open) within activation (inc latency)',/,
     & ' Option number = ')
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.5) goto 40
	ID=120+iopt       !id=121,122,123,124,125
	thist=.true.
	bhist=.true.
	if(id.eq.121) then
         print 42
         if(pon()) write(7,42)
         if(discprt) write(8,42)
42	   format(/,' Time from t=0 to end of last opening')
	else if(id.eq.122) then
         print 43
         if(pon()) write(7,43)
         if(discprt) write(8,43)
43	   format(/,' Time from start of first opening to end of last')
	else if(id.eq.123) then
         print 59
         if(pon()) write(7,59)
         if(discprt) write(8,59)
59	   format(/,' Total open time per jump')
	else if(id.eq.124) then
	   thist=.false.
	   bhist=.false.
         print 591
         if(pon()) write(7,591)
         if(discprt) write(8,591)
591	   format(/,' P(open) within activation (exc latency)')
	else if(id.eq.125) then
	   thist=.false.
	   bhist=.false.
         print 592
         if(pon()) write(7,592)
         if(discprt) write(8,592)
592	   format(/,' P(open) within activation (inc latency)')
	endif
      print 27
      if(pon()) write(7,27)
      if(discprt) write(8,27)
c27	format(' ======================================',/)
c
	print 306
      if(pon()) write(7,306)
	if(discprt) write(8,306)
c306	format(
c     &	' Null sweeps are omitted',/)
c
	exczero=.false.
	exczero1=.false.
	if(id.ge.122.and.id.le.125) then
	   ans='Y'
	   if(id.eq.123) ans='N'
	   call DCASK(
     &	'Exclude sweeps that are open at t=0',ans,ans)
	   if(ans.eq.'Y') exczero1=.true.
	   if(id.eq.124.or.id.eq.125) then
		exczero=.true.
	   else if(.not.exczero1) then
	      call DCASK(
     &	'Exclude the initial open time when open at t=0','N',ans)
	      if(ans.eq.'Y') exczero=.true.
	   endif
	   if(exczero1) then
      	if(pon()) write(7,4052)
	      if(discprt) write(8,4052)
4052		format(
     &' Sweeps that are open at t=0 are omitted',/)
	   else
      	if(pon()) write(7,4053)
	      if(discprt) write(8,4053)
4053		format(
     &' Sweeps that are open at t=0 are included',/)
		if(exczero) then
      	   if(pon()) write(7,451)
	         if(discprt) write(8,451)
451		   format(
     &' Openings that are open at t=0 are omitted',/)
		else
      	   if(pon()) write(7,4051)
	         if(discprt) write(8,4051)
4051		   format(
     &' Openings that are open at t=0 are included',/)
		endif
	   endif
	endif
c
	exclast=.false.
	exclast1=.false.
	if(id.ge.121.and.id.le.125) then
	   ans='Y'
	   if(id.eq.123) ans='N'
	   print 46
46	   format(
     &' If the channel is open at the end of the sweep, should this',/,
     &'    (truncated) value be excluded ?')
	   call DCASK('  Exclude last value',ans,ans)
	   if(ans.eq.'Y') then
		exclast=.true.
		ans='N'
		if(id.eq.124.or.id.eq.125) ans='Y'
		call DCASK(
     &	' -should the entire sweep be excluded too',ans,ans)
		if(ans.eq.'Y') then
		   exclast1=.true.
		else
		   exclast1=.false.
		endif
	   endif
	   if(exclast) then
		if(exclast1) then
      	   if(pon()) write(7,461)
	         if(discprt) write(8,461)
461		   format(
     &' All sweeps in which channel is open at the end are omitted',/)
		else
      	   if(pon()) write(7,463)
	         if(discprt) write(8,463)
463		   format(
     &' Open times at the end of a sweep (truncated) are omitted')
		endif
	   else
      	if(pon()) write(7,462)
	      if(discprt) write(8,462)
462		format(
     &' Open times at the end of a sweep (truncated) are included',/,
     &'  (and used for fitting)',/)
	   endif
	endif
c
c nyval=number of jumps, at most
	debon=debug()
	j=0 		!to count good bursts
	nfitt=0	!to count truncated values (open at end)
c=	nbad=0	!to count number of sweeps with no nth gap
c
	do 400 ij=1,nsweep
c
	if(mod(ij-1,4).eq.0) then	!check if debug on every 4th jump
	   debon=debug()
	endif
	if(debon) then
	   print 505,jumps(ij)
         if(discprt) write(8,505) jumps(ij)
	endif
	if(nomit.gt.0) then
	   do i=1,nomit
		if(jumpomit(i).eq.ij) goto 400   !omit this sweep altogether
	   enddo
	endif
c
	ifirst=kjumps(ij)	!index in tint() of 1st interval in jth jump
	if(ij.lt.nsweep) then
	   ilast=kjumps(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nint
	endif
c
	if(ifirst.eq.ilast) then
	   null=BTEST(iprops(ifirst),5)
	else
	   null=BTEST(iprops(ifirst),5).and.BTEST(iprops(ifirst+1),5)
	endif
	if(null) then
	   if(debon) then
		k=ifirst
		print 503,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,503) k,tint(k),ampl(k),iprops(k)
c503		format(i6,g12.5,g12.5,i5,' (null sweep)')
	      print 108
      	if(discprt) write(8,108)
	   endif
	   goto 400		!no bursts in nulls
	endif
c
	zero=BTEST(iprops(ifirst),5).and.ampl(ifirst).ne.0
	lastop=BTEST(iprops(ilast),6).and.ampl(ilast).ne.0
c
c=	ntrans=ilast-ifirst	!number of transitions in current sweep
c Find index of 1st and last openings in this sweep
	do k=ifirst,ilast
	   if(ampl(k).ne.0.) goto 47
	enddo
47	iop1=k
c
	do k=ilast,ifirst,-1
	   if(ampl(k).ne.0.) goto 48
	enddo
48	iopn=k
c
	if(zero.and.exczero1) then
	   if(debon) then
		k=iop1
		print 501,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
	      print 108
      	if(discprt) write(8,108)
	   endif
	   goto 400 !exclude sweeps open at start
	endif
	if(lastop.and.exclast1) then
	   if(debon) then
		k=iopn
		print 501,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
	      print 108
      	if(discprt) write(8,108)
	   endif
	   goto 400   !exclude sweeps open at end
	endif
c
c   add lengths of all intervals
	s=0.0
	s1=0.0
	bad=.false.
	if(id.eq.121.or.id.eq.125) then	!length from t=0
	   i1=ifirst
	   i2=iopn
	   if(zero.and.exczero) then	!make i1=index for 1st op after that at t=0
		do k=ifirst+1,ilast
		   if(ampl(k).ne.0) goto 49
		enddo
49		i1=k
	   endif
	   if(lastop.and.exclast) then !make i2=index for last op before that at end
		do k=ilast-1,ifirst,-1
		   if(ampl(k).ne.0) goto 491
		enddo
491		i2=k
	   endif
c
	   do k=i1,i2
		if(badint(k)) bad=.true.
		s=s + tint(k)
		if(ampl(k).ne.0) s1=s1 + tint(k)	!open time
		if(debon) then
		   print 501,k,tint(k),ampl(k),iprops(k)
      	   if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
		endif
	   enddo
c
	else if(id.eq.122.or.id.eq.124) then	!length from start of 1st opening
	   i1=iop1
	   i2=iopn
	   if(zero.and.exczero) then	!make i1=index for 1st op after that at t=0
		do k=iop1+1,ilast
		   if(ampl(k).ne.0) goto 492
		enddo
492		i1=k
	   endif
	   if(lastop.and.exclast) then !make i2=index for last op before that at end
		do k=ilast-1,ifirst,-1
		   if(ampl(k).ne.0) goto 493
		enddo
493		i2=k
	   endif
c
	   do k=i1,i2
		if(badint(k)) bad=.true.
		s=s + tint(k)	!total time
		if(ampl(k).ne.0) s1=s1 + tint(k)	!open time
		if(debon) then
		   print 501,k,tint(k),ampl(k),iprops(k)
      	   if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
		endif
	   enddo
	else if(id.eq.123) then		!total open time
	   do k=ifirst,ilast
		if(badint(k)) bad=.true.
		if(ampl(k).ne.0) s=s + tint(k)
		if(debon) then
		   print 501,k,tint(k),ampl(k),iprops(k)
      	   if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
		endif
	   enddo
	endif
c
	if(bad) goto 400		!don't use sweeps any bad interval
c
	if(lastop.and.(.not.exclast)) then
	   nfitt=nfitt+1
	   YVALt(nfitt)=s
	   if(debon) then
	      print 504,nfitt,yvalt(nfitt),jumps(ij)
            if(discprt) write(8,504) nfitt,yvalt(nfitt),jumps(ij)
	   endif
	else
	   j=j+1
	   YVAL(j)=s
	   if(id.eq.124.or.id.eq.125) yval(j)=s1/s	!P(open)
	   if(debon) then
		print 502,j,yval(j),jumps(ij)
      	if(discprt) write(8,502) j,yval(j),jumps(ij)
	   endif
	endif
c
400	continue	!end of ij=1,nsweep loop
	NYVAL=j     !no of usable gaps
	goto 207	!Yval() now defined
c
c
c BURSTS WITHIN JUMPS (idest=5)
c NB
c (1) null sweeps skipped, except that they count as a 'burst with 0 jumps'
c for option 4.
c (2) Intervals of undefined length (badint) case burst to be
c abandoned, and new burst started after next gap>tcrit found.
c (dubious amplitudes are ignored)

73	continue
	tres=treso
      if(pon()) write(7,84) tcrit
      if(discprt) write(8,84) tcrit
84	format(' For definition of bursts, tcrit = ',g13.6,' ms')
	print 74
74	format(
     & ' (1) Total burst length',/,
     & ' (2) Total open time per burst',/,
     & ' (3) Number of open periods per burst',/,
     & ' (4) Number of bursts per jump',/,
     & ' Option number = ')
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.4) goto 73
	ID=130+iopt       !id=131,132,133,134
	if(id.eq.131) then
         print 75
         if(pon()) write(7,75)
         if(discprt) write(8,75)
75	   format(/,' Total burst length')
	else if(id.eq.132) then
         print 76
         if(pon()) write(7,76)
         if(discprt) write(8,76)
76	   format(/,' Total open time per burst')
	else if(id.eq.133) then
         print 77
         if(pon()) write(7,77)
         if(discprt) write(8,77)
77	   format(/,' Number of open periods per burst')
	else if(id.eq.134) then
         print 78
         if(pon()) write(7,78)
         if(discprt) write(8,78)
78	   format(/,' Number of bursts per jump')
	endif
      print 27          !underline
      if(pon()) write(7,27)
      if(discprt) write(8,27)
c
	if(id.eq.131.or.id.eq.132) then
	   bhist=.true.
	   thist=.true.
	else if(id.eq.133.or.id.eq.134) then
	   obhist=.true.
	   thist=.false.
	endif
c
	endbad=.false.
	print 85
85	format(
     & ' Should a gap of undefined length be deemed to end burst ',
     & '(>tcrit) [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') endbad=.true.
c
	if(allocated(ygap)) DEALLOCATE(ygap)
	if(.not.conam1) then
	   ALLOCATE(ygap(nintt))
	endif
c
	exczero=.true.
	j=0 		!to count open periods
	j1=0		!to count number of bursts
	n1=0		!to count number of open periods per burst
	nbad1=0	!to count number of open bursts abandoned because
c			!of bad interval
	bad=.false.	!current burst contains no untervals of undefined length
c
	do 79 ij=1,nsweep
	n2=0		!to count number of bursts per sweep (jump)
c
	if(mod(ij-1,4).eq.0) then	!check if debug on every 4th jump
	   debon=debug()
	endif
	if(debon) then
	   print 505,jumps(ij)
         if(discprt) write(8,505) jumps(ij)
c505	   format(' Sweep # ',i4)
	endif
	if(nomit.gt.0) then
	   do i=1,nomit
		if(jumpomit(i).eq.ij) goto 79   !omit this sweep altogether
	   enddo
	endif
c
	ifirst=kjumps(ij)	!index in tint() of 1st interval in jth jump
	if(ij.lt.nsweep) then
	   ilast=kjumps(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nint
	endif
c
	if(ifirst.eq.ilast) then
	   null=BTEST(iprops(ifirst),5)
	else
	   null=BTEST(iprops(ifirst),5).and.BTEST(iprops(ifirst+1),5)
	endif
c
	if(null) then	!print if req and jump to next sweep
	   if(debon) then
		print 503,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,503) k,tint(k),ampl(k),iprops(k)
c503		format(i6,g12.5,g12.5,i5,' (null sweep)')
	   endif
	   if(id.eq.134) then
		Yval(ij)=0.		!0 bursts in a null jump
		if(debon) then
		   print 502,ij,yval(ij),jumps(ij)
	         if(discprt) write(8,502) ij,yval(ij),jumps(ij)
c502		   format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
		endif
	   endif
	   goto 79
	endif
c
c=/===not nec?
c	if(exclast) then
c	   a1=ampl(ilast)
c	   if(a1.ne.0) ilast=ilast-1	!omit last (truncated) opening
c	endif
c
c Find start of a group (open period) -valid start must have a good shut
c time followed by a good opening -if a bad opening is found as first (or
c any later) opening then the open period is abandoned altogether, and the
c next good shut time sought as start for next open period, but for the
c purposes of identifying the nth open period, rejected ones must be counted
c as an open period even though their length is undefined.
c This version requires a shut time first, so cannot include shut periods
c for which channel is open at t=0
	ifst=ifirst
	n=0			!counts number of open periods
	sum1=0.0		!add burst length
	sum2=0.0		!add TO/bst
c If open period that is open at t=0 is to be included, then an alternative
c bit of code is needed to find the start of the first open period in a jump
c (but this could have more openings concat with it)
c=	if(.not.exczero) then
c=	   a1=ampl(ifst)
c=	   open1=a1.ne.0
c=	   setbad1=badint(ifst)		!tint(i) was unusable
c=	   bad1=setbad1.or.(dubious(ifst).and.exass)
c=	   if(bad1) then
c=		n=n+1      		!count this as nth open period, although not usable
c=		goto 80  		!look for next good regular start
c=	   else
c=		n=n+1 		!count this as nth open period
c=		ifst=ifst-1		!so ifst set correctly after 53
c=		goto 53		!valid 'start' now found -look for more openings
c=	   endif
c=	endif
c
c Look for normal (not open at t=0) 'start of open period'
80	continue
	do while (ifst.le.ilast-1)
	   a1=ampl(ifst)
	   a2=ampl(ifst+1)
	   open1=a1.ne.0
	   open2=a2.ne.0
	   shut1=.not.open1
	   setbad1=badint(ifst)		!tint(i) was unusable
	   setbad2=badint(ifst+1)		!tint(i) was unusable
c	   bad1=setbad1.or.(dubious(ifst).and.exass)
c	   bad2=setbad2.or.(dubious(ifst+1).and.exass)
	   if(debon) then	!print first in group
		print 501,ifst,tint(ifst),ampl(ifst),iprops(ifst)
      	if(discprt) write(8,501)ifst,tint(ifst),ampl(ifst),
     &		iprops(ifst)
	   endif
	   if(setbad1.or.setbad2) then
		ifst=ifst+1
		n=n+1      		!count this as nth open period, although not usable
		bad=.true.		!not reset true until end of current burst
		goto 80  		!look for next good start
	   else if(shut1.and.open2) then
		if(debon) then	!print first in group
		   print 507
      	   if(discprt) write(8,507)
507		   format(' start of open period found')
		endif
		n=n+1 		!count this as nth open period
		goto 81		!valid 'start of open period' now found
	   endif
	   ifst=ifst+1
	enddo
c if this point reached, all intervals inspected
	j=j-1
	goto 83	!all intervals in this jump inspected
c valid 'start of open period' found
81	continue
	j=j+1		!count number of open periods found (reversed if bad)
	ifst=ifst+1		!index of first opening in open period just started
c now sum1=bst length, sum2=TO/bst
	sum1=sum1 + tint(ifst)	!add length of this opening to prev op period
	sum2=sum2 + tint(ifst)	!add length of this opening to prev op period
	if(debon) then	!print first in group
	   print 501,ifst,tint(ifst),ampl(ifst),iprops(ifst)
         if(discprt) write(8,501)ifst,tint(ifst),ampl(ifst),iprops(ifst)
	endif
c if last interval is open, and not excluded, there are no more intervals
c to find, and do not want the next 'do while' to decrement j, so skip to end.
	if(.not.exclast.and.ifst.eq.ilast) then
	   k=ifst	!so do not return to 80 to look for new group
	   goto 82
	endif
	k=ifst+1	!index of interval that follows the first opening
c Now find the rest of the openings in this open period
	do while (k.le.ilast)
	   if(debon) then		!print rest in group (inc shutting that ends it)
		print 501,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
	   endif
	   a1=ampl(k)
	   open1=a1.ne.0
c Go to 82 when end of group found.
c Whole group is set bad, and so rejected, if (1) contains any unusable
c opening (2) when, if exass=true, it contains any opening with dubious
c amplitude OR if the interval that ends the group has 'dubious'amp.
	   if(.not.open1) goto 82		!end of group found
c now sum1=bst length, sum2=TO/bst
	   sum1=sum1 + tint(k)	!add any subsequent openings to same YVAL
	   sum2=sum2 + tint(k)	!add any subsequent openings to same YVAL
	   setbad=badint(k)		!tint(k) was unusable
	   if(setbad) bad=.true.	!this group bad, but finish anyway
	   k=k+1
	enddo
c get here if last interval in jump is reached without finding end
c of the current open period
	j=j-1		! j not used for anything here
	goto 83	!all intervals in this jump inspected
c
c Open period completed
82	continue
	n1=n1+1			!count number of open periods/burst
c
	ifst=k	!so do not skip past shut time needed to start new group
	last=null.or.BTEST(iprops(k),6)
	ygap(j)=TINT(k)   !record length of shut time following open period #j
	badgap=badint(k)	!set true if gap is bad
c=	indexg(j)=jumps(ij)	!record jump in which ygap(j) occurs
c NB ygap() is used only for id=115 and id=116 so bad value does not
c matter here e.g. for open periods per jump (id=114) for which last
c opening is used (if open at end there is no following gap -see next bit)
	if(ampl(k).ne.0) then
	   ygap(j)=-1.e20		!error value!
	   if(BTEST(iprops(k),6)) then
	      print 702,k
	      if(discprt) write(8,702) k
c702	      format(' Event #',i5,' is open at end of jump')
	   else
	      print 700,k,tint(k),ampl(k),iprops(k)
	      if(discprt) write(8,700) k,tint(k),ampl(k),iprops(k)
c700	      format(' YGAP ERROR:',i5,2g13.6,i3)
	   endif
c	abort burst and go to next sweep if open at end
	   nbad1=nbad1+1	!count # of aborted bursts
	   sum1=0.0		!initialise for next burst
	   sum2=0.0		!initialise for next burst
	   n1=0.0		!initialise for next burst
	   bad=.false.	!initialise for next burst
	   goto 79
	endif
	if(debon) then
	   print 701,j,ygap(j)
	   if(discprt) write(8,701) j,ygap(j)
c701	   format(16x,'  Ygap(',i4,') = ',g13.6,/)
	endif
c NOW check if end of burst reached
	if(badgap) then
	   endbst=endbad		!bad gap ends burst in endbad=true
	else
	   endbst=ygap(j).gt.tcrit
	endif
c=	if(ygap(j).lt.tcrit) then
	if(.not.endbst) then
	   sum1=sum1 + ygap(j)		!add shut time for total bst length
	   if(badgap) bad=.true.	!bad gap makes whole burst bad
	   if(ifst.lt.ilast) goto 80	!look for start of next open period
c Arrive here if last interval in jump is reached, but gap after it is
c shorter than tcrit.  In this case, burst is not complete so this burst
c must be discarded, so abort burst and go to next sweep
	   sum1=0.0		!initialise for next burst
	   sum2=0.0		!initialise for next burst
	   n1=0.0		!initialise for next burst
	   bad=.false.	!initialise for next burst
	   goto 79
	else		!burst ended
	   if(.not.bad) then
		j1=j1 + 1	!count number of bursts
      	n2=n2 + 1 	!count number bursts per sweep
	      if(id.eq.131) then
		   Yval(j1)=sum1	!burst length
      	else if(id.eq.132) then
		   Yval(j1)=sum2	!TO/burst
	      else if(id.eq.133) then
		   Yval(j1)=float(n1) 	!no of open periods/burst
	      endif
		if(debon.and.id.ne.134) then
		   print 502,j1,yval(j1),jumps(ij)
	         if(discprt) write(8,502) j1,yval(j1),jumps(ij)
c502		   format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
		endif
	   else
		nbad1=nbad1+1	!count # of bad bursts
		if(debon) then
		   print 506
	         if(discprt) write(8,506)
506		   format(36x,' Burst contained a bad interval -discarded')
		endif
	   endif
	   sum1=0.0		!initialise for next burst
	   sum2=0.0		!initialise for next burst
	   n1=0.0		!initialise for next burst
	   bad=.false.	!initialise for next burst
	   if(ifst.lt.ilast) goto 80	!look for start of next open period
c						!i.e. start of next burst
	endif
C N.B. does not return if ifst=nint, as if tint(nint) is start of an
C open period there is no way to tell if this group has been completed-
C (the next interval might have been open too)
c=/C  NOTE that NYVAL=naset calc here may not be identical with that
C found in calc of burst distributions because latter includes
C only those open periods that are part of valid bursts (no bad
C openings and proper end)
c
83	continue	!all intervals in this jump inspected
	if(id.eq.134) then
	   Yval(ij)=float(n2)		!number of bursts per sweep
	   n2=0				!initialise for next sweep
	   if(debon) then
		print 502,ij,yval(ij),jumps(ij)
	      if(discprt) write(8,502) ij,yval(ij),jumps(ij)
c502		format(36x,' Yval(',i4,') = ',g12.5,' (jump ',i5,')')
	   endif
	endif
c
79	continue	! end of 'do 79 ij=1,nsweep'
c
c All sweeps done
	if(id.ge.131.and.id.le.133) then
	   NYVAL=j1		!no of bursts
	else if(id.eq.134) then
	   NYVAL=nsweep
	endif
c
	goto 207	!Yval() now defined
c
c
c ALL DONE
207	continue
	if(allocated(ygap)) DEALLOCATE(ygap)
	if(idest.eq.5) then
         print 86, nyval
         if(pon()) write(7,86) nyval
         if(discprt) write(8,86) nyval
86	   format(/,' ',i5,' values for distribution')
	else
         print 50, nyval,nfitt
         if(pon()) write(7,50) nyval,nfitt
         if(discprt) write(8,50) nyval,nfitt
50	   format(/,' ',i5,' values for distribution, plus ',i3,
     &	' truncated values')
	endif
	if(nth.gt.0.and.id.eq.102.or.id.eq.104) then
         print 51,nbad
         if(pon()) write(7,51) nbad
         if(discprt) write(8,51) nbad
51	   format(' ',i4,' sweeps had no nth shut time')
	endif
	if(id.ge.131.and.id.le.134) then
         print 87,nbad1
         if(pon()) write(7,87) nbad1
         if(discprt) write(8,87) nbad1
87	   format(
     & ' ',i4,' bursts aborted by undefined intervals, or because ',
     & 'open at end')
	endif
c
999	continue
	if(nth.gt.0.and.id.eq.111.or.id.eq.113) then
         print 64,nbad
         if(pon()) write(7,64) nbad
         if(discprt) write(8,64) nbad
64	   format(' ',i4,' sweeps had no nth open period')
         print 65,nbad1
         if(pon()) write(7,65) nbad1
         if(discprt) write(8,65) nbad1
65	   format(' ',i4,' open periods aborted')
	endif
c
	nth=nthsav		!for titlex in scvdu
	idtype=id
	RETURN		!Rest done in SETBIN
	END



