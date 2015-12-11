	subroutine CALCPS(Yspec,Ymc,nspec,nsize,npspec,nchan,ibad,
     & ymsum,nmsum,tsum,ntsum,isec0,ip0,isecn,ipn,ig,PS,
     & varv,cmean,varsav,ybsav,timsav,calfac,calfac1,
     & ipmon,errfac,ibcor,ym1,ym2,reject,psprt)
c To display data and calculate PS when nsize usable  points (of which
c ibad (<=nmask) may be bad) have accumulated in Yspec()
	dimension PS(2048)
	real*4 varsav(1024),ybsav(1024),timsav(1024)
	dimension YSPEC(4096)	!for spectrum
	dimension Ymc(4096)	!for mean current display
	character ch,getch
	integer*2 ktype
      character*1 ans,UC
	logical reject,kbhit,disp,psprt
	logical discprt,pon,slock,caplock,debug
	common/dp/discprt
c
c
	pon()=slock()
	debug()=caplock()
c
101   format(a1)
c
	ycrit=1.e19		!all above this are bad
	call VARN(Yspec,Nsize,ycrit,nfolt,YBAR,VARY)
	if(nfolt.ne.ibad) then
	   call BELL(1)
	   print 1,ibad,nfolt
1	   format(' ibad,nfolt = ',2i6,': should agree!')
	endif
c
	if(debug()) then
	   print 20
20	   format(' Print Yspec (before subtraction) and Ymc [N] ?')
         read 101,ans
         if(UC(ans).eq.'Y') then
		print 23
		if(discprt) write(8,23)
23		format('  i   Yspec(i) (not sub)   Ymc(i)')
		do 21 i=1,nsize
		print 22, i,Yspec(i),Ymc(i)
		if(discprt) write(8,22) i,Yspec(i),Ymc(i)
22		format(i8,2x,g13.6,2x,g13.6)
21		continue
		call VARN(Ymc,Nsize,ycrit,nfolt,Ymcm,Vymc)
		print 24,Ybar,Ymcm,vary,Vymc
		if(discprt) write(8,24) Ybar,Ymcm,vary,Vymc
24          format(' mean =',5x,g13.6,2x,g13.6,/,
     &      ' variance =',1x,g13.6,2x,g13.6)
	   endif
	endif
c calc mean current (if nchan=2 -no! for non-stationary case at least
c get mean current from ADC0 record)
	if(nmsum.gt.0) ymean=ymsum/float(nmsum)	!in pA
	if(ntsum.gt.0) tmean=tsum/float(ntsum)	!time for this mean
c
c Now normalise good points to zero mean, and set bad (masked) points
c to zero too. (If ibcor=2 mean should be zero but will do no harm
c to subtract it)
	do 33 i=1,nsize
	if(yspec(i).gt.ycrit) then
	   yspec(i)=0.0
	else
	   yspec(i)=yspec(i)-ybar
	endif
33	continue
c
c Correction for masking
	corfac=1.0/(1.0-float(nfolt)/float(nsize))
cc	nspec=nspec+1		!now in calling prog
c	VARY=VARY*CALFC2*corfac		!VARIANCE IN AMP**2
c	vary=vary*1.e-24*corfac		!corrected variance in amp**2
	vary=vary*corfac		!corrected variance in pA**2
c Note that if this spectrum is rejected (below) then nspec is decremented
c again so following values will be overwritten by next good values
	varv=varv+vary		!accum for overall mean
	cmean=cmean+ymean		!ditto
	varsav(nspec)=vary	!keep variance for VAR vs MEAN plot
	ybsav(nspec)=ymean	!keep mean for VAR vs MEAN plot
	timsav(nspec)=tmean	!keep time for this mean for MEAN vs t plot
c
c Now have data for 1 spectrum in YSPEC; check yspec on screen (if ipmon.ne.3)
c and calc PS.
	reject=.false.
	disp=.false.
c Restart checks by hitting 1 or 2?
	if(iabs(ipmon).eq.3) then
	   disp=.false.
	   if(KBHIT()) then
		ch=GETCH(ktype)
		ival=ichar(ch)
		if(ktype.eq.3) ikey=ival-48	!digit (or+-) hit; ikey=0,1,...,9 for return
		if(ikey.eq.1.or.ikey.eq.2) ipmon=ikey
		if(ipmon.eq.1) then
	   	  print 35,errfac
35	   	  format(
     & ' Display data if mean or var differs from average by factor',/,
     & 	' [',f5.2,'] = ')
c	   	  read 36,x
c36		  format(g13.6)
c	   	  if(x.gt.1.e-10) errfac=x
		  call INPUTr(errfac)
		endif
	   endif
	endif
	if(iabs(ipmon).eq.2) disp=.true.
	if(iabs(ipmon).eq.1.and.nspec.gt.1) then	!check if this spectrum is 'atypical'
	   am=cmean/float(nspec)   	!current mean
	   va=varv/float(nspec)
	   test=abs(ymean/am)
	   if(test.gt.errfac.or.test.lt.1./errfac) disp=.true.
	   test=abs(vary/va)
	   if(test.gt.errfac.or.test.lt.1./errfac) disp=.true.
	endif
	if(disp) call PSMON(yspec,ymc,nsize,nchan,ipmon,calfac,calfac1,
     & nspec,ymean,vary,ibcor,reject)
	if(reject) then
ccc==	   nspec=nspec-1	!reverse increment of nspec-DO IN MAIN PROG
	   varv=varv-vary		!reverse these too
	   cmean=cmean-ymean		!ditto
	   goto 91	 	!reject spectrum on screen
	endif
c
	call FORRT(Yspec,nsize)
C
	do 131 i=2,npspec
	A=Yspec(i)		!REAL PART OF OUTPUT
	B=Yspec(i+npspec)		!IMAG PART OF OUTPUT
	ps1=A*A + B*B
	j=i-1
	PS(j)=PS(j)+ps1
131	continue
	A=Yspec(npspec+1)
	PS(npspec)=PS(npspec)+A*A
	if(nfolt.gt.0) then
	   do 132 i=1,npspec
132		PS(i)=PS(i)*corfac
	endif
c
c	if(debug()) print 103,Npspec,A
c103	FORMAT(I6,G13.6)
	if(psprt) then
	 if(nchan.eq.1.and.ibcor.eq.1) then
	    if(pon()) write(7,421)nspec,ig,ibad,vary
          if(discprt) write(8,421)nspec,ig,ibad,vary
	    print 421,nspec,ig,ibad,vary
421	    format(i5,4x,i5,5x,i5,6x,g13.6)
	 else
	  if(ibcor.eq.1) then
	    if(pon()) write(7,423)nspec,ig,ibad,vary,ymean
          if(discprt) write(8,423)nspec,ig,ibad,vary,ymean
	    print 423,nspec,ig,ibad,vary,ymean
423	    format(i5,4x,i5,5x,i5,6x,g13.6,4x,g13.6)
	  else if(ibcor.eq.2.or.ibcor.eq.3) then
	    if(pon()) write(7,424)nspec,ig,ibad,vary,ymean,ym1,ym2
          if(discprt) write(8,424)nspec,ig,ibad,vary,ymean,ym1,ym2
	    print 424,nspec,ig,ibad,vary,ymean,ym1,ym2
424	    format(i5,4x,i5,5x,i5,6x,g13.6,3g12.5)
	  endif
	 endif
	endif
c
c Calc of one spectrum complete. Any more?
	if(debug()) then
        print 95,ybar
        if(pon()) write(7,95) ybar
        if(discprt) write(8,95) ybar
95	  format(' mean of points for PS (to be subtracted) = ',g13.6)
	  if(ibcor.eq.1) then
	    print 93,ip0,isec0,ipn,isecn
	    if(pon()) write(7,93) ip0,isec0,ipn,isecn
          if(discprt) write(8,93) ip0,isec0,ipn,isecn
93	    format(
     & ' (spectrum from point',i6,', section',i4,' to point',i6,
     & ', section',i4,')')
	  else if(ibcor.eq.2.or.ibcor.eq.3) then
	    print 94,ip0,ipn,ig
	    if(pon()) write(7,94) ip0,ipn,ig
          if(discprt) write(8,94) ip0,ipn,ig
94	    format(
     & ' (spectrum from point',i6,' to point',i6,
     & ', in good bit #',i4,')')
	  endif
	endif
c
91	continue	!jump here if spectrum rejected
c Re-initialise
	ibad=0
	ymsum=0.0
	nmsum=0
	tsum=0.0
	ntsum=0
c
	RETURN
	end



