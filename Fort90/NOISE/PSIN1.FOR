	subroutine PSIN1(rate,rate0,filt,fn,nskip,res,nsize,nsizet,
     & nmask,ipmon,errfac,istart,iend,ngood,nchan,npsmax,npfmax,nsmax,
     & ibcor,ngsub,nspecbit,igval,ndev,restart,contin,psprt)
c For further input in PSREAL
	integer istart(512),iend(512)
c	real*4 enpnt(512)
	integer npnt(512)
	integer*2 igval(512)  !for subtracted bits on disc
	character*1 ans,UC
	character*2 ndev
	logical restart,contin,psprt
	logical pon,slock,debug,caplock
	logical discprt
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c
	nskip=1		!unless redefined below
c
c If reanalysing data already fitted (ibcor=2) then must read ngsub,igval()
c and the modified istart(),iend(), from disc.  Do this also if CONTINuing
c fitting from where stopped earlier, because in this case values for all
c fits before the point at which fitting restarted will be missing from
c these arrays in memory
	if(restart.or.contin) then	!read ngsub,istart,iend,igval from disc
         OPEN(unit=16,file=ndev//'\NOISPAR.DAT',status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(16,rec=1) ngsub
	   do 45 i=1,ngsub
	    irec=5 + (i-1)*116
	    read(16,rec=irec) ig,istart(ig),iend(ig)
	    igval(i)=int2(ig)
	    if(debug()) print 451,i,ig,istart(ig),iend(ig)
451	    format(' i,ig,istart(ig),iend(ig)=',2i6,2i10)
45	   continue
	CLOSE(unit=16)
	endif
c Calc array containing no of points in each good bit (so # of spectra
c that can be got from data can be pre-specified)
	npmin=2147483647		!min,max no of points/good bit (2**31 -1)
	npmax=0.
	do 10 ig=1,ngood
	  np=iend(ig) - istart(ig) + 1
	  if(np.gt.npmax) npmax=np
	  if(np.lt.npmin) npmin=np
	  npnt(ig)=np
	  if(debug()) print 11,ig,np,istart(ig),iend(ig)
11	  format(1x,i5,' :number of good points= ',i12,/,1x,
     & ' (point ',i6,' to point ',i6,')')
10	continue
c
	rate0=rate	!orig rate (RATE will be reduced if NSKIP>1)
26	continue
	print 200,rate
200	format(' Sample rate (Hz)= ',g13.6)
	if(debug()) then
	 print 206
206	 format('&   O.K. [Y] ? ')
	 read 101,ans
	 if(UC(ans).ne.'N') goto 208
	 print 207
207	 format(' Sample rate (Hz) = ')
	 read 21,rate
21	 format(g13.6)
208	 continue
	 if(ibcor.ne.3) then
	  print 209,filt
209	  format('&Low pass filter (Hz) = ',g13.6)
	  print 206
	  read 101,ans
	  if(UC(ans).ne.'N') goto 205
	  print 210
210	  format(' Low pass filter (Hz) = ')
	  read 21,filt
205	  continue
	 endif
	endif
c
	fn=rate/2.	!Nyquist
	if(ibcor.eq.3) then
	   nskip=1
	else
	   r=fn/filt
	   if(r.lt.2.) goto 24
	   print 25,fn,filt
25	   format(' Data overfiltered/sampled unecessarilly fast',/,
     &   ' Spectrum goes up to (Hz)= ',g13.6,' Hz, but filtered at',/,
     &    g13.6,' Hz. Omit data points to reduce sampling rate? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 24
	   print 251
251	   format(' -omit every Nth data point: N= ')
	   read 2,nskip
	   rate=rate/float(nskip)
	   goto 26
	endif
24	continue
	print 23,fn
23	format(' Spectrum goes up to (Hz)= ',g13.6)
	print 3
3	FORMAT( ' Resolution (Hz, real)= ')
	read 21,RES
	nsize=ifix(0.1+rate/res)
	if(IPOW2(nsize).gt.0) goto 22
c NSIZE nust be a power of 2. If not find nearest smaller and greater
c power of 2 and recalc res.
	n=ifix(alog(float(nsize))/alog(2.))
	nsize=2.0**n
	ns1=2*nsize
	nst=nsize*nskip		!no of orig points
	nst1=ns1*nskip
	res=rate/float(nsize)
	res1=rate/float(ns1)
c Calc no of spectra that can be got from data with res/res1
c (could actually get more if too good bits sep by few enough bad points
c that latter can be masked). But, particularly in cases where exponentials
c fitted, may get fewer if points set bad (=1.e20) within a 'good' (fitted)
c bit, or if some 'good bits' rejected altogether in NOISFIT -can allow for
c latter by using ngsub,igval defined in NOISFIT
	ispec=0
	ispec1=0
	if(ibcor.eq.1) then	!stationary noise
	   do 12 i=1,ngood		!go through each good bit
c	Calc no of spectra from npnt(i) original points
	   ispec=ispec + ifix(.001+float(npnt(i))/float(nst))
	   ispec1=ispec1 + ifix(.001+float(npnt(i))/float(nst1))
12	   continue
	else if(ibcor.eq.2.or.ibcor.eq.3) then	!nonstationary noise
	   do 13 i=1,ngsub		!number of subtracted bits defined
	   ig=int4(igval(i))		!original good bit from which it comes
	   np=iend(ig)-istart(ig)+1	!no of points in it (some may be bad!)
	   ispec=ispec + ifix(.001+float(np)/float(nst))
	   ispec1=ispec1 + ifix(.001+float(np)/float(nst1))
13	   continue
	endif
c
c	print 27,nsize,res,1.e3/res,ispec,ns1,res1,1.e3/res1,ispec1
c27	format(' Choose',/,
c     & ' (1) Data points/spectrum= ',i8,'; resolution= ',g11.4,/,
c     & ' msec/spectrum= ',g11.4,'; estim. number of spectra= ',i5,/,
c     & ' (2) Data points/spectrum= ,',i8,'; resolution= ',g11.4,/,
c     & ' msec/spectrum= ',g11.4,'; estim. number of spectra= ',i5,/,
c     & '  Option 1 or 2 (or 0=try other values)= ')
	print 27,nsize,1.e3/res,nsize/2,fn,res,ispec,
     & ns1,1.e3/res1,ns1/2,fn,res1,ispec1
27	format(' Choose',/,
     & ' (1) Data points/spectrum= ',i8,'; msec/spectrum= ',g11.4,/,
     & '    frequency points/spectrum= ',i8,'; max frequency (Hz)= ',
     & g11.4,/,'    resolution= ',f11.4,'; estim. number of spectra= ',
     & i5,/,
     & ' (2) Data points/spectrum= ',i8,'; msec/spectrum= ',g11.4,/,
     & '    frequency points/spectrum= ',i8,'; max frequency (Hz)= ',
     & g11.4,/,'    resolution= ',f11.4,'; estim. number of spectra= ',
     & i5,/,
     & ' Option 1 or 2 (or 0=try other values)= ')
	read 2,i
2	format(i8)
	if(i.le.0) goto 24
	if(i.eq.1) goto 22
	nsize=ns1
	res=res1
22	continue
	if(nsize.le.npfmax) goto 29
	call BELL(1)
	print 30,npfmax
30	format(
     & ' Cannot have more than ',i5,' data points/spectrum at present')
	goto 24
c Calc no of contiguous good (or maskable) points needed to get
c NSIZE points in YSPEC
29	nsmax=1024		!unless reset
	print 291
291	format(
     &' Calculate the max possible number of spectra (up to 1024) [Y] ')
	read 101,ans
	if(UC(ans).ne.'N') goto 292
	print 293
293	format(' Number of spectra to be calculated = ')
	read 2,nsmax
292	continue
c
	nsizet=nsize*nskip
	nspecbit=0		!used only for ibcor=2,3
	if(ibcor.eq.2.or.ibcor.eq.3) then
	   print 31
31	   format(
     & ' Calculate only the first n spectra from each fitted bit',/,
     & ' [0 to calculate all possible spectra]: n = ')
	   read 2,nspecbit
	   if(nspecbit.gt.0) then
      	if(pon()) write(7,32) nspecbit
      	if(discprt) write(8,32) nspecbit
32		format(/,
     &   ' Only first ',i3,' spectra calculated from each fitted bit',/)
	   endif
	endif
c Print values chosen
	print 28,rate,nsize,1.e3/res,res,nsize/2,fn
	if(pon()) write(7,28) rate,nsize,1.e3/res,res,nsize/2,fn
      if(discprt) write(8,28) rate,nsize,1.e3/res,res,nsize/2,fn
28	format(
     & ' Sample rate= ',g11.4,'; Data points/spectrum= ',i8,/,
     & ' msec/spectrum= ',g11.4,' resolution= ',f11.4,/,
     & ' frequency points/spectrum= ',i8,'; max frequency (Hz)= ',g11.4)
	if(nskip.eq.1) goto 281
	print 282,nskip,rate0,nsizet
	if(pon()) write(7,282)nskip,rate0,nsizet
      if(discprt) write(8,282)nskip,rate0,nsizet
282	format(/,' Every',i3,'th point omitted:',/,
     & '  original sample rate= ',g13.6,/,
     & '  number of original points/spectrum= ',i5,/)
281	continue
c
	nmask=ifix(0.02*float(nsize))
	print 40,nmask
	if(pon()) write(7,40)nmask
      if(discprt) write(8,40)nmask
40	format(' Up to 2% of points may be masked= ',i5,' points')
c
	ipmon=1
	print 1391,ipmon
1391	format(
     & ' (1) Show data for spectrum if it is atypical',/,
     & ' (2) Check data for every spectrum on screen ',/,
     & ' (3) Calculate spectra without showing data',/,
     & '  Option number [',i2,'] = ')
	read 2,i
	if(i.ge.1.and.i.le.3) ipmon=i
	errfac=2.0		!default
	if(ipmon.eq.1) then
	   print 35,errfac
35	   format(
     & ' Display data if mean or var differs from average by factor',/,
     & ' [',f5.2,'] = ')
	   read 21,x
	   if(x.gt.1.e-10) errfac=x
	endif
c	read 101,ans
c	if(UC(ans).ne.'N') ipmon=1
c
	if(ipmon.eq.2) then
	   print 44
44	   format(
     &  '&Check mean current (rather than noise) on screen [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') ipmon=-ipmon
	endif
C
c Print titles for printout in CALCPS
	psprt=.true.
	print 60
60	format(' Print out mean & variance for each spectrum [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') psprt=.false.
	if(.not.psprt) goto 99		!no titles
	if(nchan.eq.1.and.ibcor.eq.1) then
         print 50
         if(pon()) write(7,50)
         if(discprt) write(8,50)
50	   format(
     &' Spectrum  From bit  Bad pnts  Variance (pA**2)')
	else
	  if(ibcor.eq.1) then
         print 51
         if(pon()) write(7,51)
         if(discprt) write(8,51)
51	   format(
     &' Spectrum  From bit  Bad pnts  Variance (pA**2)    Mean current')
	  else if(ibcor.eq.2.or.ibcor.eq.3) then
         print 52
         if(pon()) write(7,52)
         if(discprt) write(8,52)
52	   format(
     &' Spectrum  From bit  Bad pnts  Variance (pA**2)    Mean current',
     &/,'                                             ',
     &  ' Obs      Obs-asym       Calc')
	  endif
	endif
c
99	continue
	RETURN
	end


