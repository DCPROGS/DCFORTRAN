	subroutine Kcheck(nchan,gamma,ir,K,schan,readp,titlep,IQ,npar)
c Checks, and prints final values, for kA,kB,.., V-dep,nchan,gamma
	integer IQ(10,10)
	character*10 titlep(50)
	real*8 gamma(10)
	character*1 ans,UC
	logical schan,readp,pon,slock
	logical discprt
	common/dp/discprt
	COMMON/KBLK/KA,KB,KC,KD
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
c
	pon()=slock()
c
101	format(a1)
	IR=KA			!always??
	print 243,ka,kb,kc,kd
243	format(
     & ' No of states in each subset: kA,kB,kC,kD= ',4i3,': O.K.[Y]? ')
	input 101,ans
	if(UC(ans).ne.'N') goto 205
242	print 201
201	FORMAT('&  number of open states, kA= ')
	input 10,KA
	print 202
202	FORMAT('&  number of short shut states, kB= ')
	input 10,KB
	print 203
203	FORMAT('&  number of long-lived shut states C, kC= ')
	input 10,KC
	print 204
204	FORMAT('&  number very long (between cluster) shut states, kD= ')
	input 10,KD
	if((kA+kB+kC+kD).ne.k) goto 242
205	CONTINUE
	if(pon()) write(7,2431)ka,kb,kc,kd
      if(discprt) write(8,2431)ka,kb,kc,kd
2431	format(
     &   ' No of states in each subset: kA,kB,kC,kD= ',4i3)
c
c Check V-dependence
	IF(.NOT.READP) GOTO 307
	IF(NVDEP.EQ.0) GOTO 501
c Array IQ  defines correspondence
c between elements of THETA and elements of Q: IQ(i,j)=m where
c theta(m) is the parameter that goes in QT(i,j) (though may be
	DO 321 L=1,NVDEP
	i=iv(L)
	j=jv(L)
	m=IQ(i,j)
	print 322,i,j,titlep(m),hpar(L)
322	FORMAT(' For q(',2i2,')= ',a10,':   H (mV)= ',F8.1)
321	continue
c321	print 322,IV(L),JV(L),HPAR(L)
c322	FORMAT(' For q(',2I2,')   H(mV)= ',F8.1)
501	print 308,nvdep
308	FORMAT( ' Same (',i3,' ) H values [Y] ? ')
	input 101,ans
	if(UC(ans).ne.'N') GOTO 309
307	print 107
107	FORMAT( ' Number of V-dependent rates= ')
	input 10,NVDEP
	IF(NVDEP.EQ.0) GOTO 309
C RESULT DECREASES WITH HYPERPOL (INCREASES WITH V) IF H POSITIVE,
C I.E. RATE GETS SLOWER (TAU LONGER) WITH HYPERPOL
	print 1082
1082	format(' If H is positive rate slows (tau longer) with hyperpol')
c type parameter names if nmod>0
	print 28,(i,titlep(i),i=1,npar)
28	format(10(1x,i2,': ',a10,/))
	DO 1081	L=1,NVDEP
	print 88
88	format('&parameter number= ')		!for nmod>0
	input 10,m
	call GETIJ(IQ,k,i,j,m)		!get i,j for this parameter
87	IV(L)=I
	JV(L)=J
	print 2081,I,J
2081	FORMAT('&H(',2I2,') (mV; real)= ')
	input 21,HPAR(L)
1081	continue
309	CONTINUE	!JUMP TO HERE IF H VALUES FROM DISK USED
c print values
	if(.not.pon().and.(.not.discprt)) goto 13
	if(nvdep.eq.0) then
	   if(pon()) write(7,323)
         if(discprt) write(8,323)
323	   format(' No voltage-dependent rate constants')
	else
	   DO 11 L=1,NVDEP
	   i=iv(L)
	   j=jv(L)
	   m=IQ(i,j)
	   if(pon()) write(7,322)i,j,titlep(m),hpar(L)
         if(discprt) write(8,322)i,j,titlep(m),hpar(L)
11	   continue
	endif
C NOW HAVE H VALUES
c
c
13	if(.not.readp) goto 407
	print 320,NCHAN,IR,(1.0e12*gamma(i),i=1,IR)
320	FORMAT(' Number of channels= ',i10,/,
     & 3X,I3,' open states, with gamma (pS)=', 5G13.6)
	print 43
43	FORMAT( ' Use same no of channels, and gamma values [Y] ? ')
	input 101,ans
	if(UC(ans).ne.'N') GOTO 402	!IF NOT OVERWRITE WITH NEW ONES
C
407	continue
	DO 100 I=1,K	!INITIALISE
100	gamma(i)=0.0d0
c	nchan=10000000
c	if(schan) nchan=1
	print 403
403	FORMAT(' number of channels (integer)= ')
	input 1,nchan
1	format(i10)
c
c	if(schan) goto 4061
c	print 404
c404	FORMAT(' number of open states= ')
c	input 10,IR
10	format(i8)
4061	continue
	DO 406 I=1,IR
	print 405,I
405	FORMAT( ' gamma(',I2,') (pS)= ')
	input 21,gamma(i)
21	format(g13.6)
	gamma(i)=gamma(i)*1.0e-12		!IN SIEMENS
406	CONTINUE
402	continue
c print values
	if(pon()) write(7,320) NCHAN,IR,(1.0e12*gamma(i),i=1,IR)
      if(discprt) write(8,320) NCHAN,IR,(1.0e12*gamma(i),i=1,IR)
c
	RETURN
	end

