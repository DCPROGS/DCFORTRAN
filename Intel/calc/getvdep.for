	subroutine GETVDEP(k,titlep,npar,IQ,readp,sameq,
     & vkin,vhold,vref,idest)
	character*10 titlep(200)
	character*1 ans,UC
	integer IQ(100,100)
	logical readp,sameq
	logical discprt,pon
	common/dp/discprt
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
c
	pon=.false.
c
	IF(.NOT.READP) GOTO 307
	IF(NVDEP.EQ.0) GOTO 501
	if(nvdep.gt.0) then
	   do L=1,nvdep
		print 322,IV(L),JV(L),HPAR(L)
	   enddo
322	   FORMAT(' For q(',2I2,')   H(mV) = ',F8.1)
	endif
c
501	if(sameq) goto 309
	print 308,nvdep
308	FORMAT(
     &' Same (',i3,' ) values for voltage-dependence of rates [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') GOTO 309
c
307	print 107
107	FORMAT(' Number of voltage-dependent rates = ')
	call INPUTi(nvdep)
	IF(NVDEP.EQ.0) GOTO 309
C RESULT DECREASES WITH HYPERPOL (INCREASES WITH V) IF H POSITIVE,
C I.E. RATE GETS SLOWER (TAU LONGER) WITH HYPERPOL
	print 1082
1082	format('&If H is positive rate slows (tau longer) with hyperpol')
	do i=1,npar
	   print 28,i,titlep(i)
28	   format(1x,i2,': ',a10)
	enddo
	do L=1,nvdep
82	   print 88
88	   format(' parameter number= ')
	   call INPUTi(m)
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   IV(L)=I
	   JV(L)=J
	   print 2081,I,J
2081	   FORMAT(' H(',2I2,') (mV)= ')
	   call INPUTr(Hpar(L))
	enddo
c
309	CONTINUE	!JUMP TO HERE IF H VALUES FROM DISK USED
c define cfkin here
	if(.not.pon.and.(.not.discprt)) goto 13
	if(pon) write(7,323) nvdep
      if(discprt) write(7,323) nvdep
323	format(/,1x,i3,' voltage-dependent rate constants')
	if(nvdep.gt.0) then
	   do 11 L=1,NVDEP
	   i=iv(L)
	   j=jv(L)
	   m=IQ(i,j)
	   if(pon) write(7,12)i,j,titlep(m),hpar(L)
         if(discprt) write(7,12)i,j,titlep(m),hpar(L)
12	   FORMAT(' For q(',2i2,')= ',a10,':   H (mV)= ',F9.1)
11	   continue
	endif
13	continue
c Add bit to check Vhold, Vref
	if(nvdep.gt.0) then
	   print 20,vref
20	   format(
     &  ' Reference potential (that for input rate constants) (mV) [',
     &    f8.3,'] = ')
	   call INPUTr(vref)
	   if(vhold.eq.0.) vhold=vref
	   print 21,vhold
21	   format(
     &  ' Holding potential (at which measurements made) (mV)  [',
     &    f8.3,'] = ')
	   call INPUTr(vhold)
	   vkin=vhold
	endif
c Array IQ defines correspondence
c between elements of THETA and elements of Q: IQ(i,j)=m where
c theta(m) is the parameter that goes in QT(i,j) (though may be
C NOW HAVE H VALUES
	RETURN		!from GETVDEP
	END
