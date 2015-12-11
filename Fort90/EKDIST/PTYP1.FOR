	subroutine PTYP1(THETA,AREA,AMEAN,SD,KMAX,NCOMP,YHIGH,YLOW,NFIT,
     & ANTRUE,ERRORS,GAMDST,ISG,idtype,ahist,thist,obhist,ghist,
     & excreg,yexclow,yexchigh,isdcon,anfit,nsweep,nnull,nfitt)
C
c Modif 12/14/00 06:09pm for nset>1
C 11/09/95 08:01am Now sorts parameters into descending order of tau
c 06/03/89 06:42pm Lahey version
c
C TO TYPE FITTING RESULTS IN EKDIST AND TO ASSIGN AREA(),AMEAN() AND
C SD() FROM THETA, AND CALC ANTRUE.
C	PTYP1=VERSION IN EKDIST THAT DOES CALCS, BUT PRINTS RESULTS ONLY
C		IF ERRORS NOT WANTED
C
	REAL THETA(50)
c	REAL AREA(10),AMEAN(10),SD(10)
c for nset>1 need
	real antrue(10)
	real area(10,10),amean(10,10),sd(10,10)	!amp fits only for nset=1?
	integer ncomp(10),nfit(10)
	real*4 yexclow(10),yexchigh(10)
	real*4 ylow(10),yhigh(10)
	integer itcon(10,10),ialoc(10,10),itloc(50)	!see below
	integer*4 neq,ieq(50),jeq(50),ieq0(50),jeq0(50)
c	common/const/nset1,neq,ieq0,jeq0,ieq,jeq,itcon,ialoc,itloc
	common/const/nset,neq,ieq0,jeq0,ieq,jeq,itcon,ialoc,itloc
c
	character*1 ans
C
	LOGICAL AHIST,THIST,OBHIST,ghist,ERRORS,GAMDST,pon,slock,excreg
	logical usewfreq,constrained
	logical discprt
	common/dp/discprt
	common/gapsav/ncomps(10),ameans(10,10),areas(10,10)
	common/ampsav/ncompa,ameana(10),areaa(10),sda(10)
C
	pon()=slock()
c
	jset=1 	!only for 1 set at present, for amps etc
	if(nfit(1).le.0) then
	   print 14
14	   format(' No parameters to type')
	   RETURN
	endif
C FIRST ASSIGN THETA VALUES TO AREA(),AMEAN() AND SD() (PREV DONE
C IN MLFIT-NEEDED FOR CALC CURVE IN SCVDU).
13	continue
	usewfreq=ahist.and.idtype.eq.7
	F1=0.0
	F2=0.0
	f3=0.0
c
c=	IF(.NOT.THIST) GOTO 10
	if(obhist.and.nset.eq.1) goto 10	!original code
	if(thist.or.(obhist.and.nset.gt.1)) then
	 if(nset.eq.1) then
	   J=0
	   SA=0.
	   DO 11 I=1,NCOMP(1)
	   J=J+1
	   AMEAN(I,1)=THETA(J)
	   IF(I.EQ.NCOMP(1)) GOTO 11
	   J=J+1
	   AREA(I,1)=THETA(J)
	   SA=SA+AREA(I,1)
11	   CONTINUE
	   AREA(NCOMP(1),1)=1.0-SA
	   call SORT2r(amean(1,1),area(1,1),ncomp(1),10,.true.)
	   IF(GAMDST) then		!only for nset=1
c=		GOTO 12	!nset=1 in this case
C  NOW ANTRUE FOR GAMMA (ETC) DIST (SEE MLFIT1)
c         (call should be OK for nset=1)
c12		CALL CUMGAM(NCOMP(1),ISG,YLOW(1),YHIGH(1),
c     &		AMEAN(1,1),AREA(1,1),F1,F2)
		if(ncomp(1).eq.1) then
		   rate=1.0/amean(1,1)
		   t1=ylow(1)
		   t2=yhigh(1)
		   n=isg+1				!number of openings
		   D=GAMMAinc(t1,rate,n)-GAMMAinc(t2,rate,n)	!Prob[ylow < t < yhigh]
		   ANTRUE(1)=FLOAT(NFIT(1))/D
		   GOTO 100
		else if(ncomp(1).eq.2) then
		   print*, ' Not yet done for 2 open time components'
		   GOTO 100
		endif
	   endif
C   CALC ANTRUE,F1,F2,DENOM=PROB(YLOW<OBS<YHIGH)
	   DO 112 J=1,NCOMP(1)
	   F1=F1+AREA(J,1)*exp1(-YLOW/AMEAN(J,1))	!PROB(OBS>YLOW)
	   F2=F2+AREA(J,1)*exp1(-YHIGH/AMEAN(J,1))	!PROB(OBS>YHIGH)
112	   CONTINUE
	   ANTRUE(1)=FLOAT(NFIT(1))/(F1-F2)
	   IF(ERRORS) GOTO 999
	   GOTO 100		!original printout for nset=1 case
	endif
c=====NB next bit works only if tau(set 2) is constrained to equal tau(set 1)
c=====If defined other way round, the amean(i,j) on rhs has not yet been
c====defined
c	if(nset.gt.1) then
c	  m=0
c	  do j=1,nset 	!log likelihood calc sep for each set, and summed at end
c	   do i=1,ncomp(j)
c		if(neq.gt.0) then		!check if the current tau is constrained
c		   do n=1,neq
c			if(i.eq.ieq(n).and.j.eq.jeq(n)) then
c			   amean(ieq(n),jeq(n))=amean(ieq0(n),jeq0(n))
c			   goto 22
c			endif
c		   enddo
c		endif
c		m=m+1
c		amean(i,j)=theta(m)	!tau
c22		continue
c		if(i.lt.ncomp(j)) then
c		   m=m+1
c		   area(i,j)=theta(m)
c		endif
c         enddo			!end of i=1,ncomp(j)
c	  enddo			!end of j=1,nset
c First set all the non-constrained amean(i) (and the areas, never constrained)
	if(nset.gt.1) then
	   m=0
	   do j=1,nset 	!log likelihood calc sep for each set, and summed at end
		do i=1,ncomp(j)
		   m=itcon(i,j)
		   if(m.ne.0) then
			amean(i,j)=theta(m)
		   endif
		   n=ialoc(i,j)
		   if(n.ne.0) then
			area(i,j)=theta(n)
		   endif
		enddo
	   enddo
c  now set the constrained amean(i)
	  m=0
	  do j=1,nset 	!log likelihood calc sep for each set, and summed at end
	   do i=1,ncomp(j)
		if(neq.gt.0) then		!check if the current tau is constrained
		   do n=1,neq
			constrained=i.eq.ieq(n).and.j.eq.jeq(n)
			if(constrained) then
			   amean(ieq(n),jeq(n))=amean(ieq0(n),jeq0(n))
			endif
		   enddo
		endif
         enddo			!end of i=1,ncomp(j)
	  enddo			!end of j=1,nset
c
c All amean(i,j) set, and all area(i,j) except for last -set that now for each set
c
	  do j=1,nset
	   if(ncomp(j).eq.1) then
		area(1,j)=1.0
	   else
		sa=0.0	!total area exc last
		do i=1,ncomp(j)-1
		   sa=sa+area(i,j)
		enddo
		area(ncomp(j),j)=1.0 - sa
	   endif	!end of ncomp>1
	  enddo	!end of j=1,nset
c All amean, area assigned (inc constrained ones)
	  do j=1,nset
	   call SORT2r(amean(1,j),area(1,j),ncomp(j),10,.true.)
	   f1=0.0
	   f2=0.0
	   if(thist) then
	    do i=1,ncomp(j)
		f1=f1+area(i,j)*exp1(-ylow(j)/amean(i,j))		!Prob(obs>ylow)
		f2=f2+area(i,j)*exp1(-yhigh(j)/amean(i,j))	!Prob(obs>yhigh)
	    enddo
	   else if(obhist) then
	    do i=1,ncomp(j)
		x=1.0-(1.0/amean(i,j))		!=RHO(i,j)
		f1=f1+area(i,j)*(x**(ylow(j)-1.0))	!PROB(OBS=>YLOW)
		f2=f2+area(i,j)*(x**(yhigh(j)))	!PROB(OBS>YHIGH)
	    enddo
	   endif
	   antrue(j)=float(nfit(j))/(f1-f2)	!for current set
	   if(errors) goto 999
	   print 412,j
	   if(discprt) write(8,412) j
412	   format(/,' FIT FOR SET #',i3)
c
c       For case of nset>1 do printout here (rather than label 100)
	   s=0.0
	   do i=1,NCOMP(j)
	    if(thist) then
		w=area(i,j)*1000./amean(i,j)
		en=antrue(j)*area(i,j)
		if(itcon(i,j).ne.0) then
		   print 41,I,AMEAN(I,j),1000./AMEAN(I,j)
		   if(discprt) write(8,41) I,AMEAN(I,j),1000./AMEAN(I,j)
41		   FORMAT( ' Tau(',I1,') (ms)= ',G13.6,
     & 	   '     lambda (1/s)= ',G13.6)
		else
		   print 411,I,AMEAN(I,j),1000./AMEAN(I,j)
		   if(discprt) write(8,411) I,AMEAN(I,j),1000./AMEAN(I,j)
411		   FORMAT( ' Tau(',I1,') (ms)= ',G13.6,
     & 	   '     lambda (1/s)= ',G13.6,' (constrained)')
		endif
	      if(discprt) write(8,43) I,AREA(I,j),en,w
	      print 43,I,AREA(I,j),en,w
43	      FORMAT(' Area(',i2,') = ',G13.6,':  number = ',g13.6,
     &	 ':  amplitude (1/s) = ',G13.6)
	      s=s+area(i,j)*amean(i,j)
	    else if (obhist) then
		w=area(i,j)/amean(i,j)
		en=antrue(j)*area(i,j)
		if(itcon(i,j).ne.0) then
		   print 45,I,AMEAN(i,j),1.0 - (1.0/amean(i,j))
		   if(discprt) write(8,45) I,AMEAN(i,j),
     &		1.0 - (1.0/amean(i,j))
45		   FORMAT(' Mean(',i2,') (ops/bst) = ',
     &	    G13.6,':        rho = ',G13.6)
		else
		   print 44,I,AMEAN(i,j),1.0 - (1.0/amean(i,j))
		   if(discprt) write(8,44) I,AMEAN(i,j),
     &		1.0 - (1.0/amean(i,j))
44		   FORMAT(' Mean(',i2,') (ops/bst) = ',G13.6,
     &	 ':        rho = ',G13.6,' (constrained)')
		endif
	      if(discprt) write(8,46) I,AREA(I,j),en,w
	      print 46,I,AREA(I,j),en,w
46	      FORMAT(' Area(',i2,') = ',G13.6,':  number = ',g13.6,
     &	 ':  amplitude = ',G13.6)
	      s=s+area(i,j)*amean(i,j)
	    endif
	   enddo	!end of i=1,ncomp
	   print 431,s
	   if(pon()) write(7,431) s
	   if(discprt) write(8,431) s
431	   format(' Overall mean (ms) = ',g13.6)
c  and print antrue here too when nset>1 (not at label 20)

	   if(.not.excreg) then
		print 113,ANTRUE(j),NFIT(j),ANTRUE(j)*(1.-F1),ANTRUE(j)*F2
		if(discprt) write(8,113) antrue(j),nfit(j),
     &		antrue(j)*(1.0-f1),antrue(j)*f2
	   else
		if(discprt) write(8,114)
     &		ANTRUE(j),NFIT(j),ANTRUE(j)*(1.-F1),
     &		ANTRUE(j)*F2,antrue(j)*f3
		print 114,ANTRUE(j),NFIT(j),ANTRUE(j)*(1.-F1),
     &	ANTRUE(j)*F2,antrue(j)*f3
	   endif
	  enddo	!end of j=1,nset
	  goto 201
	 endif		!end of nset>1 (THIST case)
	endif			!end of thist/obhist
c
c -end of thist
C
10	IF(.NOT.(AHIST.or.idtype.eq.14.or.idtype.eq.52)) GOTO 30
c NB id=14 or 52 for Popen dists
C NOW AMPLITUDE FIT
c (only for nset=1)
	jset=1 	!only for 1 set at present
	j=0
	sa=0.0
	do i=1,ncomp(1)
	   j=j+1
	   amean(i,1)=theta(j)		! mean(i)
	   if(isdcon.eq.3) then
		if(i.eq.1) j=j+1
		sd(i,jset)=theta(2)		!if SD fixed, otherwise redefined
	   else if(isdcon.eq.2) then
		if(i.eq.1) j=j+1
		sd(i,jset)=theta(2)*amean(i,1)	!theta(2)=sdfac in this case
	   else				!isdcon=1
		j=j+1
		sd(i,jset)=theta(j)
	   endif
	   if(i.lt.ncomp(1)) then
		j=j+1
		area(i,1)=theta(j)
		sa=sa+area(i,1)
	   endif
	enddo
	area(ncomp,1)=1.0-sa
c
	call SORT4r(amean(1,1),area(1,1),sd(1,1),ncomp(1),10,.true.)
c
C Calc denominator=PROB(ylow<Y<yhigh)
c or, if range excluded, PROB(ylow<Y<yexclow.or.yexchigh<Y<yhigh)
	jset=1	!only for 1 set now
	st=sqrt(2.0)
	d=0.
	if(excreg) then
	 do i=1,ncomp(1)
	   ulo=(ylow(1)-amean(i,jset))/(sd(i,jset)*st)
	   uhi=(yhigh(1)-amean(i,jset))/(sd(i,jset)*st)
	   uxlo=(yexclow(1)-amean(i,jset))/(sd(i,jset)*st)
	   uxhi=(yexchigh(1)-amean(i,jset))/(sd(i,jset)*st)
	   f1=f1 + area(i,jset)*0.5*(1.0-erfs(ulo))			!prob(obs>ylow)
	   f2=f2 + area(i,jset)*0.5*(1.0-erfs(uhi))			!prob(obs>yhigh)
	   f3=f3 + area(i,jset)*0.5*(erfs(uxhi)-erfs(uxlo))	!prob(yxlo<obs<yxhigh)
	   d=d + area(i,jset)*0.5*(erfs(uhi)-erfs(uxhi)+
     &	erfs(uxlo)-erfs(ulo))
	 enddo
	else		!no excluded region (as earlier version)
	 do i=1,ncomp(1)
	   ulo=(ylow(1)-amean(i,jset))/(sd(i,jset)*st)
	   uhi=(yhigh(1)-amean(i,jset))/(sd(i,jset)*st)
	   f1=f1 + area(i,jset)*0.5*(1.0-erfs(ulo))			!prob(obs>ylow)
	   f2=f2 + area(i,jset)*0.5*(1.0-erfs(uhi))			!prob(obs>yhigh)
c	   d=d + area(i,jset)*0.5*(erfs(uhi)-erfs(ulo))
	 enddo
	endif
	if(usewfreq) then
	   ANTRUE(1)=anfit/(f1-f2-f3)		!anfit calc in MLFITS
	else if(ahist.and.idtype.eq.8) then		!Patlak
	   ANTRUE(1)=anfit/(f1-f2-f3)		!anfit calc in MLFITS
	else
	   ANTRUE(1)=float(nfit(1))/(f1-f2-f3)	!normal calc
	endif

c========debug
	x=f1-f2-f3		!should=d
c=======
c 	DO 1121 J=1,NCOMP
c	X=(YLOW-AMEAN(J))/(SD(J)*SQRT(2.0))
c	F1=F1+AREA(J)*0.5*(1.0-ERFS(X))		!PROB(OBS>YLOW)
c	X=(YHIGH-AMEAN(J))/(SD(J)*SQRT(2.0))
c	F2=F2+AREA(J)*0.5*(1.0-ERFS(X))		!PROB(OBS>YHIGH)
c1121	CONTINUE
c	ANTRUE=FLOAT(NFIT)/(F1-F2)
	IF(ERRORS) GOTO 999
	GOTO 101
C
30	CONTINUE
C NOW OP/BURST FIT (original for nset =1; see above for nset>1)
	Sa=0.		!total area except for last comp
	m=0
	do 32 i=1,ncomp(1)
	m=m+1
	amean(i,jset)=theta(m)
c	rho(i)=1.0 - (1.0/theta(m))
	m=m+1
	if(i.eq.ncomp(1)) goto 32		!last area not a parameter
	area(i,jset)=theta(m)
	sa=sa+area(i,jset)
32	continue
	area(ncomp(1),jset)=1.0 - sa
c
	call SORT2r(amean(1,jset),area(1,jset),ncomp(jset),10,.true.)
c
C   CALC ANTRUE,F1,F2,DENOM=PROB(YLOW<=OBS<=YHIGH)
	DO 1122 i=1,NCOMP(1)
	X=1.0-(1.0/AMEAN(i,jset))		!=RHO(J)
	F1=F1+AREA(i,jset)*(X**(YLOW(1)-1.0))	!PROB(OBS=>YLOW)
	F2=F2+AREA(i,jset)*(X**(YHIGH(1)))	!PROB(OBS>YHIGH)
1122	CONTINUE
	ANTRUE(1)=FLOAT(NFIT(1))/(F1-F2)
	IF(ERRORS) GOTO 999
	GOTO 102
C
C AREA(I) ETC DEFINED. NOW TYPE RESULTS FOR THIST (only for nset=1; when
c nset>1 the printing is done above)
100	CONTINUE
	s=0.0
	DO 40 I=1,NCOMP(1)
	if(pon()) write(7,42) I,AMEAN(I,jset),1000./AMEAN(I,jset)
	if(discprt) write(8,42) I,AMEAN(I,jset),1000./AMEAN(I,jset)
	print 42,I,AMEAN(I,jset),1000./AMEAN(I,jset)
42	FORMAT( ' Tau(',I1,') (ms)= ',G13.6,
     & '     lambda (1/s)= ',G13.6)
	w=area(i,jset)*1000./amean(i,jset)
	en=antrue(1)*area(i,jset)
	if(pon()) write(7,43) I,AREA(I,jset),en,w
	if(discprt) write(8,43) I,AREA(I,jset),en,w
	print 43,I,AREA(I,jset),en,w
c43	FORMAT(' Area(',I1,')= ',G13.6,':  number = ',g13.6,
c     & ':  amplitude (1/s)= ',G13.6)
	s=s+area(i,jset)*amean(i,jset)
40	continue
      print 432,s
      if(pon()) write(7,432) s
      if(discprt) write(8,432) s
432	format(' Overall mean (ms) = ',g13.6)
	GOTO 20		!print ANTRUE
C
101	CONTINUE
C NOW print AHIST RESULTS
	DO 52 I=1,NCOMP(1)
	J=3*I-2		!=1,4,7= INDEX OF MEAN VALUES
	if(discprt) write(8,53) I,AMEAN(I,jset),I,SD(I,jset)
	print 53,I,AMEAN(I,jset),I,SD(I,jset)
53	FORMAT(
     & ' Mean (',I1,') (pA)       = ',G13.6,
     & ' Standard dev(',I1,') (pA)= ',G13.6)
c	DO 57 I=1,NCOMP(1)
	if(pon()) write(7,58) I,AREA(I,jset)
	if(discprt) write(8,58) I,AREA(I,jset)
	print 58,I,AREA(I,jset)
58	FORMAT(
     & ' Area(',I1,')             = ',G13.6)
52	continue
	if(isdcon.eq.2) then
	   if(pon()) write(7,62) theta(2)
	   if(discprt) write(8,62) theta(2)
	   print 62,theta(2)
62	   format(' Constant ratio of SD/mean = ',g13.6)
	endif
	GOTO 20
C
C NOW print OP/BURST FIT.
102	CONTINUE
	s=0.0
	DO 60 I=1,NCOMP(1)
	if(pon()) write(7,61) I,AMEAN(I,jset),1.0-1.0/AMEAN(I,jset)
	if(discprt) write(8,61) I,AMEAN(I,jset),1.0-1.0/AMEAN(I,jset)
	print 61,I,AMEAN(I,jset),1.0-1.0/AMEAN(I,jset)
61	FORMAT(' ''Mean''(',I1,') (ops/bst)= ',G13.6,'.     rho= ',G13.6)
	w=area(i,jset)/amean(i,jset)
	en=antrue(1)*area(i,jset)
	if(pon()) write(7,43) I,AREA(I,jset),en,w
	if(discprt) write(8,43) I,AREA(I,jset),en,w
	print 43,I,AREA(I,jset),en,w
c43	FORMAT(' Area(',I1,')= ',G13.6,':  number = ',g13.6,
c     & ':  amplitude (1/s)= ',G13.6,/)
	s=s+area(i,jset)*amean(i,jset)
c	if(pon()) write(7,63) I,AREA(I,jset),AREA(I,jset)/AMEAN(I,jset)
c	if(discprt) write(8,63) I,AREA(I,jset),AREA(I,jset)/AMEAN(I,jset)
c	print 63,I,AREA(I,jset),AREA(I,jset)/AMEAN(I,jset)
c63	FORMAT( ' Area(',I1,')= ',G13.6,'.       amplitude= ',G13.6)
60	CONTINUE
      print 631,s
      if(pon()) write(7,631) s
      if(discprt) write(8,631) s
631	format(' Overall mean (ops/bst) = ',g13.6)
C
C NOW print NTRUE ETC
20	CONTINUE
	if(.not.excreg) then
	   print 113,ANTRUE(1),NFIT(1),ANTRUE(1)*(1.-F1),ANTRUE(1)*F2
	   if(pon()) write(7,113) ANTRUE(1),NFIT(1),ANTRUE(1)*(1.-F1),
     &	ANTRUE(1)*F2
	   if(discprt) write(8,113) ANTRUE(1),NFIT(1),ANTRUE(1)*(1.-F1),
     &	ANTRUE(1)*F2
113	   FORMAT(' Predicted true number of events= ',g13.6,/,
     &    ' = No fitted (',i6,') +no below Ylow = ',g13.6,
     &    ' + no above Yhigh = ',g13.6,/)
	else
	   if(pon()) write(7,114) ANTRUE(1),NFIT(1),ANTRUE(1)*(1.-F1),
     &	ANTRUE(1)*F2,antrue(1)*f3
	   if(discprt) write(8,114) ANTRUE(1),NFIT(1),ANTRUE(1)*(1.-F1),
     &	ANTRUE(1)*F2,antrue(1)*f3
	   print 114,ANTRUE(1),NFIT(1),ANTRUE(1)*(1.-F1),
     &	ANTRUE(1)*F2,antrue(1)*f3
114	   FORMAT( ' Predicted true number of events= ',G13.6,/
     & ,' =No fitted(',I5,') +no below Ylow= ',G13.6,
     & ' +no above Yhigh= ',G13.6,/,' +no excluded = ',g13.6,/)
	endif
c
c Print all tcrits for shut time distribution (idtype=1, or, for all
c shut times after jump, idtype=103)
201	if(ghist.and.(idtype.eq.1.or.idtype.eq.2.or.idtype.eq.103)) then
c keep the shut time pdf for use by TCRITS when burst requested
	  do j=1,nset
	   ncomps(j)=ncomp(j)
	   do m=1,ncomp(j)
		ameans(m,j)=amean(m,j)
		areas(m,j)=area(m,j)
	   enddo
	   call DCASK(' Print all tcrit values','Y',ans)
	   if(ans.eq.'Y') then
		kth=0		!print all
		call TCRITS(amean(1,j),area(1,j),ncomp(j),kth,.false.)
	   endif
	  enddo	!end of j=1, nset
	endif
c Print all Acrits for amplitude distribution
	if(ahist) then
c keep the amplitude pdf for use by TCRITS when burst requested
c NB nset=1 for amp histos
	   j=1
	   ncompa=ncomp(j)
	   do m=1,ncomp(j)
		ameana(m)=amean(m,j)
		areaa(m)=area(m,j)
		sda(m)=sd(m,j)
	   enddo
	   call DCASK(' Print all Acrit values','Y',ans)
	   if(ans.eq.'Y') then
		kth=0		!print all
		call ACRITS(amean(1,j),area(1,j),sd(1,j),ncomp,kth,.false.)
	   endif
c Do the how calculation so warning can be printed about duration of
c (1) opening, to ensure it is long enough that it is full not sub
c () shutting, to ensure it is long enough that it is zero not sub
	   if(ncomp(1).gt.1) then
		jset=1
		call AAMAX(area,amean,sd,ncomp,jset)
	   endif
	endif
c
c
c Print estimate of prob[true null sweep] for latencies
c Total number of sweeps in which latency measured=nfit
c Total number of null sweeps in data=nnull, but nfitt=number to be used
c for estimation purposes
	if(idtype.eq.101) then
	   j=jset
	   Ftmax=0.0
	   do i=1,ncomp(j)
		Ftmax=Ftmax + area(i,jset)*exp1(-yhigh/amean(i,jset))	!prob t>tmax
	   enddo
	   Ftmax=1.0 - Ftmax		!prob t<tmax
	   N=nfit(j)+nfitt			!observed latencies + observed nulls
	   p1obs=float(nfit(j))/float(N)	!obs fraction of measured latencies
	   p0obs=1.0 - p1obs         	!obs fraction of nulls
	   p1=p1obs/Ftmax			!estimated true fraction that open eventually
	   pnull=1.0-p1			!estimated fraction of true nulls
	   pfalse=p1*(1.0-Ftmax)	!estimated fraction of false nulls
	   en=float(N)
         print 21,nfit(j),p1obs*100.,nfitt,p0obs*100.,
     &    N,pnull,en*pnull,p1,p1*en,pfalse*en
         if(discprt) write(8,21) nfit(j),p1obs*100.,nfitt,p0obs*100.,
     &    N,pnull,en*pnull,p1,p1*en,pfalse*en
21	   format(
     &   ' Number of observed latencies   = ',i6,' (',f8.2,'%)',/,
     &   ' Number of observed null sweeps = ',i6,' (',f8.2,'%)',/,
     &   ' Total number                   = ',i6,/,
     &   ' Estimated Prob[true null]      = ',f7.4,' (n = ',f9.1,')',/,
     &   ' Estimated Prob[non-null]       = ',f7.4,' (n = ',f9.1,')',/,
     &   ' Estimated number of false nulls= ',g11.4)
c     &   ' Estimated number of true nulls = ',g11.4,' (',f8.2,'%)',/,
c     &   ' Estimated number of non-nulls  = ',g11.4,' (',f8.2,'%)',/,
c     &    N,pnull*en,pnull*100.,p1*en,p1*100.,pfalse*en
	endif
C
999	CONTINUE
	call flush(7)
	RETURN
	END


