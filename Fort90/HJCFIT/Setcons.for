	subroutine SETCONS(readini,imod0,imodsav,titlep,IQ,jcon,
     &	theta0,obeymr,chkmr,npar,useprim,QT,km)
c
c Subroutine for HJCFIT to set constraints -called from main prog when
c ordering method used for MR but called from GETQD1 when spanning tree used
c CHKMR=true if requested constraints are to be checked to see if they
c		are already set my MR (as in original approach when MR set before
c		constraints)
c CHKMR=false with spanning tree method for which MR is not yet set when
c	 constraints are applied, but constraints are assured to be in tree
c	 (not MR) when MR is set later
c
c Modif 07/07/04 07:31am by adding useprim and QT to parameters, to provide
c default theta0 when useprim=true
c
	logical readini,chkmr
	logical obeymr(50),useprim
	character*10 titlep(200)
	integer IQ(100,100),jcon(200),irate(200),jrate(200)
	real*8 theta0(200)
	real*8 QT(100,100)
	logical discprt
	common/dp/discprt
	character*1 ans
	common/q0/irate,jrate		!for modec50
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
	integer NSC(50),IM(50,100),JM(50,100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	integer isetmr(50)
	common/mr1/isetmr

c For useprim, theta0 not yet defined, so define it from QT)
	if(readini.and.imod0.eq.imodsav) then
	  if(neq.gt.0) then
	   print 324,neq
324	   format(/,' The following ',i3,' parameters are constrained:')
	   do n=1,neq
		i=ie(n)
		j=je(n)
		m=IQ(i,j)
c		jcon(m)=1	!define jcon (already read from init file)
		i1=if(n)
		j1=jf(n)
		m1=IQ(i1,j1)
		if(useprim) then
		   theta0(m1)=QT(irate(m1),jrate(m1))
		endif
		if(efac(n).gt.0.) then
       	   print 321,m,i,j,titlep(m),efac(n),
     &	    m1,i1,j1,titlep(m1),theta0(m1)
321	 	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' times rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
		else
       	   print 322,m,i,j,titlep(m),-efac(n),
     &	    m1,i1,j1,titlep(m1),theta0(m1)
322	 	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' minus rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
		endif
	   enddo
	   print 1161,neq 		!OK?
1161	   format(' Are these ',i3,' constraints O.K [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.ne.'N') goto 212	!jfix already done
	  endif	!end of neq.ne.0
	else		!not readini
	   neq=0
	   do i=1,npar
		jcon(i)=0
		efac(i)=0.0
	   enddo
	endif
	neq1=0
	do i=1,neq
	   if(efac(i).lt.0.) neq1=neq1+1	!default
	enddo

2121	continue
      print 138
      if(discprt) write(8,138)
138	format(
     &' ------------------------------------------------------------')
	call BELL(1)
	print 216,neq
216	format('  DEFINE CONSTRAINTS',/,
     &  ' Number of rate constants to be constrained [',i3,'] = ')
	call INPUTi(neq)
	ans='N'
	do i=1,km		!km=100 at present
	   jcon(i)=0		!in case old jcon read from disc
	enddo
	if(neq.eq.0) goto 212
c
	if(neq1.gt.0) ans='Y'
	print 2161,ans
2161	format(
     &  '&Constrain any of these rates to add to fixed total [',
     &   a1,'] ? ')
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   print 2162,neq1
2162	   format(
     &  '&  Number of additive constraints [',i2,'] = ')
	   call INPUTi(neq1)
	endif
	neq0=neq-neq1	!number of multiplicative constraints
c	 do n=1,neq
	 do n=1,neq0
c	    print 217,N
c217	    format(' #',i2,' . Rate #n1 = factor times rate #n2',/,
c     & '   Specify: n1, n2 = ')
c	    call INPUT2i(n1,n2)
c	    ie(n)=irate(n1)
c	    je(n)=jrate(n1)
c	    if(n)=irate(n2)
c	    jf(n)=jrate(n2)
	    i=ie(n)		!default
	    j=je(n)
	    print 215,N,ie(n),je(n)
215	    format(' #',i2,'. Rate q(i,j) = factor times rate q(i1,j1)',/,
     & '   specify state numbers for rate to be constrained: i, j ['
     &     i2,',',i2,'] = ')
	    call INPUT2i(i,j)
	    ie(n)=i
	    je(n)=j
c
	    i1=if(n)		!default
	    j1=jf(n)
	    print 217,if(n),jf(n)
217	    format(
     & '   specify states for rate from which it is found: i1, j1 ['
     &     i2,',',i2,'] = ')
	    call INPUT2i(i1,j1)
	    if(n)=i1
	    jf(n)=j1
c Check that constrained parameter, q(ie,je), is not one of the params
c calc by micro rev -ONLY if chkmr=true
	   if(ncyc.gt.0.and.CHKMR) then
		do m1=1,ncyc
		   m=isetmr(m1)	!actual cycle number
		   if(obeymr(m)) then
			if(ie(n).eq.im(m,1).and.je(n).eq.jm(m,1)) then
			   call BELL(1)
			   print 221,ie(n),je(n),m
221			   format(
     & ' Element ',i2,',',i2,' is fixed by microscopic reversibility',/,
     & ' (cycle #',i3,'). Do not constrain it! Start again.')
			   goto 2121
		      endif
		   endif
		enddo
	   endif
c Get the factor
	   i=ie(n)
	   j=je(n)
	   m=IQ(i,j)
	   i1=if(n)
	   j1=jf(n)
	   m1=IQ(i1,j1)
     	   print 218,m,i,j,titlep(m),
     &	 m1,i1,j1,titlep(m1),theta0(m1),efac(n)
218	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',/,
     &' factor times rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6,
     & /,' Factor [',f8.2,'] = ')
	   call INPUTr(efac(n))
c   Check and print the constrained param OK
	   jcon(m)=1	!define jcon
         print 321,m,i,j,titlep(m),efac(n),
     &    m1,i1,j1,titlep(m1),theta0(m1)
c321	   format(' rate ',
c     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
c     &/,g13.6,' times rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
	   print 1163
1163	   format(' Is this O.K [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') goto 2121
	enddo
	if(neq1.eq.0) goto 212
c Now get additive constraints
2122	do n=neq0+1,neq
325	    print 2171,N
2171	    format(' #',i2,' . Rate #n1 = total minus rate #n2',/,
     & '   Specify: n1, n2 = ')
	    call INPUT2i(n1,n2)
	    ie(n)=irate(n1)
	    je(n)=jrate(n1)
	    if(n)=irate(n2)
	    jf(n)=jrate(n2)
c Check that constrained parameter, q(ie,je), is not one of the params
c calc by micro rev
	   if(ncyc.gt.0.and.CHKMR) then
		do m1=1,ncyc
		   m=isetmr(m1)	!actual cycle number
		   if(obeymr(m)) then
			if(ie(n).eq.im(m,1).and.je(n).eq.jm(m,1)) then
			   call BELL(1)
			   print 221,ie(n),je(n),m
c221			   format(
c     & ' Element ',i2,',',i2,' is fixed by microscopic reversibility',/,
c     & ' (cycle #',i3,'). Do not constrain it! Start again.')
			   goto 2122
			endif
		   endif
		enddo
	   endif
c Get the total
	   i=ie(n)
	   j=je(n)
	   m=IQ(i,j)
	   i1=if(n)
	   j1=jf(n)
	   m1=IQ(i1,j1)
     	   print 2181,m,i,j,titlep(m),
     &	 m1,i1,j1,titlep(m1),theta0(m1)
2181	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',/,
     &' total minus rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6,
     & /,' Total rate = ')
	   call INPUTr(efac(n))
	   if(efac(n).le.theta0(m1)) then
		call BELL(2)
		print 323,i1,j1,titlep(m1),theta0(m1)
323		format(
     &	' ERROR: total rate must be bigger than q(',
     & 	i2,',',i2,')=',1x,a10,2x,g13.6)
		goto 325
	   endif
c
	   efac(n)=-efac(n)	!negative as sign this is total
c
c   Check and print the constrained param OK
	   jcon(m)=1	!define jcon
         print 322,m,i,j,titlep(m),-efac(n),
     &     m1,i1,j1,titlep(m1),theta0(m1)
c322	   format(' rate ',
c     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
c     &/,g13.6,' minus rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
	   print 1163
c1163	   format(' Is this O.K [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') goto 2122
	enddo
c
212	continue
c Print the values here
	if(neq.gt.0) then
	  if(discprt) write(8,324) neq
c324	   format(/,' The following ',i3,' parameters are constrained:')
	  do n=1,neq
		i=ie(n)
		j=je(n)
		m=IQ(i,j)
		i1=if(n)
		j1=jf(n)
		m1=IQ(i1,j1)
		if(efac(n).gt.0.) then
       	   if(discprt) write(8,222) m,i,j,titlep(m),efac(n),
     &	     m1,i1,j1,titlep(m1),theta0(m1)
222	 	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' times rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
		else
       	   if(discprt) write(8,223) m,i,j,titlep(m),-efac(n),
     &	     m1,i1,j1,titlep(m1),theta0(m1)
223	 	   format(' rate ',
     &i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,'is constrained to be',
     &/,g13.6,' minus rate ',i3,3x,' q(',i2,',',i2,')=',1x,a10,2x,g13.6)
		endif
	   enddo
	else
         if(discprt) write(8,162)
162	   format(' No parameters constrained')
	endif
c
	RETURN
	end

