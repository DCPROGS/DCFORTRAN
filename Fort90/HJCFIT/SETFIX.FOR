	subroutine SETFIX(readini,imod0,imodsav,npar,theta0,titlep,
     & QT,IQ,obeymr,nfix,jfix,km,useprim)
c To set fixed rates in hjcfit (called from main prog, or from getqd1 if
c spanning tree used)
c Modif 12/09/03 03:44pm to ask for i,j rather than parameter number
c whether useprim or not
	real*4 xs
	character*10 titlep(200)
	real*8 theta0(200)
	character*1 ans
	integer*4 jfix(200)
	allocatable::msav
	integer*4 msav(:)
	integer*4 irate(200),jrate(200)
	common/q0/irate,jrate		!for modec50
	real*8 QT(100,100)
	integer*4 IQ(100,100)
	logical obeymr(50),useprim,readini,allocated
	logical discprt
	common/dp/discprt
c needed for checks
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
	integer NSC(50),IM(50,100),JM(50,100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	logical fixec50,prtec50,dcmodel
	real*8 ec501,xqlo,xqhi
	common/ec/fixec50,nmod,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel
c    nmod=imod0 (already in commom/model)
	real*8 ec502,xqlo2,xqhi2
	real*4 conc_ec1(10),conc_ec2(10)
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
c
	if(readini.and.imod0.eq.imodsav) then
	  if(nfix.gt.0) then
	   print 224
c224	   format(' The following parameters were fixed during fit')
	   do m=1,npar
	    if(jfix(m).eq.1) then
		i=irate(m)
		j=jrate(m)
		if(useprim) then		!theta0 not yet defined
		   theta0(m)=QT(irate(m),jrate(m))
		endif
       	print 121,m,i,j,titlep(m),theta0(m)
121		format(i3,3x,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	    endif
	   enddo
	   print 1161
1161	   format(' Are these O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.ne.'N') then	!print values
	    goto 225	!jfix already done
	   endif
	  endif
	endif

1162	continue
c Store rates that were fixed before as defaults
	allocate(msav(200))
	n=0
	do m=1,npar
	   if(jfix(m).eq.1) then
		n=n+1
		msav(n)=m		!parameter # m is fixed
	   endif
	enddo
	nfix0=n
	print 116,nfix
116	FORMAT(/,
     & ' Number of parameters to be fixed during fit [',i2,'] = ')
	call INPUTi(nfix)
c
c  Zero jfix
	do i=1,km
	   jfix(i)=0		!in case old jfix read from disc
	enddo
c
	if(nfix.gt.0) then
	   do n=1,nfix
		if(n.le.nfix0) then	!set default
		   m=msav(n)
		   i=irate(m)
		   j=jrate(m)
		endif
c Ask for route rather than parameter # in span tree method
c Now always asks for route
		   print 215,N,i,j
215	 	   format('    ',i2,
     & ': specify state numbers for rate to be fixed: i, j ['
     &     i2,',',i2,'] = ')
		   call INPUT2i(i,j)
		   m=IQ(i,j)
c Before accepting, check whether this rate is already constrained
		do L=1,neq
		   i1=ie(L)
		   j1=je(L)
		   if(i.eq.i1.and.j.eq.j1) then 	!q(i,j) already constrained
			m=IQ(i,j)
			print 10,titlep(m),i,j,efac(L),if(L),jf(L)
	      	if(discprt) write(8,10)titlep(m),i,j,
     &			efac(L),if(L),jf(L)
10			format(' ERROR: ',a10,' = Rate q(',i2,',',i2,
     &	 ') is already  constrained',/,' (to be ',f7.1,
     &	 ' times q(',i2,',',i2,'))',/,' Try again')
			call BELL(2)
			pause
			if(allocated(msav)) deallocate(msav)
			goto 1162
		   else if(i.eq.if(L).and.j.eq.jf(L)) then
			m=IQ(i,j)
			print 11,titlep(m),i,j,ie(L),je(L)
	      	if(discprt) write(8,11)titlep(m),i,j,ie(L),je(L)
11			format(' NOTE: ',a10,' = Rate q(',i2,',',i2,
     &  ') is used to constrain q(',i2,',',i2,')) which ',/,
     &  ' is therefore also fixed')
			call BELL(1)
			pause
		   endif
		enddo
c
c check whether this rate is already set by MR
		do L=1,ncyc
		   if(obeymr(L)) then
 			i1=im(L,1)
			j1=jm(L,1)
			if(i.eq.i1.and.j.eq.j1) then 	!q(i,j) already set by MR
			   m=IQ(i,j)
			   print 12,titlep(m),i,j
	      	   if(discprt) write(8,12)titlep(m),i,j
12			format(' ERROR: ',a10,' = Rate q(',i2,',',i2,
     &	 ') is already  set by micro rev:',/,
     &	 ' Try again')
			   call BELL(2)
			   pause
			   if(allocated(msav)) deallocate(msav)
			   goto 1162
			endif
		   endif
		enddo
c
c check whether this rate is already set by EC50
		do L=1,nfixec50
		   m=IQ(i,j)
		   if((i.eq.i50.and.j.eq.j50).or.
     &		 (L.eq.2.and.i.eq.i502.and.j.eq.j502)) then
			print 13,titlep(m),i,j
	      	if(discprt) write(8,13)titlep(m),i,j
13			format(' ERROR: ',a10,' = Rate q(',i2,',',i2,
     &	 ') is fixed by specified EC50: try again')
			call BELL(2)
			pause
			if(allocated(msav)) deallocate(msav)
			goto 1162
		   endif
		enddo
c
c Confirm value (for useprim, theta0 not yet defined, so define it from QT)
		if(useprim) then
		   theta0(m)=QT(irate(m),jrate(m))
		endif
		if(readini.and.imod0.eq.imodsav) then
		   print 14,m,titlep(m),i,j,theta0(m)
14		   format('&',' Parameter ',i2,':',a10,' = q(',i2,',',i2,
     &		') to be fixed at ',g13.6,' O.K. [Y] ? ')
		   ans='Y'
		else
		   print 141,m,titlep(m),i,j,theta0(m)
141		   format('&',' Parameter ',i2,':',a10,' = q(',i2,',',i2,
     &		') to be fixed at ',g13.6,' O.K. [N] ? ')
		   ans='N'
		endif
		call INPUTa(ans)
		if(ans.eq.'N') then
		   iopt=2
		   print 1192,titlep(m),iopt
1192		   format(/,
     &	' (1) Change which parameters are to be fixed',/,
     &	' (2) Change the value at which ',a10,'is to be fixed',/,
     &	' Option number [',i2,'] = ')
		   call INPUTi(iopt)
		   if(iopt.eq.1) then
			if(allocated(msav)) deallocate(msav)
			goto 1162
		   else
			xs=sngl(theta0(m))
			print 1193,titlep(m),xs
1193			format(' Fixed value for ',a10,' [',g13.6,'] = ')
			call INPUTr(xs)
			theta0(m)=dble(xs)
			QT(irate(m),jrate(m))=theta0(m)
		   endif
		endif
		JFIX(m)=1		!ELEMENT OF JFIX=1 IF PARAM FIXED
	   enddo	!end of do n=1,nfix
	endif
	deallocate(msav)
c
225	continue
c
c Print the fixed values
	if(nfix.gt.0) then
         if(discprt) write(8,224)
224	   format(/,' The following parameters are fixed:')
	   do m=1,npar
		if(jfix(m).eq.1) then
	 	   i=irate(m)
		   j=jrate(m)
		   if(discprt) write(8,121) m,i,j,titlep(m),theta0(m)
c121		   format(i3,3x,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
		endif
	   enddo
	   if(discprt) write(8,108)
108	   format(/)
	else
         if(discprt) write(8,161)
161	   format(/,' No parameters fixed',/)
	endif
c
	RETURN
	end

