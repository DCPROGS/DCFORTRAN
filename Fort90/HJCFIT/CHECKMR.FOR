	subroutine CHECKMR(QD)
	real*8 QD(100,100),r1,r2,err
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
	character*25 text
	common/deb2/idebug2	!for hjcfit to control printing
	logical discprt
	common/dp/discprt
c
c Modified 07/05/06 09:50am so this is version for HJCFIT, in local
c hjcfit directory (not same as checkmr in \calc)
c
c  Modif 09/26/03 10:23am so that cycles are printed in the order they
c are set, and labelled as automr where appropriate
c
	if(ncyc.eq.0) RETURN
c
	if(idebug2.eq.1) then
	   print 2
	else if(idebug2.eq.2) then
	   print 2
	   if(discprt) write(8,2)
	endif
2	format(/,' Check microscopic reversibility',/,
     & 12x,'          products of rates:',/,
     & 12x'  one way round       other way round         error (%)')
	do m1=1,ncyc
	   m=isetmr(m1)	!cycle number
c
         r1=1.d0
	   r2=1.d0
	   do j=1,nsc(m)
		i1=im(m,j)
		j1=jm(m,j)
		r1=r1*QD(i1,j1)	!product one way round
		r2=r2*QD(j1,i1)	!product other way round
	   enddo
	   if(obeymr(m).and.(.not.automr(m))) then
		text='(set to obey MR)    '
	   else if(automr(m)) then
		text='(obeys MR automatically)'
	   else
		text='(not set to obey MR)'
	   endif
	   if(r1.gt.1.d-20) then
		err=100.d0*(r2-r1)/r1
	   else
		err=r2-r1
	   endif
	   if(idebug2.eq.1) then
		print 1,m1,m,r1,r2,err,text
	   else if(idebug2.eq.2) then
		print 1,m1,m,r1,r2,err,text
		if(discprt) write(8,1) m1,m,r1,r2,err,text
	   endif
1	   format(' (',i3,
     &   ':) Cycle ',i3,':  r1 = ',g13.6,'   r2 = ',g13.6,2x,f9.6,
     &       1x,a25)
	enddo
	RETURN
	end

