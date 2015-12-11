	subroutine MRTEST(Q,pinf,subname,ifcall,k,kdim)
c To test whether each reaction step separately obeys the microscopic rev
c criterion given QD and the corresponding pinf().
c
c subname=string that contains name of subroutine that MRTEST was called from
c ifcall=integer that controls printing
c	ifcall=0 full printout of any breach of mic rev
c	ifcall=1 no printout at all (needed eg if mrtest called within function)
c	ifcall=2 prints only number of failures
c
	real*8 Q(kdim,kdim),pinf(kdim)
	real*8 S,S1,diff,crit
	character subname*8
	logical fail
	logical discprt
	common/dp/discprt
c
	nfail=0	!no failures found
	do i=1,k
	   do j=i,k
		S=PINF(I)*Q(I,J)
		S1=PINF(J)*Q(J,I)
		diff=dabs(s-s1)	!use percent criterion unless den=0
		crit=1.0d0		!if denom=0 so not defined
		if(s1.lt.1.d-10) goto 40
		crit=dabs(diff/s1)
c		fail=crit.gt.3.d-3	!0.3 percent difference
		fail=crit.gt.1.5d-2	!1.5 percent difference (Q is still real*4)
		if(fail) nfail=nfail+1		!at least one failure
		if(crit.gt.crtmax) crtmax=crit
		goto 41
40		fail=diff.gt.1.d-10
41		if(fail.and.ifcall.eq.0) then
		   print 107,subname,I,J,PINF(I),Q(I,J),
     & 		S,PINF(J),Q(J,I),S1,diff,100.*crit
		   if(discprt) write(8,107) subname,I,J,PINF(I),Q(I,J),
     & 		S,PINF(J),Q(J,I),S1,diff,100.*crit
		endif
107		FORMAT(
     &      ' **MICRO REV FAILURE in subroutine ',a8,' for i,j= ',2I3,/,
     &	 ' p(i),Q(i,j),p(i)*Q(i,j)',/,3(2X,e15.8),/,
     &	 ' p(j),Q(j,i),p(j)*Q(j,i)',/,3(2X,e15.8),/,
     &	 ' difference= ',g13.6,'  PERCENT diff= ',g13.6)
	   enddo
	enddo
	if(nfail.gt.0.and.ifcall.eq.2) then
	   print 10,nfail,subname
	   if(discprt) write(8,10) nfail,subname
10	   format(
     &	' Microscopic reversibility failed ',i6,' times in ',a8)
c	else
c	   print 11,subname
c	   if(discprt) write(8,11) subname
c11	   format(
c     &	' Microscopic reversibility check passed in ',a8)
	endif
	RETURN
	end

