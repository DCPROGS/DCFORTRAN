	subroutine ACONVd(amp,eigen,ncomp,tres,discprt,km)
c To renormalise areas that are defined from t=tres to infinity, to range
c t=0 to infinity, and scale to sum to 1.
c Expects EIGEN to contain negative values
	use menu_f90
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 eigen(km),amp(km)
	allocatable::area
	real*8 area(:)
	real*8 dexp1
	logical discprt
	common/tty/ittypanel,itty
	character string*256
c
	allocate(area(ncomp))
	atot=0.0d0
	do 20 m=1,ncomp
	rate=-eigen(m)		!positive rate constant
	if(rate.ge.1.d-30) then
	   area(m)=amp(m)/rate
	   area(m)=area(m)*dexp1(rate*tres)	!scale area
	   atot=atot+area(m)
	else
	   area(m)=-1.d6		!error signal
	endif
20	continue
c
c      print 22
      if(discprt) write(7,22)
c	CALL GMSETTEXTSETTING(ITTY,
c     &' Areas for asymptotic pdf renormalised for t=0 to infinity')
22	format(
     &' Areas for asymptotic pdf renormalised for t=0 to infinity',
     &' (and sum=1)',/,
     &' (so areas can be compared with ideal pdf)')
	do 23 m=1,ncomp
	  area(m)=area(m)/atot
c        print 24,m,area(m)
        if(discprt) write(7,24) m,area(m)
	   write(string,fmt='(i4,g13.6)') m,area(m)
     
cc	  CALL GMSETTEXTSETTING(ITTY,string)
24	  format(i3,4x,g13.6)
23	continue
c      print 2
      if(discprt) write(7,2)
2	format(/)
	deallocate(area)
	RETURN
	end


