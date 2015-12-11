	real*4 af(3,2,5)
c NB next line does not work -the DECLARED dimensions must be passed to subroutime
c (apart from 1D arrays)
c	real*4 af(10,10,10)
c	common/ablk/af
	common/kmax/kAm,kFm

	kAm=3
	kFm=2
	do i=1,kAm
	   do j=1,kFm
		do m=1,kAm+kFm
		   af(i,j,m)=float(i+j+m)
		enddo
	   enddo
	enddo
c	call SUB()
	call SUB(af)
c	call SUB(kAm,kFm)
	end

	subroutine SUB(af)
c	subroutine SUB(kAm,kFm)
	common/kmax/kAm,kFm
c	common/ablk/af
c
	real*4 af(kAm,kFm,kAm+kFm)
c	real*4 af
c	common/ablk/af(kAm,kFm)
c
	print*, af(kAm,kFm,kAm+kFm)
	return
	end


