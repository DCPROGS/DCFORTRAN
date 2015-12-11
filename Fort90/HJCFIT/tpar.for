	program tpar
	PARAMETER(kAmx=3)
	real*8 array(kAmx,6)
	common/d1/array
c
	do i=1,kAmx
	   do j=1,6
		array(i,j)=dfloat(i+j)
	   enddo
	enddo
	call SUB()
	call SUB1(array,kAmx)
	end

	subroutine SUB()
	PARAMETER(kAmx=3)
	real*8 array
	common/d1/array(kAmx,6)
	print*, array(3,6)
	return
	end

	subroutine SUB1(array,kAmx)
	real*8 array(kAmx,6)
	print*, array(3,6)
	return
	end

	subroutine SUB2(array,kAmx)
	PARAMETER(k=5)
	real*8 array(kAmx,6)
	real*8 a1
	common/ablk/a1(k,3)
	print*, array(3,6)
	return
	end


	subroutine SUB3(kAmx)
c This one is ilegal -cannot pass the dimension as parameter when
c array is in common
	real*8 array(kAmx,6)
	common/d1/array(kAmx,6)
	print*, array(3,6)
	return
	end
