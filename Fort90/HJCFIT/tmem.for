	program TMEM
c Is memory used in a subroutine recovered when the subroutine is exited?
c	real*8 array(200,200,200)
	real*8 array(2,2,2)
c
	pause 'in main '
	call SUB0(array)
	pause 'in main '
	call SUB1(array)
	pause 'back to main'
	call SUB2(array)
	pause 'back to main'
	call SUB3(array)
	pause 'back to main'
	call SUB0(array)
	pause 'in main '
	call SUB1(array)
	pause 'back to main'
	end

	subroutine SUB0(a)
	real*8 a0(100,100,100)
	real*8 a(2,2,2)
	print*, a0(1,1,1)
	pause 'in sub0'
	RETURN
	end


	subroutine SUB1(a)
	real*8 a1(100,100,100)
	real*8 a(2,2,2)
	print*, a1(1,1,1)
	pause 'in sub1'
	RETURN
	end

	subroutine SUB2(a)
	real*8 a2(100,100,100)
	real*8 a(2,2,2)
	print*, a2(1,1,1)
	pause 'in sub2'
	RETURN
	end

	subroutine SUB3(a)
	real*8 a3(100,100,100)
	real*8 a(2,2,2)
	print*, a3(1,1,1)
	pause 'in sub3'
	RETURN
	end

