	program TMEMA
c Is memory used in a subroutine recovered when the subroutine is exited?
c	real*8 array(200,200,200)
	real*8 array(2,2,2)
c
	call SUBa(array)
	pause 'back to main -now subb'
	call SUBb(array)
	pause 'back to main'
	call SUBa(array)
	pause 'back to main -now subb'
	call SUBb(array)
	pause 'back to main'
	call SUBa(array)
	pause 'back to main -now subb'
	call SUBb(array)
	pause 'back to main'
	call SUBa(array)
	pause 'back to main -now subb'
	call SUBb(array)
	pause 'back to main'
	end

	subroutine SUBa(a)
	allocatable::array
	real*8 array(:,:,:)
	real*8 a(2,2,2)
	pause 'in suba -next allocate array'
	allocate (array(100,100,100))
	pause 'in suba -array allocated'
	print*, array(1,1,1)
	deallocate(array)
	pause 'in suba -array deallocated'
	RETURN
	end

	subroutine SUBb(a)
	allocatable::ab
	real*8 ab(:,:,:)
	real*8 a(2,2,2)
	pause 'in sub3 -next allocate array'
	allocate (ab(100,100,100))
	pause 'in sub3 -array bow allocated'
	print*, ab(1,1,1)
	deallocate(ab)
	pause 'in sub3 -array deallocated'
	RETURN
	end
