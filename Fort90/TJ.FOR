c to test RJUSTIF, CJUSTIF
	character string*20
c
1	call TITENT0('string=',string,20,.true.)
	print*,string
c	call RJUSTIF(string,20)
c	print*,string
	call CJUSTIF(string,20)
	print*,string
	goto 1
	end
