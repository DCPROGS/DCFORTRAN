c==================================================================
	program testa
c==================================================================

      call GINO
	call VGA
	call gsetcols(0)
	call mode(18)

	call papenq(xp,yp,ipap)
	r=10.
	do i=1,16
	   call lincol(i-1)
	   call movto2(10.,r)
	   call linto2(100.,r)
	   r=r+5.
	enddo
      call printopt(45.,230.,39.,167.,280.,196.,1.1)
	pause
	call devend
	   ICGMDV=14
	   OPEN(UNIT=ICGMDV,FILE='new.cgm',STATUS='UNKNOWN')
	   CALL CGMBi
	   CALL DEVICE(ICGMDV,0)
	   call rgbdef(0,0.0,0.0,0.0)
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call mode(18)
	   idev=2
	r=10.
	do i=1,16
	   call lincol(i-1)
	   call movto2(10.,r)
	   call linto2(100.,r)
	   r=r+5.
	enddo
	call devend
	call ginend
	call mode(3)
	end
