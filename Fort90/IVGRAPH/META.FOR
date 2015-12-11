	program meta
	integer*4 ispace,dskspc
	call gino
	ispace=dskspc('C:')
	print*,'ispace=',ispace
	pause
	ICGMDV=14
	OPEN(UNIT=ICGMDV,FILE='meta1.cgm',STATUS='UNKNOWN')
	CALL CGMBi
	CALL DEVICE(ICGMDV,0)
	call rgbdef(0,0.0,0.0,0.0)
	call errswi(-1)
	call brkswi(1)
	call gsetcols(0)
	y=10.
	call lincols(0)
	call  movto2(10.,10.)
	call chafnt('start')
	do i=1,15
		call lincols(i)
		call movto2(10.,y)
		if(i.eq.1) then
		call linto2(100.,y)
		else
		call linto2(200.,y)
		endif
		y=y+10.
	enddo
	CLOSE(UNIT=ICGMDV)
	CALL DEVEND
	ispace=dskspc('C:')
	print*,'ispace=',ispace
	pause

	end
