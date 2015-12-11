	program TCHKFILE
c To test CHKFILE
	character filnam*30
c
	filnam='                               '
1	pause
	call TITENT0('File name = ',filnam,30,.false.)
	call CHKFILE(filnam,nerr,30)
	print 2,nerr
2	format(' nerr = ',i3)
	goto 1
	end


