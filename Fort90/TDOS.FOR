	program TDOS
	character*40 file,filout
c
	n=40
	file='c:\dir\   12345.6789'
	call TITENT0('file name:',file,n,.false.)
c	call DOSFILE(file,n,filout)
c	print 1,filout
	call DOSFILE(file,n,file,nerr)
	print 1,file,nerr
1	format(1x,a40,/,' nerr = ',i2)
	end

