	program TT
	character inifile*40

	inifile="test.ini"
	OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=1024)
	i=10
	write(19,rec=1) i
	CLOSE(unit=19)
	call SYSTEM("copy "//inifile//", test0.ini")
	end

