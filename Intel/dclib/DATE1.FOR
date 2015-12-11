	subroutine DATE1(DATE,TIME)
	character DATE*11,TIME*8,RDATE*8,RTIME*10,ZONE*5
	INTEGER*4 VALUES(8)

	CALL DATE_AND_TIME(RDATE,RTIME,ZONE,VALUES)

	date(1:2)=rdate(7:8)		!day
	if(rdate(5:6).eq.'01') date(3:7)=' Jan '
	if(rdate(5:6).eq.'02') date(3:7)=' Feb '
	if(rdate(5:6).eq.'03') date(3:7)=' Mar '
	if(rdate(5:6).eq.'04') date(3:7)=' Apr '
	if(rdate(5:6).eq.'05') date(3:7)=' May '
	if(rdate(5:6).eq.'06') date(3:7)=' Jun '
	if(rdate(5:6).eq.'07') date(3:7)=' Jul '
	if(rdate(5:6).eq.'08') date(3:7)=' Aug '
	if(rdate(5:6).eq.'09') date(3:7)=' Sep '
	if(rdate(5:6).eq.'10') date(3:7)=' Oct '
	if(rdate(5:6).eq.'11') date(3:7)=' Nov '
	if(rdate(5:6).eq.'12') date(3:7)=' Dec '
	date(8:11)=rdate(1:4)		!year
	time(1:3)=rtime(1:2)//':'
	time(4:6)=rtime(3:4)//':'
	time(7:8)=rtime(5:6)
	RETURN
	end
