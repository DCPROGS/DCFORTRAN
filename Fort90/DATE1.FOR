	subroutine DATE1(cdate)
	character*11 cdate
	character*8 cdate1
c To convert Lahey DATE output to nicer format
	call DATE(cdate1)
c	print 2,cdate1
c2	format(' cdate1=',a8)
	cdate(1:2)=cdate1(4:5)		!day
	if(cdate(1:1).eq.'0') cdate(1:1)=' '
	cdate(8:9)='20'		!year
	cdate(10:11)=cdate1(7:8)		!year
	cdate(3:3)='-'
	cdate(7:7)='-'
	if(cdate1(1:2).eq.'01') cdate(4:6)='Jan'
	if(cdate1(1:2).eq.'02') cdate(4:6)='Feb'
	if(cdate1(1:2).eq.'03') cdate(4:6)='Mar'
	if(cdate1(1:2).eq.'04') cdate(4:6)='Apr'
	if(cdate1(1:2).eq.'05') cdate(4:6)='May'
	if(cdate1(1:2).eq.'06') cdate(4:6)='Jun'
	if(cdate1(1:2).eq.'07') cdate(4:6)='Jul'
	if(cdate1(1:2).eq.'08') cdate(4:6)='Aug'
	if(cdate1(1:2).eq.'09') cdate(4:6)='Sep'
	if(cdate1(1:2).eq.'10') cdate(4:6)='Oct'
	if(cdate1(1:2).eq.'11') cdate(4:6)='Nov'
	if(cdate1(1:2).eq.'12') cdate(4:6)='Dec'
	if(cdate(1:6).eq.'19-Jul') then
	   call BELL(2)
	   print 1
1	   format(' I know that today is someone"s birthday, but I cannot
     &remember whose...!')
	   pause
	endif
c	print 3,cdate
c3	format(' cdate=',a11)
	RETURN
	end
