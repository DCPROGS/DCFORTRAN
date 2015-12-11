	character*32 filnam
	character ndev*2
	logical discprt,apfile,open,present
	character*40 mtitles(20),mtitle		!machine names
c
	INQUIRE(file='C:\WINPRINT.INI',exist=present,flen=len)
	if(present.and.len.gt.0) then
         OPEN(unit=14,file='C:\WINPRINT.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	   read(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitles
	   mtitle=mtitles(machine)		!name of this machine
	endif
	end
