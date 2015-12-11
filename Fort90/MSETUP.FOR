	program MSETUP
c To create/alter machine setup in C:\WINPRINT.INI
	character ndev*2
      character*1 ans,UC
	logical discprt,apfile,present
	character*40 mtitles(20),mtitle		!machine names
c
	INQUIRE(file='C:\WINPRINT.INI',exist=present,flen=len)
	if(present.and.len.gt.0) then
         OPEN(unit=14,file='C:\WINPRINT.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	   read(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitles
	   mtitle=mtitles(machine)		!name of this machine
	else		!make it!
	   mtitles(1)='DC Dell 425E'
	   mtitles(2)='DC Dell 433P'
	   mtitles(3)='DC Dell 450DE'
	   mtitles(4)='DC Dell 325P'
	   mtitles(5)='DC Toshiba 5200'
	   nmac=5
         OPEN(unit=14,file='C:\WINPRINT.INI',status='NEW',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	endif
c
	   do 15 i=1,nmac
	    print 16,i,mtitles(i)
16	    format(' (',i3,') ',a20)
15	   continue
	   newm=nmac+1	!next machine number
	   print 1,newm
1	   format(
     & ' Machine number [0=add machine #',i3,' to list] = ')
	   read 2,machine
2	   format(i8)
	   if(machine.le.0) then
		nlen=40
		call TITENT0('Owner and name of machine:',mtitle,
     &		nlen,.true.)
		mtitles(newm)=mtitle
		machine=newm
	   else
		mtitle=mtitles(machine)
	   endif
	   print 9,ndisc
9	   format(
     & ' Number of hard disc partitions on this machine [',i2,'] = ')
	   read 2,i
	   if(i.gt.0) ndisc=i
	   ic=ichar(ndev(1:1))
c   check if ndev=a-z or A-Z
	   if((ic.ge.65.and.ic.le.90).or.(ic.ge.97.and.ic.le.122)) then
	      print 10,ndev
10	      format('&Printout to go on disc partition ',a2,
     &	' O.K. [Y] ? ')
	      read 101,ans
101		format(a1)
	      if(UC(ans).eq.'N') then
		   call DISCNUM1(id,ndev,0,0)
	      endif
	   else
		if(ndisc.eq.10) ndev='H:'	!default
		if(ndisc.eq.4) ndev='E:'	!default
		if(ndisc.eq.3) ndev='E:'	!default
		if(ndisc.eq.1) ndev='C:'	!default
	      print 110
110	      format('&Specify disc partition for printout:')
		call DISCNUM1(id,ndev,0,1)
	   endif
	   icol=1
	   print 11
11	   format(' Colour screen [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') icol=0
	   write(14,rec=1) discprt,apfile,machine,ndev,ndisc,icol,
     &	nmac,mtitles
c
      close(unit=14)
	END

