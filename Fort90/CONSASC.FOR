	PROGRAM CONSASC
	allocatable :: idata
	integer*2 idata(:)
	integer*2 iver,idt,inc,id1,id2
	integer*4 ioff,ilen
	character ascfil*40,infil*40,ans*1,UC*1
	character cdate*11,cs*3,adctime*8
	character title*70
	logical   present

5     continue
	call TITENT0('Enter path & name for input CONSAM file:',
     & infil,40,.true.)
c	print 1
c1 	format
c     &(' Enter path and name of CONSAM file (i.e. C:\CONSAM.DAT)=',/)
c	read*,infil
	INQUIRE(file=INFIL,exist=present,flen=ilen)
	if(present.and.ilen.gt.0) then
	   call TITENT0('Enter path & name for output ASCII file:',
     &   ascfil,40,.true.)
c	   print 2
c2	   format
c     &(' Enter path and name for ASCII file (i.e. C:\CONSAM.ASC)=',/)
c	   read*,ascfil
	   ndat=(ilen-512)/2		!number of integer*2
	   OPEN(unit=14,file=INFIL,status='UNKNOWN',access='TRANSPARENT')
	   read(14,rec=1) iver,title,cdate,adctime,idt,ioff,ilen,inc,
     &   id1,id2,cs(1:3),calfac
	   print 3,calfac
3	   format(/,1x,' calfac=',f15.8)
	   irec=513
	   allocate(idata(ndat))
	   read(14,rec=irec) (idata(i),i=1,ndat)
	   CLOSE(unit=14)
	   ncurvd=1
	   OPEN(unit=12,file=ascfil,status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
	   write(12,FMT=8000) ncurvd
8000	   format(i10)
	   write(12,FMT=8000) ndat
	   do i=1,ndat
		write(unit=12,fmt=801) i,idata(i),float(idata(i))*calfac
801		format(2I10,f15.5)
	   enddo
	   CLOSE(unit=12)
	   deallocate(idata)
	else
	   call BELL(1)
	   ans='Y'
	   call DCASK('CONSAM file not found: try again',ans,ans)
c	   print 4
c4	   format('Wrong name. Try again? (Y or N) ? ',/)
c	   call inputa(ans)
	   if(UC(ans).eq.'Y') goto 5
	endif

	end
