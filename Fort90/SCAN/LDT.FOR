	program ldt

	allocatable :: idata
	character*76 title
	integer*2 idt,inchan,id1,id2,iver
	integer*4 ioff,ilen
	character*30 adcfil1,adctest
	character ADCFIL*30,cdate*11
	character cs*3,adctime*8
	integer*4 leader,segdur,segstart
	integer*2 interval,count,idata(:)
	integer*1 idat(100)
	character  header(8)
	adcfil1="consam.ldt"
	ADCFIL="consam.dat"
	adctest="d:\temp\j220.ldt"
	call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
	OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &    id1,id2,cs(1:3),cfac,srate
	print 35,adcfil,cdate,adctime,ioff,title,srate,ilen
35	format(
     &' CONSAM file: ',a30,/,' Date= ',a11,' time= ',a8,
     &' ioff = ',i10,/,1x,a76,/,
     &' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/)
	interval=1000000/srate
	segdur=ilen/2
	count=cfac
	leader=0
	segstart=0
	allocate(idata(segdur))
	OPEN(unit=11,file=ADCtest,status='UNKNOWN',access='TRANSPARENT')
	read(11,rec=1) leader,interval,count,segstart,segdur
	do i=1,16,2
	   read(11,rec=i) idat(i),idat(i+1)
	   read(11,rec=i) header(i),header(i+1)
c	   t=idat(i)
c	   idat(i)=idat(i+1)
c	   idat(i+1)=t
c	   write(11,rec=i) idat(i),idat(i+1)
	enddo
c	read(11,rec=1) leader,interval,count,segstart,segdur
	read(11,rec=17) (idata(i),i=1,5)


	OPEN(unit=12,file=ADCFIL1,status='UNKNOWN',access='TRANSPARENT')
	write(12,rec=1) leader,interval,count,segstart,segdur
	read(14,rec=513) (idata(i),i=1,segdur)
	write(12,rec=17) (idata(i),i=1,segdur)
	close(unit=14)
	close(unit=12)
	close(unit=11)
	deallocate(idata)
	end
