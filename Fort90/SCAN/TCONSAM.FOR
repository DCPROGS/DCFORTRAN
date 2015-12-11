	program TCONSAM
c To test consam file written to CD
c
	logical present,discprt
	integer*2 idata(1024000)
	integer*2 idata1(1024000)
	character*1 ans
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,ndev*2,ndevdat*2
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,adctime*8
	character*76 title
c
	character ADCFIL1*30,cdate01*11,cdate11*11,ndev1*2,ndevdat1*2
	integer*2 idt1,inchan1,id11,id21,iver1
	character cs1*3,adctime1*8
	character*76 title1
c
c
	discprt=.false.
c
	nbuf=1024000
	do i=1,nbuf
	   idata(i)=0
	enddo
c
	print 55
55	format(' Specify disc partition for CONSAM file:')
	call DISCNUM1(idisc,ndev,0,0)	!ask for winchester (only)- no default
	adcfil=ndev//'\scdat\newland\consmcn2.dat'
	call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
	ndev=adcfil(1:2)
	ans='Y'
	call DCASK(' Compare two files',ans,ans)
	   nfile=1
	if(ans.eq.'Y') then
	   print 551
551	   format(' Specify disc partition for second CONSAM file:')
	   call DISCNUM1(idisc,ndev1,0,0)	!ask for winchester (only)- no default
	   adcfil1=ndev1//'\scdat\newland\consmcn2.dat'
	   call TITENT0('Input 2nd CONSAM file name:',ADCFIL1,30,.false.)
	   nfile=2
	endif
c
c Open data file, and read header
	INQUIRE(file=ADCFIL,exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &      id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	   CLOSE(unit=14)
	   pAV=calfac*6553.6			!pA/volt (display now scaled in volts)
         print 37, ndev,cdate,adctime,ffilt,title,srate,ilen,calfac
         if(discprt)write(8,35) ndev,cdate,adctime,ffilt,title,srate,
     &	ilen,calfac
37	   format(' Disk ',a2,' Date= ',a11,' time= ',a8,
     &    ' filter (Hz) = ',f9.1,/,1x,a76,/,
     &    ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/,
     &    ' calibration factor (pA/ADC unit) = ',g13.6)
	   nbyte=nlen-ioff
	   nint=nbyte/2
	   nsec=1 + (nint-1)/nbuf	!number of complete arrays that can be read
	   nlast=nint-(nsec-1)*nbuf	!number of values for last read
	   nread=nbuf
	   print 36,nbyte,nint,nsec,nread,nlast
36	   format(
     & ' Number of bytes for data = ',i10,/,
     & ' Number of integers for data = ',i10,/,
     & ' Number of data sections = ',i10,/,
     & ' Number of points/section = ',i10,/,
     & ' Number of points in last section = ',i10,/)
	endif
c
c Now read 2nd file to be compared?
	if(nfile.eq.1) goto 100
c
	INQUIRE(file=ADCFIL1,exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
         OPEN(unit=15,file=ADCFIL1,status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(15,rec=1)title1,cdate01,adctime1,idt1,ioff1,ilen1,inchan1,
     &      id11,id21,cs1(1:3),calfac1,srate1,ffilt,ffilt1,calfac1,iver1
	   CLOSE(unit=15)
	   pAV=calfac*6553.6			!pA/volt (display now scaled in volts)
         print 37, ndev1,cdate1,adctime1,ffilt1,title1,
     &	srate1,ilen1,calfac
         if(discprt)write(8,37) ndev1,cdate1,adctime1,ffilt1,title1,
     &	srate1,ilen1,calfac
c35	   format(' Disk ',a2,' Date= ',a11,' time= ',a8,
c     &    ' filter (Hz) = ',f9.1,/,1x,a76,/,
c     &    ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/,
c     &    ' calibration factor (pA/ADC unit) = ',g13.6)
	   nbyte=nlen-ioff
	   nint=nbyte/2
	   nsec=1 + (nint-1)/nbuf	!number of complete arrays that can be read
	   nlast=nint-(nsec-1)*nbuf	!number of values for last read
	   nread=nbuf
	   print 36,nbyte,nint,nsec,nread,nlast
	endif
c
100	continue
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
	if(nfile.eq.1) then
	   OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   do isec=1,nsec
		n=nread
		if(isec.eq.nsec) n=nlast
		j1=1+(isec-1)*nbuf
		irec=ioff+(2*j1-1)
c		read(14,rec=irec) (idata(i),i=1,n)
c temp check
		ir0=irec
		do i=1,n
		   read(14,rec=ir0) idata(i)
		   ir0=ir0+2	!2 bytes/number
		   print 7,ir0,i,idata(i)
7		   format('  ',i10,3x,i10,3x,i8)
		enddo
c end of temp check
c	now check values
		print 51,isec,irec,j1,j1+n-1
51		format(' section ',i4,' start record ',i10,/,
     &	' points ',i10,' to ',i10)
		do i=1,n
		   print 35,i,idata(i)
35		   format(' point ',i10,5x,i7)
		enddo
		print 5,isec,irec,j1,j1+n-1,nbad
5		format(' section ',i4,' start record ',i10,/,
     &	' points ',i10,' to ',i10,': number of errors = ',i3)
	   enddo
	   CLOSE(unit=14)
	else if(nfile.eq.2) then
	   OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   OPEN(unit=15,file=ADCFIL1,status='UNKNOWN',
     &	access='TRANSPARENT')
	   do isec=1,nsec
		n=nread
		if(isec.eq.nsec) n=nlast
		j1=1+(isec-1)*nbuf
		irec=ioff+(2*j1-1)
		read(14,rec=irec) (idata(i),i=1,n)
		read(15,rec=irec) (idata1(i),i=1,n)
c temp check
c		ir0=irec
c		do i=1,n
c		   read(14,rec=ir0) idata(i)
c		   read(15,rec=ir0) idata1(i)
c		   ir0=ir0+2	!2 bytes/number
c		   print 71,ir0,i,idata(i),idata1(i)
c71		   format('  ',i10,3x,i10,3x,i8,3x,i8)
c		enddo
c	now check values
		nbad=0
		do i=1,n
		   if(idata(i).ne.idata1(i)) then
			print 3,i,idata(i),idata1(i)
3			format(' point ',i10,5x,2i7)
			nbad=nbad+1
		   endif
		enddo
		print 52,isec,irec,j1,j1+n-1,nbad
52		format(' section ',i4,' start record ',i10,/,
     &	' points ',i10,' to ',i10,': number of errors = ',i3)
	   enddo
	   CLOSE(unit=14)
	   CLOSE(unit=15)
	endif
c
	end

