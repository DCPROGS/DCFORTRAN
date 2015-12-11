	program TCONSAM
c
	integer*2  idata(500000)
	character adcfil*30,ndev*2
	integer*2 nneg(600),npos(600)
	logical present
c
	print 1
1	format(
     &' Test program: tests accuracy of ADC sampling by CONSAM.',/,
     &' (1) Attach a square wave generator to ADC0 and/or ADC1',/,
     &' (2) Set the square wave to plus/minus 1-3 volts.',/,
     &' (3) Set frequency to get around 1000 points at each level:',/,
     &'   e.g. for sampling at 10 kHz=10 points/ms need 100ms at ',/,
     &'   each level = 5 Hz square wave (use a digital signal ',/,
     &'   generator so the frequency is precise).',/,
     &' (4) Make the sample at least 400 kb long (+512 byte header).',/,
     &' (5) Run CONSAM to take the sample',/,
     &' (6) Run this program to test the resulting CONSAM.DAT file',/,
     &'    -should get 1000 points at each level',/,
     &'  ',/,
     &' Sample freq   Min length    Min length      Sq wave freq',/,
     &'   10000 Hz      20 sec         400 kb          5 Hz',/,
     &'   20000         10             400 kb          10 Hz',/,
     &'   40000          5             400 kb          20 Hz',/,
     &'   50000          4             400 kb          25 Hz',/,
     &'   60000          4             480 kb          30 Hz',/,
     &'   70000          3             420 kb          35 Hz',/,
     &'   80000          3             480 kb          40 Hz',/,
     &'   90000          3             540 kb          45 Hz',/,
     &'  100000          2             400 kb          50 Hz')
c
	print 2
2	format(' Signal generator frequency (Hz) = ')
	call INPUTr(fsig)
	print 3
3	format(' Sampling frequency (Hz) = ')
	call INPUTr(fsamp)
c
	nps=fsamp/(2.*fsig)	!number of points expected at each level
c
	idim=500000		!size of idata
	imax=idim/2		!max number of integers
	ndim=600		!size of nneg,npos
c
c Disc for CONSAM?
	print 4
4	format(' Enter disk partition where CONSAM.DAT is')
	call DISCNUM1(idisc,ndev,0,0)	!ask for hard disk (only)- no default
	ADCFIL=ndev//'\CONSAM.DAT'
	INQUIRE(file=ADCFIL,exist=present,flen=ilen)
	ndat=(ilen-512)/2		!number of integer*2
	if(ndat.gt.idim) ndat=imax
	irec=513
	OPEN(unit=14,file=ADCFIL,status='UNKNOWN',
     &	access='TRANSPARENT')
	read(14,rec=irec) (idata(i),i=1,ndat)
	CLOSE(unit=14)
c
	j=0
	j1=0
	do i=1,ndim
	   nneg(i)=0
	   npos(i)=0
	enddo
c
c Find the first sign change
c	i=1
c	do while(idata(i)*idata(i+1).gt.0)		!overlows with int*2!
	if(idata(1).lt.0) then
	   islast=-1
	else
	   islast=1
	endif
	do i=2,ndat
	   if(idata(i).lt.0) then
		isign=-1
	   else
		isign=1
	   endif
	   if(isign.ne.islast) then
		i1=i
		goto 9
	   endif
	   islast=isign
	enddo
c
c==	i1=i+1		!first value after sign change
9	continue
	k=i1
c
	if(idata(i1).lt.0) then		!neg sequence first
20	   j=j+1
	   if(j.gt.ndim) goto 99
	   do while (idata(k).lt.0)
		nneg(j)=nneg(j)+1
		k=k+1
		if(k.gt.ndat) goto 99
	   enddo
	   j1=j1+1
	   if(j1.gt.ndim) goto 99
	   do while (idata(k).gt.0)
		npos(j1)=npos(j1)+1
		k=k+1
		if(k.gt.ndat) goto 99
	   enddo
	   goto 20
	else
21	   j1=j1+1
	   if(j1.gt.ndim) goto 99
	   do while (idata(k).gt.0)
		npos(j1)=npos(j1)+1
		k=k+1
		if(k.gt.ndat) goto 99
	   enddo
	   j=j+1
	   if(j.gt.ndim) goto 99
	   do while (idata(k).lt.0)
		nneg(j)=nneg(j)+1
		k=k+1
		if(k.gt.ndat) goto 99
	   enddo
	   goto 21
	endif
c
99	continue
c
	kpos=j1-1	!reject last sample
	kneg=j-1
	print 30,kpos,kneg,nps
30	format(/,1x,i5, ' positive runs and ',i5,' negative runs',/,
     & ' (expect ',i6,' points per run)',/,
     & '  i       n(pos)       n(neg)')
	k=kpos
	if(kneg.gt.kpos) k=kneg
	do j=1,k
	   if(j.lt.kneg.and.j.lt.kpos) then
		print 31,j,npos(j),nneg(j)
31		format(1x,i6,2x,i6,2x,i6)
	   else if(j.lt.kneg) then
		print 32,j,nneg(j)
32		format(1x,i6,8x,2x,i6)
	   else if(j.lt.kpos) then
		print 33,j,npos(j)
33		format(1x,i6,2x,i6)
	   endif
	   if(mod(j,20).eq.0) then
		print 34
34		format(' hit any key for more ...')
		call ANYKEY
	   endif
	enddo
c
c Get mean and SD
c for runs with lengths that are all either n and n+1, in equal numbers,
c SD should be sqrt(0.25*N/(N-1))
	sd1=sqrt(0.25*float(kpos)/float(kpos-1))
	call IVAR2(npos,kpos,ybar,vary)
	print 40,kpos,ybar,sqrt(vary),sd1
40	format(/,1x,i6,' positive runs: mean = ',g13.6,': SD = ',g13.6,/,
     & '(asymptotic SD expected for jitter of 1 point = ',g13.6,')',/)
c
	sd1=sqrt(0.25*float(kneg)/float(kneg-1))
	call IVAR2(nneg,kneg,ybar,vary)
	print 41,kneg,ybar,sqrt(vary),sd1
41	format(1x,i6,' negative runs: mean = ',g13.6,': SD = ',g13.6,/,
     & '(asymptotic SD expected for jitter of 1 point = ',g13.6,')')
c
	end
