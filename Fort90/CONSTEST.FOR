	program constest


	integer*2  idata(100000)
	character adcfil*30,ndev*2
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
     &' (4) Make the sample at least 100 kb long (+512 byte header).',/,
     &' (5)',/,
     &'',/,
     &'',/,
	print 2
2	format(' Signal generator frequency (Hz) = ')
	call INPUTr(fsig)
	print 3
3	format(' Sampling frequency (Hz) = ')
	call INPUTr(fsamp)
c
	nps=fsamp/(2.*fsig)	!number of points expected at each level
c
c Disc for CONSAM?
	print 4
4	format(' Enter disk partition where CONSAM.DAT is')
	call DISCNUM1(idisc,ndev,0,0)	!ask for hard disk (only)- no default
	ADCFIL=ndev//'\CONSAM.DAT'
	INQUIRE(file=ADCFIL,exist=present,flen=ilen)
	ilen1=ilen-512
	irec=513
	OPEN(unit=14,file='CONSAM.DAT',status='UNKNOWN',
     &	access='TRANSPARENT')
	read(14,rec=irec) (idata(i),i=1,ilen1)
c
	nlow(1)=0
	nhigh(1)=0
	nsum1=0
	nsum2=0
	j=0
	j1=0
====
	do i=1,ilen1
	   if(idata(i).lt.0) then
		if(nhigh(j1).gt.0) then
		   print 9,nhigh(j1)
9		   format(' number of positive values = '
		   j1=j1+1
		   nsum2=nsum2+nhigh(j1)		!accumulate for average
		   nhigh=0		!reset
		endif
c
		nlow(j)=nlow(j)+1
	   else
		if(nlow.gt.0) then
		   print 10,nlow
10		   format(' number of negative values = '
		   j=j+1
		   nsum1(j)=nsum1(j)+nlow			!accumulate for average
		   nlow=0		!reset
		endif
c
		nhigh=nhigh+1
	   endif
	enddo
================
	scalfac=0.001
	select case(iext1)
	case(1)
		filname(1)='consam.1'
	case(2)
		filname(1)='consam.2'
	case(3)
		filname(1)='consam.3'
	case(4)
		filname(1)='consam.4'
	case(5)
		filname(1)='consam.5'
	case(6)
		filname(1)='consam.6'
	case(7)
		filname(1)='consam.7'
	case(8)
		filname(1)='consam.8'
	case(9)
		filname(1)='consam.1_5'
	case(10)
		filname(1)='consam.dat'
	end select
	select case(iext2)
	case(1)
		filname(2)='consam.1'
	case(2)
		filname(2)='consam.2'
	case(3)
		filname(2)='consam.3'
	case(4)
		filname(2)='consam.4'
	case(5)
		filname(2)='consam.5'
	case(6)
		filname(2)='consam.6'
	case(7)
		filname(2)='consam.7'
	case(8)
		filname(2)='consam.8'
	case(9)
		filname(2)='consam.1_5'
	case(10)
		filname(2)='consam.dat'
	end select
	select case(iext3)
	case(1)
		filname(3)='consam.1'
	case(2)
		filname(3)='consam.2'
	case(3)
		filname(3)='consam.3'
	case(4)
		filname(3)='consam.4'
	case(5)
		filname(3)='consam.5'
	case(6)
		filname(3)='consam.6'
	case(7)
		filname(3)='consam.7'
	case(8)
		filname(3)='consam.8'
	case(9)
		filname(3)='consam.1_5'
	case(10)
		filname(3)='consam.dat'
	end select
c	ADCFIL='I:'//'\'//FILNAME(IFIL)

	id1=iext1*10000./f-1
	id2=iext2*10000./f-1
	id3=iext3*10000./f-1
	i1=1
	i2=1000
	irec=513
	ii1=iext1*10000
	ii2=iext2*10000
	ii3=iext3*10000
	call realtoch(ii1,c1,5)
	call realtoch(ii2,c2,5)
	call realtoch(ii3,c3,5)
	do i=1,1000
		ix1(i)=int(i*8/iext1)
	enddo
	do i=1,1000
		ix2(i)=i*8/iext2
	enddo
	do i=1,1000
		ix3(i)=i*8/iext3
	enddo
	irec=513
	OPEN(unit=14,file=filname(1),status='UNKNOWN',
     &	access='TRANSPARENT')
	read(14,rec=irec) (idata1(i),i=i1,ii1)
	CLOSE(unit=14)

	irec=513
	OPEN(unit=14,file=filname(2),status='UNKNOWN',
     &	access='TRANSPARENT')
	read(14,rec=irec) (idata2(i),i=i1,ii2)
	CLOSE(unit=14)
	irec=513
	OPEN(unit=14,file=filname(3),status='UNKNOWN',
     &	access='TRANSPARENT')
	read(14,rec=irec) (idata3(i),i=i1,ii3)
	CLOSE(unit=14)

	OPEN(unit=12,file='i:\cons.dat',status='unknown',
     &	access='DIRECT',form='formatted',recl=40)
	ll1=40*iext1
	do i1=1,ll1
		if(idata1(i1).lt.0) then
			print*,i1
			goto 1
		endif
	enddo
	ll2=40*iext2
1	do i2=1,ll2
		if(idata2(i2).lt.0) then
			print*,i2
			goto 2
		endif
	enddo
	ll3=40*iext3
2	do i3=1,ll3
		if(idata3(i3).lt.0) then
			print*,i3
			goto 3
		endif
	enddo

3	continue
	do i=1,ii1
		if(idata1(i).lt.0) then
			j=j+1
		else
			j1=j1+1
		endif
	enddo
	print*,ii1,' Hz:NEGATIVE=',J,'  	POSITIVE=',J1
	a=getch(b)
	write(unit=12,fmt=10) j,j1,char(13),char(10)
10	format('NEGATIVE =',i8,'  POSITIVE =',
     & i8,2a1)
	m=1
 11	continue
	j=0
	j1=0
	do i=i1,i1+id1
		if(idata1(i).lt.0) then
			j=j+1
		else
			j1=j1+1
		endif
	enddo
	print*,ii1,'Hz (',m,'): NEGATIVE=',J,'	POSITIVE=',J1
	write(12,10) j,j1,char(13),char(10)
	i1=i1+id1
	m=m+1
	if(m.eq.21.or.m.eq.41.or.m.eq.61.or.m.eq.81.or.m.eq.101
     &   .or.m.eq.121.or.m.eq.141.or.m.eq.161.or.m.eq.181) then
		a=getch(b)
	endif
	if(i1.le.(ii1-ii1/10)) goto 11
	a=getch(b)


	do i=1,ii2
		if(idata2(i).lt.0) then
			k=k+1
		else
			k1=k1+1
		endif
	enddo
	print*,ii2,' Hz:NEGATIVE=',K,'	POSITIVE=',K1
	write(unit=12,fmt=10) k,k1,char(13),char(10)
	a=getch(b)
	m=1
 12	continue
	k=0
	k1=0
	do i=i2,i2+id2
		if(idata2(i).lt.0) then
			k=k+1
		else
			k1=k1+1
		endif
	enddo
	print*,ii2,'(',m,'):NEGATIVE=',k,'	POSITIVE=',k1
	write(12,10) k,k1,char(13),char(10)
	i2=i2+id2
	m=m+1
	if(m.eq.21.or.m.eq.41.or.m.eq.61.or.m.eq.81.or.m.eq.101
     &   .or.m.eq.121.or.m.eq.141.or.m.eq.161.or.m.eq.181) then
		a=getch(b)
	endif
	if(i2.le.(ii2-ii2/10)) goto 12
	a=getch(b)



	do i=1,ii3
		if(idata3(i).lt.0) then
			l=l+1
		else
			l1=l1+1
		endif
	enddo
	print*,ii3,' Hz:NEGATIVE=',L,'	POSITIVE=',L1
	write(12,10) l,l1,char(13),char(10)
	a=getch(b)
	m=1

13 	continue
	l1=0
	l=0
	do i=i3,i3+id3
		if(idata3(i).lt.0) then
			l=l+1
		else
			l1=l1+1
		endif
	enddo
	write(12,10) l,l1,char(13),char(10)
	print*,ii3,'(',m,'):NEGATIVE=',l,'	POSITIVE=',l1
	i3=i3+id3
	m=m+1
	if(m.eq.21.or.m.eq.41.or.m.eq.61.or.m.eq.81.or.m.eq.101
     &   .or.m.eq.121.or.m.eq.141.or.m.eq.161.or.m.eq.181) then
		a=getch(b)
	endif
	if(i3.le.(ii3-ii3/10)) goto 13
	CLOSE(unit=12)

	a=getch(b)

	do i=1,20
		print*,i,idata1(i),idata2(i),idata3(i),
     &		 i+20,idata1(i+20),idata2(i+20),idata3(i+20)
	enddo
	a=getch(b)
c Scale here
	do i=1,1000
	   re1=float(idata1(i))*scalfac
	   re2=float(idata2(i))*scalfac
	   re3=float(idata3(i))*scalfac
	   idata1(i)=int2(ifixr(re1)+60)
	   idata2(i)=int2(ifixr(re2)+240)
	   idata3(i)=int2(ifixr(re3)+420)
	enddo

	n1=80*iext1
	n2=80*iext2
	n3=80*iext3
	call mode(18)
	call clears(0,480,1)
	call hline(0,640,60,12)
	call hline(0,640,240,12)
	call hline(0,640,420,12)
	call draw(ix1,idata1,n1,1,14,14,1)
	call draw(ix2,idata2,n2,1,11,11,1)
	call draw(ix3,idata3,n3,1,15,15,1)
	call enter_str(40,16,c1,14,1)
	call enter_str(40,192,c2,11,1)
	call enter_str(40,384,c3,15,1)
	a=getch(b)
	call clears(0,480,1)
	call hline(0,640,60,12)
	call hline(0,640,240,12)
	call hline(0,640,420,12)
	call draw(ix8,idata1,640,1,14,14,1)
	call draw(ix8,idata4,640,1,11,11,1)
	call draw(ix8,idata8,640,1,15,15,1)
	a=getch(b)
	call mode(3)
	end
