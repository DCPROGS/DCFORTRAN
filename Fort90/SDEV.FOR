	program SDEV
	character*79 heading,title
	real data(100,10),Yobs(100),sd(100),w(100)
	logical discprt,pon,slock,first
      character*1 ans,UC
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c To calc weighted means and SD
c
	pon()=slock()
c
      filnam='SDEV.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
c
101   format(a1)
	iw=1		!default 1st time
	first=.true.
6	print 106
106	FORMAT(' Number of observations in set = ')
	call INPUTi(n)
	if(n.lt.2) goto 6
104	format(i8)
	call TITENT0('Give title for the set:',title,79,.true.)
33	print 32,iw
32	format(
     &    ' (1) Unweighted mean',/,
     &    ' (2) Specify standard deviation, s(y)',/,
     &    ' (3) Specify weight, 1/var(y)',/,
     &    ' (4) Specify arbitrary weight',/,
     &    ' Option number [',i2,']= ')
	call INPUTi(i)
	if(i.lt.0.and.i.gt.4) goto 33
	if(i.ge.1) iw=i
c
	if(iw.eq.1) then
	   heading='    Y value'
	   ncol=1
	else if(iw.eq.2) then
	   heading='    Y value     Standard dev '
	   ncol=2
	else if(iw.eq.3.or.iw.eq.4) then
	   heading='    Y value       Weight     '
	   ncol=2
	endif
c
	nidisp=n
	if(nidisp.gt.20) nidisp=20	!best value?
	nr1=-1		!so window initially in default position
	iconst=0		!cols not linked
c	if(first) then
	   iflag=0	!display initially blank
c	else
c	   iflag=ncol	!display last values initially
c	endif
c
c
	call INWIND1(data,n,ncol,nidisp,title,heading,
     & nr1,nc1,iflag,iconst)
c
c allocate data
8	do 107,i=1,n
	Yobs(i)=data(i,1)
	w(i)=1.0		!if iw.eq.1
	if(iw.eq.2) then
	   SD(i)=data(i,2)
	   if(sd(i).lt.1.e18) then
		w(i)=1.0/(sd(i)*sd(i))
	   else
		w(i)=0.0
	   endif
	else if(iw.eq.3.or.iw.eq.4) then
	   w(i)=data(i,2)
	   if(w(i).gt.1.e-37) then
	      sd(i)=sqrt(1.0/w(i))
         else
	      sd(i)=1.e36
	   endif
	endif
107	continue
c Print results
	if(iw.eq.1) goto 40
c
	heading=
     & '           Y value       Standard dev   Weight'
	print 39,title,heading
	if(pon()) write(7,39) title,heading
	if(discprt) write(8,39) title,heading
39	format(1x,a79,/,a79)
	do 13 i=1,n
	print 12,i,Yobs(i),sd(i),w(i)
	if(pon())write(7,12)i,Yobs(i),sd(i),w(i)
	if(discprt)write(8,12)i,Yobs(i),sd(i),w(i)
12	format(I4,3X,3(2X,G13.6))
13	continue
	call VARW(Yobs,W,n,ybar,varybar,vary1)
	sdy1=-1.
	if(vary1.gt.0.) sdy1=sqrt(vary1)
	sey=sqrt(varybar)		!from 1/(sumw)
	sey1=sdy1/sqrt(float(n))
	print 14,ybar,sey,sdy1,sey1
	if(pon())write(7,14) ybar,sey,sdy1,sey1
	if(discprt) write(8,14) ybar,sey,sdy1,sey1
14	format(
     & '____________________________________',/,
     & ' Mean',3x,2x,g13.6,/,' s(ybar)',3x,2x,g13.6,/,
     & ' s1(y)',2x,g13.6,/,' s1(ybar)',2x,g13.6,/,/,
     & ' (Note: s(ybar) is appropriate only when weights are genuine',/,
     & ' reciprocal variances; s1(y),s1(ybar) are appropriate only',/,
     & ' weights are the number of values averaged to get each Y value',
     & /,/)
      print 41
      if(pon()) write(7,41)
      if(discprt) write(8,41)
41	format(/,' Same values give unweighted mean:')
	goto 89
c
c Section for iw=1
40	continue
	heading=
     & '   Y value  '
	print 39,title,heading
	if(pon()) write(7,39) title,heading
	if(discprt) write(8,39) title,heading
	do 131 i=1,n
	print 121,i,Yobs(i)
	if(pon())write(7,121)i,Yobs(i)
	if(discprt)write(8,121)i,Yobs(i)
121	format(I4,3X,2X,G13.6)
131	continue
89	call VAR(Yobs,n,ybar,vary)
	sdy=sqrt(vary)
	sey=sdy/sqrt(n)
	cv=100.*sey/ybar
	print 141,ybar,sdy,sey,cv
	if(pon())write(7,141) ybar,sdy,sey,cv
	if(discprt) write(8,141) ybar,sdy,sey,cv
141	format(
     & '____________________________________',/,
     & ' Mean',9x,g13.6,/,
     & ' s(y)',9x,g13.6,/,
     & ' s(ybar)',6x,g13.6,/,
     & ' C of V (%)',1x,g13.6,/,/)
c
90	continue
	first=.false.
	print 5
5	format(' Another set [Y] ? ')
      read 101,ans
      if(UC(ans).ne.'N') goto 6
	call ENDPRINT
c
	END

