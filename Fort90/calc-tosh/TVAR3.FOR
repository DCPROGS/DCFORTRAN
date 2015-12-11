	real y(10000),uran(10000)
	logical repeat
	common/rand/ix,iy,iz
c
	call RANDSK(ix,iy,iz,0,repeat)		!read IX,IY,IZ
	ny=10000
	do i=1,ny
	   y(i)=ranorm()
	enddo
	call VARV3(y,ny,ybar,vary,ymin,ymax,skew)
	print 60,ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(pon()) write(7,60) ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(discprt) write(8,60) ny,ybar,sqrt(vary),skew,ymin,ymax
60	format(/,' For normal variate:',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
c
	do i=1,ny
	   y(i)=exp(ranorm())
	enddo
	call VARV3(y,ny,ybar,vary,ymin,ymax,skew)
	print 61,ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(pon()) write(7,61) ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(discprt) write(8,61) ny,ybar,sqrt(vary),skew,ymin,ymax
61	format(/,' For log-normal variate:',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
c
c try dist such that exp(y) is Gaussian -should have negative skew
	ny1=0
	do i=1,ny
	   r=ranorm() + 5.  !Gaussian, mean=5, var=1
	   if(r.gt.0.) then
		ny1=ny1+1
		y(i)=alog(r)
	   endif
	enddo
	call VARV3(y,ny1,ybar,vary,ymin,ymax,skew)
	print 63,ny1,ybar,sqrt(vary),skew,ymin,ymax
c	if(pon()) write(7,63) ny1,ybar,sqrt(vary),skew,ymin,ymax
c	if(discprt) write(8,63) ny1,ybar,sqrt(vary),skew,ymin,ymax
63	format(/,' For anti-log-normal variate:',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
c
	do i=1,ny
	   u=random()
	   y(i)=-alog(u)
	   uran(i)=u
	enddo
	call VARV3(y,ny,ybar,vary,ymin,ymax,skew)
	print 62,ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(pon()) write(7,62) ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(discprt) write(8,62) ny,ybar,sqrt(vary),skew,ymin,ymax
62	format(/,' For exponential variate (mean=1):',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
c
	do i=1,ny
	   y(i)=-10.*alog(uran(i))
	enddo
	call VARV3(y,ny,ybar,vary,ymin,ymax,skew)
	print 64,ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(pon()) write(7,64) ny,ybar,sqrt(vary),skew,ymin,ymax
c	if(discprt) write(8,64) ny,ybar,sqrt(vary),skew,ymin,ymax
64	format(/,' For exponential variate (mean=10; otherwise same):',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
	call RANDSK(ix,iy,iz,1,repeat)		!write IX,IY,IZ
	end


