	program FIELLER
c To calculate Fieller's theorem
c
	logical discprt,recip
	character cdate*11,ctime*11,ans*1,ans1*1
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c
c	OPEN(unit=7,file='PRN',iostat=nerr)
	filnam='fieller.prt'
	call WINPRINT
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
c
	if(discprt) write(8,3000)
3000	format(/,' FIELLER: Confidence limits for a ratio')
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 3001,cdate,ctime(1:8),mtitle
	if(discprt) write(8,3001) cdate,ctime(1:8),mtitle
3001	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     &'   Machine = ',a40)
	print 3002
3002	format(
     & ' SCROLL LOCK on for printing')
	ans1='n'
c
10	continue
	recip=.false.
	print 1
1	format(/,
     & ' To calculate approximate SD of the ratio r=a/b, given',/,
     & ' the SD of a (numerator) and of b (denominator), and the',/,
     & ' correlation coefficient between a, b (zero if they are',/,
     & ' independent). Uses Fieller''s theorem (see Finney).',/,/,
     &' Numerator, denominator = ')
	call INPUT2r(a,b)
	print 2
2	format('&SD(numerator), SD(denominator) = ')
	call INPUT2r(sa,sb)
	r=0.
	print 3
3	format('&Correlation coefficient (num,den) [0] = ')
	call INPUTr(r)
	tval=1.0
	print 4
4	format('&Student''s t value [1.0] = ')
	call INPUTr(tval)
c
11	continue		!return to calc reciprocal ratio
	va=sa*sa
	vb=sb*sb
	cov=r*sa*sb
c
	g=tval*tval*vb/(b*b)
	ratio=a/b
	rat2=ratio*ratio
c Write disc in a way that does not appear to divide by vb
c (which actually cancels) so OK to use vb=0
c	disc=va - 2.0*ratio*cov + rat2*vb - g*(va-cov*cov/vb)
	disc=va - 2.0*ratio*cov + rat2*vb - g*(va-r*r*va)
	if(disc.lt.0.) then
	   call BELL(2)
         print 5,ratio,sa,sb,r,disc
         if(discprt) write(8,5) ratio,sa,sb,r,disc
5	   format(
     &   ' ratio (a/b) = ',g13.6,/,
     &   ' s(a), s(b), r(a,b) = ',3g13.6,/,
     &   ' No solution: cannot take square root of ',g13.6)
	   goto 12
	endif
c
	d=(tval/b)*sqrt(disc)
c Write pre in a way that does not appear to divide by vb
c (which actually cancels) so OK to use vb=0 (use g=tval*tval*vb/(b*b))
c	pre=ratio - g*cov/vb
	pre=ratio - (tval*tval*r*sa*sb)/(b*b)
	f=1./(1.-g)
	clower=f*(pre-d)
	cupper=f*(pre+d)
	dlow=clower-ratio
	dhi=cupper-ratio
c Approximation for small g
	appsd=sqrt(va + rat2*vb - 2.0*ratio*cov)/b
	applo=ratio - tval*appsd
	apphi=ratio + tval*appsd
c
	if(recip) then
         print 7
         if(discprt) write(8,7)
7	   format(/,/,' Limits for reciprocal of last ratio')
	else
         print 8
         if(discprt) write(8,8)
8	   format(/,/,' Confidence limits for the ratio')
	endif
	cvr=100.*appsd/ratio
      print 6,a,b,sa,sb,r,g,ratio,tval,clower,cupper,
     &     dlow,dhi,appsd,cvr,applo,apphi
      if(discprt) write(8,6) a,b,sa,sb,r,g,ratio,tval,clower,cupper,
     &     dlow,dhi,appsd,cvr,applo,apphi
6	format(
     &   '    numerator (a), denominator (b) = ',2g13.6,/,
     &   '    s(a), s(b), r(a,b) = ',3g13.6,/,
     &   '    g value = ',g13.6,/,
     &   ' Ratio (a/b) = ',g13.6,/,
     &   ' Confidence limits for t = ',f8.2,':',/,
     &   '  Lower, upper limits = ',2g13.6,/,
     &   '     ( i.e. deviations = ',2g13.6,')',/,
     &   ' Approximate SD of ratio ( when g<<1 ) = ',g13.6,/,
     &   ' Approximate CV of ratio (%) = ',g13.6,/,
     &   ' Approximate lower, upper limits = ',2g13.6)
c
	if(recip) goto 12
	print 108
108	format(/)
	call DCASK(' Calculate limits for reciprocal',ans1,ans)
	if(ans.eq.'Y') then
	   ans1=ans		!default next time
	   x=a
	   a=b
	   b=x
	   x=sa
	   sa=sb
	   sb=x
	   recip=.true.
	   goto 11
	endif
c
12	print 108
	call DCASK(' Another ratio','y',ans)
	if(ans.ne.'N') goto 10
c
	call ENDPRINT
	end


