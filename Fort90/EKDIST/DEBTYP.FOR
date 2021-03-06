	subroutine DEBTYP(nval,n1,n2,tint,ampl,iprops,jval,j1,j2,yval,
     & prnt,nintt,ndimy)
c To print values of intervals and amps for debugging in EKDIST
c Nval,jval used only to indicate no of values when i1,i2 asked for
c  here (n1=0 or j1=0), otherwise ignored.
c (1)If n1>0 in call then TINT,AMP values from n1 to n2 are printed
c without asking for values here (and without title)
c (2) If n1=0 in call then asks which elements of TINT, AMP to type
c (3) If n1<0 in call then do only Jval part
c (4) print Yval() similarly controlled by Jval,j1,j2
	real*4 tint(nintt),ampl(nintt)
	real*4 yval(ndimy)
	integer*1 iprops(nintt)
	logical prnt
	logical discprt
	common/dp/discprt
c
	if(n1.lt.0) goto 3
	if(n1.eq.0) goto 1
	i1=n1
	i2=n2
	goto 2
1	print 403,Nval
403	format(I6,
     &	' values. print times and amps for I1,I2 (0=none)= ')
	i1=0
	i2=0
	call INPUT2i(i1,i2)
	if(i1.le.0) goto 3
2	continue
	if(prnt.and.n1.le.0) write(7,146)
	if(discprt.and.n1.le.0) write(8,146)
	if(n1.le.0) print 146
146	format(10x,
     &	' Length (ms)   Amplitude (pA)   Properties')
	do i=i1,i2
	   print 345,i,tint(i),ampl(i),iprops(i)
	   if(prnt) write(7,345) i,tint(i),ampl(i),iprops(i)
	   if(discprt) write(8,345) i,tint(i),ampl(i),iprops(i)
345	   format(i8,2x,g13.6,2x,g13.6,2x,i4)
	enddo
	if(n1.eq.0) goto 1		!more?
c
c Now YVAL part
3	continue
	if(j1.lt.0) goto 9
	if(j1.eq.0) goto 11
	i1=j1
	i2=j2
	goto 12
11	print 4031,jval
4031	format(i6,
     &	' values. print Yval(i) for I1,I2 (0=none)= ')
	i1=0
	i2=0
	call INPUT2i(i1,i2)
	if(i1.le.0) goto 9
12	continue
	if(i2.gt.i1) goto 16
	if(prnt) write(7,4) i1,yval(i1)
	if(discprt) write(8,4) i1,yval(i1)
	print 4,i1,yval(i1)
4	format('&',16x,' Yval(',i5,')= ',g13.6)
	goto 9
16	continue
	print 17
17	format(' YVAL=')
	DO 15 I=i1,i2
	if(prnt) write(7,41) i,yval(i)
	if(discprt) write(8,41) i,yval(i)
	print 41,i,yval(i)
41	format(i8,3x,g13.6)
15	continue
	if(j1.eq.0) goto 11		!more?
c end
9	print 108
	if(discprt) write(8,108)
108	format(/)
	call flush(7)
	RETURN
	END


