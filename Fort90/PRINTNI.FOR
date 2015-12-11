	subroutine PRINTni(n,intarray)
c To output n integers, intarray(1) to (n), in form 24,28,31-38,41 etc
c (as input by INPUTni)
	integer*4 intarray(n)
	character string*79,cnum*11,cnum1*11
	logical discprt,pon,slock
	common/dp/discprt
c
	pon()=slock()
c
	i=1
	do while(i.le.n)
	   i1=i
	   if(i.lt.n) then
		do while(intarray(i+1).eq.1+intarray(i))
		   i=i+1
		   if(i.eq.n) goto 2
		enddo
	   endif
2	   if(i.eq.i1) then
		call INTCONV(intarray(i),cnum)
		if(i1.eq.1) then
		   string=charnb(cnum)
		else
		   string=charnb(string)//charnb(cnum)
		endif
	   else
		call INTCONV(intarray(i1),cnum)
		call INTCONV(intarray(i),cnum1)
		if(i1.eq.1) then
		   string=charnb(cnum)//'-'//charnb(cnum1)
		else
		   string=charnb(string)//charnb(cnum)//'-'//charnb(cnum1)
		endif
	   endif
	   if(i.lt.n) string=charnb(string)//','
	   i=i+1
	enddo
	ns=nblank(string)
	do i=1,ns
	   print 1,string(i:i)
         if(pon()) write(7,1) string(i:i)
         if(discprt) write(8,1) string(i:i)
1	   format('&',a1)
	enddo
c	print 1,string(1:ns)
c1	format(1x,a78)
	RETURN
	end

