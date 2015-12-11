	program SPEEDTST
c Klaus Bauer's benchmark
	parameter (n=100000)
	real s1,s2,a(n),b(n),c(n),s(n)
c
	call TIMER(n1)
	do 1 j=1,n
	k=mod(j,30)
	s1=k
	s2=j
	a(j)=exp(s1)/sqrt(s2)
	b(j)=alog(a(j))
	c(j)=cos(b(j))
1	s(j)=sin(b(j))
	s1=0.
	s2=0.
	do 2 j=1,n
	s1=s1+a(j)*b(j)
2	s2=s2+c(j)**2+s(j)**2
	s2=s2/n
c
	call TIMER(n2)
	print 3,s1,s2,(n2-n1)*10
3	format(' s1,s2 = ',2g13.6,/, ' time (ms) = ',i8)
	end


