	subroutine RISTIM(trise,z,prnt)
c If z=no of risetimes is negative calculates a table for z=0.5(0.5)2.5
c otherwise calculates a single set of values for z
	logical prnt,discprt
	common/dp/discprt
c	character*1 ans,UC
c
101	format(a2)
	print 228
	if(prnt) write(7,228)
	if(discprt) write(8,228)
228	format(' length (ms)   length/Trise    A/Amax')
	if(z.gt.0.001) goto 10
	do 229 i=1,5
	x=float(i)*0.5
	print 23,x*trise*1.e-3,x,erfs(0.88604*x)
	if(prnt) write(7,23) x*trise*1.e-3,x,erfs(0.88604*x)
229	if(discprt) write(8,23) x*trise*1.e-3,x,erfs(0.88604*x)
23	format(3(g13.6,2x))
	RETURN
c233	print 230
c230	format(' print another value? ')
c	read 101,ans
c	if(UC(ans).eq.'N') RETURN
c	print 231
c231	format(' length/trise= ')
c	read 232,z
c232	format(g13.6)
10	print 23,z*trise*1.e-3,z,erfs(0.88604*z)
	RETURN
c	goto 233
	end


