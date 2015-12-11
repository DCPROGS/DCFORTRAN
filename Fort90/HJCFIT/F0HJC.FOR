	real*8 function F0HJC(t,g00,eigen,tres,k,km)
c t=tres,2tres where t=real (NOT excess) time in ms
c NB eigen=eigenvalues of -Q
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 g00(100),eigen(100)
	real*8 dexp1
c
	t0=t*1.d-3		!t in seconds
	t1=t0-tres		!excess time
	f=0.d0
	do 1 m=1,k
1     f=f + g00(m)*dexp1(-eigen(m)*t1)
	F0HJC=f
	RETURN
	end
