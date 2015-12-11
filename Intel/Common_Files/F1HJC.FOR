	real*8 function F1HJC(t,g00,g10,g11,eigen,tres,k,km)
c To calculate exact HJC distribution, f(t1)=fo(t1)-f1(t1-tres),
c for the second interval t=2tres,3tres where t=real (NOT excess) time in ms
c NB eigen=eigenvalues of -Q
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 g00(100),g10(100),g11(100),eigen(100)
	real*8 dexp1
c
      f1hjc=0.d0
	t0=t*1.d-3		!t in seconds
	t1=t0-tres		!excess time
	t2=t1-tres
	f=0.d0
	do 1 m=1,k
	f=f + g00(m)*dexp1(-eigen(m)*t1)	!this is f0(t1)
	f=f - (g10(m)+g11(m)*t2)*dexp1(-eigen(m)*t2)
1	continue
	F1HJC=f
	RETURN
	end

