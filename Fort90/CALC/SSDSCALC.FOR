	function SSDSCALC(k,theta)
C SUM OF SQUARES for SCALCS
	real theta(100)
	real*4 xfit(2048),yfit(2048),weight(2048)
	logical fixamp
	common/fitblk/xfit,yfit,weight,nfit,ncomp,fixamp
c
	S=0.0
c
c Check that tau are positive
	do i=1,2*ncomp,2		!tau=1,3,5,...
	  if(theta(i).lt.0.0001) theta(i)=0.0001
	enddo
C
	yinf=theta(k)
	do i=1,nfit
	   t=xfit(i)
	   ycal=0.0
	   do j=1,ncomp
		j1=2*j-1		!=1,3,5..
		tau=theta(j1)
		amp=theta(j1+1)	!=2,4,6..
		ycal=ycal + amp*exp(-t/tau)
	   end do
	   ycal=ycal+yinf
	   dev=yfit(i)-Ycal
	   S=S + dev*dev
	end do
	SSDSCALC=s
c
	RETURN
	END

