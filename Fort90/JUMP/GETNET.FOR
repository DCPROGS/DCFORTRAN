	subroutine GETNET(xval,yval,ndv1,ndimd,icon1,icon2,
     &	idrug1,idrug2,in,deb)
c To calculate net points for I/V as each limb of ramp is completed in CJFIT
c (called by CJVDU). j=5 for control, j=6 for drug. Xval(ic,j) contains voltages
c in increments of Vstep, but may not start at same voltage for control
c and drug. Yval(ic,j) contains corresponding currents found by interpolation.
c Net current returned in xval(in,7),yval(in,7) for all voltages that occur
c in BOTH drug and control.  Cannot get more than one net point at given
c voltage from one limb of ramp (one call to this routine)
c=	real*4 XVAL(2048,10),YVAL(2048,10)
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	logical deb
	logical discprt
	common/dp/discprt
c
	k1=idrug1		!index to start looking through agonist values
	do 76 i=icon1,icon2
	    vcon=xval(i,5)      !voltage
	    ccon=yval(i,5)      !current
c Now want to find if there is a corresponding voltage in the agonist values
	    do 77 k=k1,idrug2
		vdrug=xval(k,6)	!voltage
	      cdrug=yval(k,6)   !current
c		if(deb) then
c		   print 771,vcon,vdrug,ccon,cdrug
c		   if(discprt) write(8,771) vcon,vdrug,ccon,cdrug
c771		   format(4g12.5)
c		endif
		if(abs(vcon-vdrug).lt.0.01) then
cc===		if(abs(vcon-vdrug).lt.0.5) then
		   in=in+1
		   xval(in,7)=vcon      	!=vdrug= voltage for net
		   yval(in,7)=cdrug-ccon	!net current at this V
c		   if(deb) then
c			print 772,in,vcon,cdrug,ccon,yval(in,7)
c			if(discprt) write(8,772) in,vcon,cdrug,ccon,yval(in,7)
c772		   	format(' NET POINT # ',i5,4g12.5)
c		   endif
		   k1=k+1			!so this value not used again
		   goto 76			!jump out of k loop as soon as value found
		endif
77	    continue
76	continue
c
	RETURN
	end


