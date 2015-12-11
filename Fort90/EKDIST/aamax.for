	subroutine AAMAX(area,amean,sd,ncomp,jset)
c To calc resolution for shut times by Howe et al method in EKDIST
c AAMAX is verion of AAMAXSUB that is designed to be called AFTER fit
c of amplitude histogram so amplitudes and SD are known
c NB amean() etc list the smallest amplitude first
c For each sublevel list (treating amps as positive)
c (1) the duration of an opening that is needed to reach above sublevel + 2SD
c (2) the duration of an opening that is needed to reach below sublevel - 2SD
c
	common/aafunc/trise
	real area(10,10),amean(10,10),sd(10,10)	!amp fits only for nset=1?
	integer ncomp(10)
	character*1 ans
	logical discprt,bad1,bad2
	common/dp/discprt
	external AFUNC
c
	nc=ncomp(jset)
	full=abs(amean(nc,jset))
	print 3,full
	if(discprt) write(8,3) full
3	format(/,' CALCULATION OF EVENT DURATION NEEDED TO PASS SUBLEVEL',
     & /,'  Amplitude of MAIN level (pA) = ',f8.3)
	if(fac.le.0.) fac=2.
8	print 4,fac
4	format(
     &' Calculate time for event to reach sublevel +/- n*sd: n = ['
     &,f5.1,'] = ')
	call INPUTr(fac)
	print 5,fac,fac
	if(discprt) write(8,5) fac,fac
5	format(
     &' Minimum usable open time (ms) calculated as duration',/,
     &'   of opening that reaches Acrit (pA)=sublevel + ',f5.1,'*SD',/,
     &' Minimum usable shut time (ms) calculated as duration ',/,
     &'   of shutting that reaches Acrit=full-(sublevel - ',f5.1,'*SD)'
     &,/,' If required level is < 0 or > 1, ''-'' is printed',/,
     & '    (If bisection gets stuck in loop, abort with F2)',/)
	print 6
	if(discprt) write(8,6)
6	format(
     &'  Sublevel    SD    ____ Open time____    ____ Shut time____',/,
     &'    (pA )    (pA)    Acrit   Duration      Acrit   Duration ')
	do k=1,nc-1			!go through each sublevel
	   sub=abs(amean(k,jset))
c	   print 7,k,sub
c7	   format(/,' For sublevel ',i2,': amplitude (pA) = ',f8.3)
c       First opening -must reach sub+fac*sd
	   s=sd(k,jset)
	   aco=sub+fac*s
	   aam=aco/full		!fractional deflection
	   bad1=(aam.ge.1.).or.(aam.le.0.)
	   xlo=0.0		!low guess for w
	   xhi=20.*trise
	   epsx=0.01	!0.01 microsec accuracy
	   ndisp=-2		!no printing
c
	   if(bad1) then
		w1=-1.
	   else
	      call BISEC0(AFUNC,xlo,xhi,aam,Xout,Yout,EPSx,epsy,
     &	   Nerr,Ndisp,.false.)
		w1=xout/1000.	!ms
	   endif
c	   print 22,xout,xout/trise,yout
c22	   format(
c     & '  -corresponding pulse width (microsec)= ',g13.6,' = ',g13.6,
c     & ' risetimes'/,
c     & ' (corresponds to A/Amax= ',g13.6,')')
c       Now shuttings
	   acs=full-(sub-fac*s)
	   aam=acs/full		!fractional deflection
	   bad2=(aam.ge.1.).or.(aam.le.0.)
	   xlo=0.0		!low guess for w
	   xhi=20.*trise
	   epsx=0.01	!0.01 microsec accuracy
	   ndisp=-2		!no printing
	   if(bad2) then
		w2=-1.
	   else
	      call BISEC0(AFUNC,xlo,xhi,aam,Xout,Yout,EPSx,epsy,
     &	   Nerr,Ndisp,.false.)
		w2=xout/1000.	!ms
	   endif
	   if(bad1.and.bad2) then
		print 71,k,sub,s,aco,acs
		if(discprt) write(8,71) k,sub,s,aco,acs
71		format(
     &	1x,i2,1x,f6.2,1x,f6.2,3x,f6.2,3x,'   -  ',5x,f6.2,3x,'   -')
	   else if(bad1.and.(.not.bad2)) then
		print 72,k,sub,s,aco,acs,w2
		if(discprt) write(8,72) k,sub,s,aco,acs,w2
72		format(
     &	1x,i2,1x,f6.2,1x,f6.2,3x,f6.2,3x,'   -  ',5x,f6.2,3x,f7.4)
	   else if((.not.bad1).and.bad2) then
		print 73,k,sub,s,aco,w1,acs
		if(discprt) write(8,73) k,sub,s,aco,w1,acs
73		format(1x,i2,1x,f6.2,1x,f6.2,3x,f6.2,3x,f7.4,5x,f6.2,3x,
     &		'   -   ')
	   else
		print 74,k,sub,s,aco,w1,acs,w2
		if(discprt) write(8,74) k,sub,s,aco,w1,acs,w2
74		format(1x,i2,1x,f6.2,1x,f6.2,
     &	3x,f6.2,3x,f7.4,5x,f6.2,3x,f7.4)
	   endif
	enddo
	ans='N'
	call DCASK('Do another calculation like this',ans,ans)
	if(ans.eq.'Y') goto 8
	RETURN
	END



