	subroutine AMAX(aam,trise,tpulse,nerr)
c To calc duration of event that reaches a fraction aam of max amplitude
c
	common/aafunc/ trise1
	external AFUNC
c
	trise1=trise	!for common/aafunc/
	xlo=0.0		!low guess for w
	xhi=20.*trise
	epsx=0.01	!0.01 microsec accuracy
	ndisp=-2	!no printing in BISEC0!
c
	call BISEC0(AFUNC,xlo,xhi,aam,Xout,Yout,EPSx,epsy,
     &	Nerr,Ndisp,.false.)
c	print 22,xout,xout/trise,yout
c22	format(
c     &	' Pulse width (microsec)= ',g13.6,' = ',g13.6,' risetimes'/,
c     &	'( corresponds to A/Amax= ',g13.6,')')
c
	tpulse=xout
	RETURN
	END

