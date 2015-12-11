	program AAMAX
c To calc A/Amax for given t (given Trise)
c OR to calculate t for given A/Amax
c
	common trise
	external AFUNC
c
30	print 1
1	format(' To calculate A/Amax=f(w); w=pulse duration',/,
     &	' (1) Specify w and calculate A/Amax',/,
     &	' (2) Specify A/Amax and calculate w',/,
     &	' Option number [2] = ')
	read 2,nopt
2	format(i8)
	if(nopt.eq.0) nopt=2
c
	print 3
3	format(' Risetime (microsec)= ')
	call INPUTr(trise)
c
	if(nopt.eq.2) goto 20
c
	print 5
5	format(' Pulse length, w (microsec)= ')
	call INPUTr(tval)
	zo=tval/trise
	aam=erfs(0.88604*zo)
	print 6,aam
6	format(' A/Amax= ',g13.6)
	goto 30
c
20	continue
	print 21
21	format(' A/Amax= ')
	call INPUTr(aam)
	xlo=0.0		!low guess for w
	xhi=20.*trise
	epsx=0.01	!0.01 microsec accuracy
	ndisp=-1
c
	call BISEC0(AFUNC,xlo,xhi,aam,Xout,Yout,EPSx,epsy,
     &	Nerr,Ndisp,.false.)
	print 22,xout,xout/trise,yout
22	format(
     &	' Pulse width (microsec)= ',g13.6,' = ',g13.6,' risetimes'/,
     &	'( corresponds to A/Amax= ',g13.6,')')
	goto 30
c
	END



	function AFUNC(tval)
	common trise
	zo=tval/trise
	AFUNC=erfs(0.88604*zo)
	return
	end

