	subroutine AAMAXSUB(trise,tres,aam)
c To calc resolution for shut times by Howe et al method in EKDIST
c
	common/aafunc/ trise1
	external AFUNC
c
	trise1=trise	!for common/aafunc/
30	continue
c
	print 3
3	format('&Amplitude of ''50pS'',''30pS'' openings= ')
	call INPUT2r(a50,a30)
c	input 2,a50,a30
c2	format(2g13.6)
	print 5
5	format('&2*standard deviation of ''30pS'' openings= ')
	call INPUTr(sd2)
c	input 4,sd2
c4	format(g13.6)
c
	a50=abs(a50)
	a30=abs(a30)
	aam=(a50-(a30-sd2))/a50
	print 21,aam
21	format('&A/Amax= ',g13.6)
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
c
	tres=xout
	aam=Yout	!actual value
	RETURN
	END



	function AFUNC(tval)
	common/aafunc/ trise
c
	zo=tval/trise
	AFUNC=erfs(0.88604*zo)
	return
	end

