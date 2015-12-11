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
C3	format('&Amplitude of ''50pS'',''30pS'' openings= ')
3	format(' Amplitude of MAIN level (pA) = ')
	call INPUTr(a50)
	print 31
31	format('&Amplitude of SUB level (pA) = ')
	call INPUTr(a30)
	print 5
5	format('&standard deviation of sublevel amp distribution = ')
	call INPUTr(sd)
	sd2=2.*sd
c
	a50=abs(a50)
	a30=abs(a30)
	aam=(a50-(a30-sd2))/a50
	print 21,aam
c21	format('&A/Amax= ',g13.6)
21	format(
     & ' Fractional deflection equiv to 2 SD below sublevel = ',g13.6)
	xlo=0.0		!low guess for w
	xhi=20.*trise
	epsx=0.01	!0.01 microsec accuracy
	ndisp=-1
c
	call BISEC0(AFUNC,xlo,xhi,aam,Xout,Yout,EPSx,epsy,
     &	Nerr,Ndisp,.false.)
	print 22,xout,xout/trise,yout
22	format(
     & '  -corresponding pulse width (microsec)= ',g13.6,' = ',g13.6,
     & ' risetimes'/,
     & ' (corresponds to A/Amax= ',g13.6,')')
c
	tres=xout
	aam=Yout	!actual value
	RETURN
	END
