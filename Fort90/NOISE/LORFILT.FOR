	subroutine LORFILT(ff,fc,R)
c To calculate reduction of variance resulting from filtering of
c Lorentzian by 8-pole Butterworth filter, as in Ogden & Colquhoun (1985)
c eqn A1.10.
c ff=filter freq (-3dB)
c fc=corner freq of Lorentzian
c
	f=ff/fc
	pi=3.14159265359
	r1=1.0/(1.0+(fc/ff)**16)	!first term
	r2=0.25*f*(1.0-f*f)		!multiplier
c now the sum
	s=0.0
	do 1 i=0,3
	 fi=float(i)
	 a=sin((2.*fi+1.)*pi/16.)
	 b=1.0 + f**4 + 2.*f*f*cos((2.*fi+1.)*pi/8.)
	 s=s + a/b
1	continue
c
	R=1.0/(r1 + r2*s)
	RETURN
	end

