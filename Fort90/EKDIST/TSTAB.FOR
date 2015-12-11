	real tint(10),ampl(10),xv(10),yv(10)
	logical badgap(10)
c
	do i=1,10
	   badgap(i)=.false.
	enddo

	tint(1)=3.
	ampl(1)=-1.

	tint(2)=0.1
	ampl(2)=0.

	tint(3)=2.1
	ampl(3)=-2.

	tint(4)=2.1
	ampl(4)=-3.

	tint(5)=2.
	ampl(6)=0.

	tint(6)=3.
	ampl(6)=-4.

	tint(7)=2000.
	ampl(7)=0.

	tint(8)=2.
	ampl(8)=-5.

	tint(9)=2.
	ampl(9)=0.
	badgap(9)=.true.

	tint(10)=2.
	ampl(10)=-5.
c
	a=2.
c
	  i1=0		!count valid values
	  xv(1)=1.0		!needed for pseudo-time Xvals
	  do 34 i=1,10
	   amp=ampl(i)
	   deltx=1.0
	   if(amp.eq.0.) then
		   if(tint(i).le.1.0) then
			deltx=1.0
		   else
			if(badgap(i)) then
			   deltx=a*3.		!like 1000ms if bad
			else
			   deltx=1.0 +a*alog10(tint(i))
			endif
		   endif
	   	   goto 34		!exclude zero amps

c===
c	   else if(i.gt.1.and.ampl(i-1).ne.0.) then
c		deltx=1.0
c===
	   endif
c
	   if(badgap(i)) goto 34			!exclude neg/unusable always
c
	      i1=i1+1
	      yv(i1)=amp
	      if(i1.gt.1) xv(i1)=xv(i1-1)+deltx
34	  continue
	pause
	end
