	subroutine CURUNIT(ymin,ymax,ytic,ifont,units,nu,scale)
c To output appropriate units for current in UNITS, and the string length
c =nu eg to define title=' current '//units(1:nu).  The input values
c of ymin,ymax, in units specified by input value of 'units' are 1st rounded
c with FIXAX to give scale that would be used if no change of scale was
c made; new units are then defined if necessary (if ymin,ymax not in range
c 1-1000), and ymin, ymax multiplied by appropriate SCALE factor.
c
c Modif 10/27/95 03:36pm -ranges in which scaling occurs are altered!
c NB after exit must multiply all data by SCALE too!
	character units*8,grk*2,fnt*2
c
	grk=char(6)//'A'		!to set greek font
	fnt=char(6)//char(ifont+48)	!to set default font for string
c
c	ymin1=ymin
c	ymax1=ymax
c Debatable whether initial call to FIXAX is desirable. When its in you
c get plots going eg from -0.5 to 1.0 nA with all other labels = 0.1 ...
c whereas 100....pA  might look better.  If this call to FIXAX is
c omitted then initial graph in such a case will be labelled 'nA*1.e-3'
c going from -0.5 to 1.0, but now if its rescaled to say -500 to 900 then
c numbers revert to nA (100, 200 etc as req)
	call FIXAX(ymin,ymax,ymin1,ymax1,ytic,0)		!non-log assumed
	scale=1.0
	ym=abs(ymax1)
	if(abs(ymin1).gt.ym) ym=abs(ymin1)
	ym=ym-1.e-5		!???
c	if(ym.lt.1.e-3) then
c	   scale=1.e6
c	else if(ym.ge.1.e-3.and.ym.lt.1.0) then
c	   scale=1.e3
c	else if(ym.ge.1.e3.and.ym.lt.1.e6) then
c	   scale=1.e-3
c	else if(ym.ge.1.e6) then
c	   scale=1.e-6
c	endif
	if(ym.lt.1.e-4) then
	   scale=1.e6
	else if(ym.ge.1.e-4.and.ym.lt.0.1) then
	   scale=1.e3
	else if(ym.ge.1.e4.and.ym.lt.1.e7) then
	   scale=1.e-3
	else if(ym.ge.1.e7) then
	   scale=1.e-6
	endif
c Now define UNITS depending on scale factor, and on input units
	is1=ifixr(alog10(scale)/3.0)	!=-2,-1,0,1,2='no of steps changed
	if(units(1:4).eq.'(pA)') then
	   if(is1.eq.1) units='(fA)'
	   if(is1.eq.-1) units='(nA)'
	   if(is1.eq.-2) units='('//grk//'m'//fnt//'A)'   !microamps
	else if(units(1:4).eq.'(nA)') then
	   if(is1.eq.1) units='(pA)'
	   if(is1.eq.-1) units='('//grk//'m'//fnt//'A)'   !microamps
	else if(units(1:3).eq.'('//grk) then
	   if(is1.eq.2) units='(pA)'
	   if(is1.eq.1) units='(nA)'
	endif
	nu=4
	if(units(1:3).eq.'('//grk) nu=8
c	ymax=scale*ymax1
c	ymin=scale*ymin1
c	ytic=scale*ytic
c Scale the original ymin,ymax, and do FIXAX again to get new scale
	ymax1=scale*ymax
	ymin1=scale*ymin
	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)		!non-log assumed
	RETURN
	end
