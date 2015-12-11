	subroutine SETFONT1(ifont)
c This is DC subroutine to set font specified on 0-11 scale
c Fonts if=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*if for upright, or ifont=2*if+1 for italic
c ( for greek use 'A', 'B' in place of 10,11 when typed after ^F)
	logical it
c
	if(ifont.lt.0) ifont=0
	if(ifont.gt.11) ifont=11
	if0=ifont/2
	it=mod(ifont,2).eq.1
	call ITALIC(it)
	call setfnt(if0)
c	call SETFONT(if0,it)
	RETURN
	end

