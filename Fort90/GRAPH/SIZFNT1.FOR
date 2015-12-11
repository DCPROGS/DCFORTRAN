	subroutine SIZFNT1(nsize,nfont,fnt,sze)
c MODIF of SIZFNT in which NFONT is already on 0-11 scale (so ITALIC
c parameter not needed- see manual, p57)
c To define two character*2 strings, to change font and chacter size,
c for concatenation into the string that is to be printed by
c JUSTIFYSTRING and WRITESTRING calls in Hgraph
c (See tests in THGLAB.FOR: note that the ISIZE parameter in WRITESTRING etc
c seems to be ignored if the string contains any ^Sn instructions, such
c that text written initially has size controlled by the last ^Sn instruction
c in the string, rather than by the isize parameter- thus string should
c start with an explicit ^Sn instruction if it contains any ^Sn later.)
c  Input:
c    nsize=1-9; nfont=0-11
c
	character*2 fnt,sze
c
c Size:  ASCII 19= ctrl-S = ^S
	sze=char(19)//char(nsize+48)
c
c Fonts:
c nfont=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*nfont for upright, or ifont=2*nfont+1 for
c italic (for greek use 'A', 'B' in place of 10,11)
c ASCII 6= ctrl-F = ^F
	fnt='*F'//'00'//char(48+nfont)
c
	RETURN
	end



