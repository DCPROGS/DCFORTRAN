	program TBOX1
c To demonstrate DRAWBOX
	integer Lb(12),Lblast(12)		!lbdel not used?
	character*78 text(12),textlast(12)
c	common/DBOX/Lblast,textlast,nblast,cslast,iflast,ihb
	common/DBOX/Lblast,nblast,cslast,ihb,csizb,ifonb,ifont
c	common/DBOX/Lblast,nblast,cslast,ihb,csizb,ifont,4,icol1,icol2
c
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c	print 11
c11	format(' icol1,icol2,ifont,csize = ')
c	read *,icol1,icol2,ifonb,csizb
	icol1=14	!yellow
	icol2=8	!grey
	ifonb=0
	csizb=1.7
c
	ifont=4
	call INIPLT(0,.false.,1.0)
	call SETLBOX(8,Lb,1)
	Lb(5)=0
	Lb(6)=-1	!italic
151	continue
	text(1)='1.  RESCALE      '
	text(2)='2. PLOT DISPLAY  '
	text(3)='3.QUEUE THE PLOT '
	text(4)='4.TITLE FOR PLOT '
	text(5)='5.MAKE POSH PLOT '
	text(6)='6.HISTO LINE TYPE'
	text(7)='7.CALC LINE TYPE '
	text(8)='8. END DISPLAY   '
	call DRAWBOX(4,Lb,text,icol1,icol2)
	iloc=nkey()
c
	goto(1,2,3,4,5,6,7,8) iloc
1	continue
2	continue
3	continue
4	continue
5	continue
6	continue
7	continue
8	continue
	pause
	call DELBOX1(4)	!delete single box
	pause
c	call DRAWBOX1(-4,text(4),11,icol2)	!redraw in italic
	call DRAWBOX1(-4,text(4),6,icol2)	!redraw in italic
	pause
	call DELBOX(Lblast)	!delete all drawn at last DRAWBOX call
	pause
	end



	subroutine SETLBOX(nbox,Lbox,j)
c sets elements i=1,2,...,nbox of Lbox equal to j, remainder to zero
c which is the most commonly required input for DRAWBOX
	integer Lbox(12)
	do 1 i=1,12
1	Lbox(i)=0
	do 2 i=1,nbox
2	Lbox(i)=j
	RETURN
	end





	subroutine IFRAME(ixlo,ixhi,iylo,iyhi)
c To draw box (version of FRAME that uses device coordinates)
	call PLOT(ixlo,iylo,0)
	call PLOT(ixlo,iyhi,1)
	call PLOT(ixhi,iyhi,1)
	call PLOT(ixhi,iylo,1)
	call PLOT(ixlo,iylo,1)
	return
	end



	subroutine SETCSIZE(csize,isize)
c Lahey/Hgraph subroutine to set character size
c Input=csize (0.5-18.0)
c Output=isize(1-9)
	if((csize.ge.1.0).and.(csize.le.9)) then
	   isize=ifix(csize)
	   cfac=csize/float(isize)
	else if(csize.lt.1.0) then
	   isize=1
	   cfac=csize
	else if(csize.gt.9.) then
	   isize=9
	   cfac=csize/float(isize)
	endif
c in case:
	if(isize.lt.1) isize=1
	if(isize.gt.9) isize=9
	if(cfac.lt.0.5) csize=0.5
	if(cfac.gt.2.0) csize=2.0
c
	call setcharaspect(cfac,cfac)	!width, height
c	print 1,isize,cfac
c1	format(' isize,cfac= ',i5,f9.3)
	RETURN
	end





	subroutine SETFONT1(ifont)
c This is DC subroutine to set font specified on 0-11 scale
c Fonts if=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*if for upright, or ifont=2*if+1 for italic
c ( for greek use 'A', 'B' in place of 10,11 when typed after ^F)
	logical italic
c
	if=ifix(ifont/2)
	italic=mod(ifont,2).eq.1
	call SETFONT(if,italic)
	RETURN
	end


	function NKEY()
c	logical numlock,numon
	character ch,getch
	integer*2 ktype
c	character*1 ittinr,ch
c Modif to use
c To input a character without <enter> using UTILITY.LIB routine GETCH
c (waits for input).  This version accepts only
c (1) digits 0-9, for which NKEY=1-9, and 10 if zero hit
ccc (2) F1-F10, for which NKEY= -1 to -10- not now- give 1-10
c (2) F1-F10, for which NKEY= 1 to 10
c (3) ESC key, for which NKEY=16,
c otherwise waits for another character
c	numon=NUMLOCK()		!record input state of numlock
c	call NUMSET			!set num lock on (UTILITY library)
3	ch=GETCH(ktype)
	if(ktype.eq.3) then	! a digit (or +,-,.)
		i=ichar(ch)-48		! ASCII version of character input
	else if(ktype.eq.0) then	!function key
c		i=-(ichar(ch)-58)
		i=ichar(ch)-58
	else if(ktype.eq.16) then
		i=16
	else
		print *,' Invalid key'
		goto 3			!try again
	endif
c
	if(i.eq.0) i=10
	NKEY=i
c	if(.not.numon) call NUMCLR	!restore input state
	return
	end



