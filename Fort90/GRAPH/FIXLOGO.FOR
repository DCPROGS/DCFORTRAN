	subroutine FIXLOGO(text,xmin,xmax,njump,t1,t2,
     & xoff1,idev,Lthk,y1,y2,idraw,ibk,icc)
	real*4 t1(10),t2(10)	!for jump logos and t=0 def.
	real*4 Lthk
	character ch,getch
	integer Lb(12)
	character*78 text(12)
c
	call DCMENU(0,5,Lb,text,0,0)	!delete all
	call SETLBOX(2,Lb,1)
	text(1)=
     & 'Move=up/down ARROWS; Deeper/shallower=+,-; invert=I'
      text(2)=
     & 'Delete/undel=DEL; hi/low sensitivity=INS; end=ENTER'
c	call DRAWBOX(1,Lb,text,2.5,ifont)
	call DCMENU(2,1,Lb,text,14,8)
c
	idraw=1
c Start with course movements
	d=0.1*abs(y2-y1)	!for move
c Look for a keystroke
1	continue
	ch=GETCH(ktype)
	ival=ichar(ch)
c Test whether to finish
	if(ktype.eq.16) goto 99				!ESC
	if(ktype.eq.4.and.ival.eq.13) goto 99	!ENTER
c Toggle the sensitivity
	if(ktype.eq.8.and.ival.eq.82) then	!INS toggles fine/course
	   if(d.eq.abs(y2-y1)) then
		d=0.1*abs(y2-y1)	!for move
	   else
		d=abs(y2-y1)
	   endif
	goto 1	!another key
	endif
c Next keys that require logo deletion and redraw
c First delete logo (call with idraw=-1)
	call JLOGO(xmin,xmax,njump,t1,t2,
     & xoff1,idev,Lthk,-1,y1,y2,ibk)
c Check which key hit
	if(ktype.eq.8) then
c Section for ktype=8 (arrow key)
c calc new position
	   if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!up
		y1=y1+d
		y2=y2+d
	   else if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then	!down
		y1=y1-d
		y2=y2-d
	   else if(ival.eq.83) then			!DEL toggles idraw
		if(idraw.eq.1) then
		   idraw=-1
		else
		   idraw=1
		endif
	   endif
	else if(ktype.eq.3) then
	   p1=d/2.
	   if(y1.lt.y2) p1=-d/2.
	   if(ival.eq.43) then 		!plus
		y1=y1+p1
		y2=y2-p1
	   else if(ival.eq.45) then		!minus
		y1=y1-p1
		y2=y2+p1
	   endif
c
	else if(ktype.eq.2) then
	   if(ival.eq.73.or.ival.eq.105) then	 !'i' or 'I' inverts logo
		p= y1
		 y1= y2
		 y2=p
	   endif
	endif
c and redraw
	call lincols(icc,idev)
	call JLOGO(xmin,xmax,njump,t1,t2,
     & xoff1,idev,Lthk,idraw,y1,y2,ibk)
	goto 1	!another move?
c
99	continue
	RETURN
	end

