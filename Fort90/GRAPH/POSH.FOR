
c POSH PLOT SECTION
	subroutine posh()
c====================================================================
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu
	common/hlp/help		!for QDIALOG
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,iangle,idraw,
     & ifnt,csfac,lth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel

c e.g.add/alter/move all text; add arrows or linetype legends;
c control line thickness (necessitates use of VTRANS at present)
c NB Need to store number of bits of extra text, arrows and their
c============================================================
c positions, so they can be reproduced when graph redrawn.
221	continue
	iret=0
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	if(.not.calbar) Lb(4)=0
	if(ncurvd.le.0) then
	   Lb(6)=0
	   Lb(7)=0
	   Lb(8)=0
	endif
	if(ncurvc.le.0) Lb(9)=0
	if(isdev.eq.-1) Lb(12)=0
	if(ncjump.eq.0.or.IVplot) Lb(13)=0
	if(nvjump.eq.0.or.IVplot) Lb(14)=0
	text(1)='1: ADD NEW TEXT'
 	text(2)='2: FIX TEXT   '
 	text(3)='3: FIX NUMBERS '
	text(4)='4.FIX CAL BAR(s)'
	text(5)='5:ARROWS & LINES '
	text(6)='6: SYMBOL TYPE'
	text(7)='7: SYMBOL SIZE'
	text(8)='8:FIX DATA LINE'
	text(9)='9:FIX CALC LINE'
	text(10)='10. REDRAW   '
	if(doframe) then
	 text(11)='+: OMIT FRAME'
	else
       text(11)='+: DRAW FRAME'
	endif
	text(12)='-:SHOW/HIDE SD'
	text(13)='x:FIX C-JUMP LOGO'
	text(14)='/:FIX V-JUMP LOGO'
	text(15)='.:LINE THICKNESS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
c   		1   2   3   4   5  6   7   8   9   10  11 12
	goto(241,232,235,233,234,116,116,116,116,306,12,404,
     &	402,402,236) ikey
	goto 221	!no valid key
c
c Section to change line type etc
c ikey=6 for symbol type; ikey=7 for symbol size
c ikey=8 for lines joining data points; ikey=9 for calc curve lines
116	continue
	call FIXCURV(ikey,ncurvd,icurvd,isym,syms,ijoin,
     & ncurvc,icurvc,iline,icol,lth,ndimd,ndimc)
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 221	!back to menu
c
c Section to alter bars (logos) that represent jumps
402	continue
	if(ikey.eq.13) call FIXLOGO(text,xmin,xmax,ncjump,t1c,t2c,
     & xoff1,idev,LTH(28),y1c,y2c,idraw(28))
	if(ikey.eq.14) call FIXLOGO(text,xmin,xmax,nvjump,t1v,t2v,
     & xoff1,idev,LTH(29),y1v,y2v,idraw(29))
	goto 221
c
c Toggle error bars
404	continue
	call DCMENU(-8,4,Lb,text,0,0)		!delete box 8 only
c	call DELBOX1(8)	!delete single box
	id=1
	if(ncurvd.gt.1) then
	   call SELDATA(ncurvd,id,icol,0)		!choose which data line
	   if(id.eq.-1) goto 221		!cancel
	endif
	if(id.eq.0) then 		!all sets
	   j1=1
	   j2=ncurvd
	else
	   j1=id
	   j2=id    !set id only
	endif
	do j0=j1,j2
	   j=icurvd(j0)
	   if(icurvw(j).eq.0) then
		icurvw(j)=1
	   else if(icurvw(j).eq.1) then
		icurvw(j)=0
	   else if(icurvw(j).eq.-1.and.j0.eq.id) then
		call BELL(1)
		call WDIALOG(1,'SD not defined for this set',12)
		goto 221
	   endif
	enddo
	goto 221	!more options?
c
12	doframe=.not.doframe	!toggle frame
	goto 221	!more changes?
c
233	continue
	call FIXCBAR(rx,ry,lth(21),idev,itx,ity,ntx,nty,itlen,idraw(1))
c Modif 04/18/95 05:16pm so that if text deleted (idraw=0) it is not redrawn
	if(idraw(8).ne.0) idraw(8)=-2       !X axis label drawn at default posn
	if(idraw(9).ne.0) idraw(9)=-2       !Y axis label drawn at default posn
	goto 306		!redraw
c
c USE FIXTEXT OPTION HERE
232	continue
	nbox=15
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(nbox,Lb,1)
	text(1)='F1:Mouse help'
	text(2)='Left butt:move'
	text(3)='Cent butt:select'
	text(4)='Right butt:esc'
	text(5)='<CR>:end select'
 	text(6)='->:Move right '
 	text(7)='<-:Move left '
	text(8)='/\,\/,:Up,Down'
	text(9)='+,-:Inc,Dec size'
	text(10)='x,/:Inc,Dec font'
	text(11)='PgUp:Inc angle'
	text(12)='PgDn:Dec angle'
	text(13)='Ins:Sensitivity'
	text(14)='Del:Delete'
	text(15)='b:Boxes'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)

c	imd=1 		! move text
	imd=0             ! move frame only
      call GINOTEXT(parval,xtitle,ytitle,title1,cnumx,cnumy,cexpx,
     &  cexpy,newtext,numbx,numby,ntext,inumx,inumy,logx,logy,
     &  rx,ry,ijus,icol,iangle,ifnt,idraw,size,icol(71),imd)
c 	Old method for locating boxes, as in VPLOT4, now removed.

	goto 221
c
c FIXNUMB OPTION
235	continue
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(5,Lb,1)
	text(1)='1. ONE NUMBER '
	text(2)='2. ALL X AXIS '
	text(3)='3. ALL Y AXIS '
	text(4)='4.ALL NUMBERS '
	text(5)='5.  DONE      '
	call DCMENU(5,5,Lb,text,icol1,icol2)
	call CKEY(ch,imode)
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(2,Lb,1)
	if(imode.eq.1) then		!define which number to alter
	   call LOCTEXT(n,80,iangle,rxbox,rybox,idraw)
	   if(n.eq.0) goto 221	!give up
	   text(1)=
     &   'ARROWS=move; R,r=rotate; '
         text(2)=
     & ' DEL=toggle delete; INS=toggle sensitivity; ESC/ENTER=end'
	else if(imode.eq.2.or.imode.eq.3) then
         text(1)=
     & 'ARROWS/F2=move; DEL=delete/undel; INS=sensit; ESC/ENTER=end'
	   Lb(2)=0
	else if(imode.eq.4) then
	   Lb(2)=0
         text(1)=
     & '+,-=size; /*=font;  INS=sensitivity; ESC/ENTER=end'
	else if(imode.eq.5) then
	   goto 221
	else
	   goto 235		!no valid imode
	endif
	call DCMENU(2,1,Lb,text,icol1,icol2)
c==============================================================
c	call GINONUMB(imode,n,size(7),ifnt(7),inumx,inumy,
c     & logx,logy,rx,ry,iangle,ijus,idraw,rxbox,rybox,
c     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
c	if(debug()) print 701,(idraw(i),i=31,40),(idraw(i),i=56,65)
c701	format(10i3,/,10i3)
	if(n.eq.-1) goto 235		!not an axis number box
c All idraw() for numbers to be drawn should be no longer set to -2
c so numbers should be redrawn at new posn in LAXES
	goto 221

c==================== Add/delete Arrow
234	continue

c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	ixfix=0		!x not fixed
	iyfix=0		!y not fixed
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15		!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
c	Lb(11)=0
	Lb(12)=0
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
2340	continue
	if(narrow.eq.0) Lb(2)=0
	if(nline.eq.0) Lb(7)=0
	if(nhline.eq.0) Lb(8)=0
	if(nvline.eq.0) Lb(9)=0
	text(1)='1:ADD AN ARROW  '
	text(2)='2:DELETE ARROW  '
	text(3)='3:ADD FREE LINE '
	text(4)='4:ADD HOR. LINE '
	text(5)='5:ADD VERT. LINE'
	text(6)='6:ADD GRID LINES'
	text(7)='7:DEL. FREE LINE  '
	text(8)='8:FIX HOR. LINE   '
	text(9)='9: FIX VERT. LINE '
	text(10)='10:  FIX GRID     '
	text(11)='PgUp:PREVIOUS MENU'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.lt.1.or.ikey.gt.15) goto 221	!no valid key
c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
c===================================================================
	if(ikey.eq.1.or.ikey.eq.2) then
	   call draw_arrow(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &   xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,lth)
	else if (ikey.ge.3.and.ikey.le.5) then
	   call draw_lines(ikey,nline,xlb,xle,ylb,yle,iltype,
     &   nhline,yhline,xhlb,xhle,ilhtype,
     &   nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     &   csize,idev,xabs1,yabs1,xabs2,yabs2,lth,
     &   xmin,xmax,ymin,ymax)
	else if(ikey.eq.6.or.ikey.eq.10) then    !add/fix grid (hor + vert lines)
	   call BELL(2)
	   call WDIALOG(1,'Grid not yet fixed',12)
	else if(ikey.eq.7.or.ikey.eq.8.or.ikey.eq.9) then           !fix lines
	   if(ikey.eq.7.and.nline.eq.0) goto 234	!nothing to fix
	   if(ikey.eq.8.and.nhline.eq.0) goto 234		!nothing to fix
	   if(ikey.eq.9.and.nvline.eq.0) goto 234		!nothing to fix
	   call FIXLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     &	 nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     &	 nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,
     & 	 lth,icol,icol(71))
	else if(ikey.eq.11)
	     goto 221
	endif

	goto 234

c========================================================================
c Line thickness
236	continue
	imode=1	!for line thickness
	call SETATT(narrow,nline,ntext,isdev,
     & lth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     & ndimd,ndimc,ifitype,imode)
	goto 306	!redraw

c========================================================================
c Change default size/font
c===MOVE THIS TO MAIN MENU 'DEFAULTS'
37	continue
	call DEFOLTi(ifont,defolt)
	call QDIALOG(1,'New font number (0-11,A,B)',defolt,ict,cans)
	call GETINPi(cans,ifont)
c	ifsav=ifont
	ifont2=ifont	!copy for common/dmenu/
	call DEFOLTr(csize,defolt)
	call QDIALOG(1,'New font size (0.5-18)',defolt,ict,cans)
	call GETINPr(cans,csize)
c Reset all defaults too (will override existing alterations): try changing
c only those that are set to current default
	do 84 i=1,30
	if(ifnt(i).ne.ifont) then
c================================
	   ifnt(i)=ifont
c	   csfac(i)=csize1/csize
	endif
84	continue
	ifont=ifont
	ifont2=ifont	!copy for common/dmenu/
c	csize=csize1
	call CHAFNT(ifont)
c	cbig=csize				!change default 'whole page' size

c	call SETSIZE(csize,isize)    !sets isize too
	goto 221
c
c============================================================================
c ADD TEXT
241	continue
	if(ntext.le.19) goto 2411	!OK- room for another one
c All 20 newtext arrays used; must delete one in order to add more
c Delete again specified bits of text that have been added
c (This bit deletes NEWTEXT for good and frees the space for another bit
c of new text, whereas FIXTEXT just sets idraw=0 so not drawn (rather
c than deleted)
	if(ntext.gt.19) then
	   call WDIALOG(1,'CURRENT NEW TEXT STRINGS:',ict)
	   do i=1,ntext
		call INTCONV(i,defolt)
		n=NBLANK(newtext(i))
	      call WDIALOG(1,CHARNB(defolt)//':'// newtext(i)(1:n),ict)
	   enddo
2443	   call QDIALOG(1,
     &   'Must delete one existing text: delete # ',' ',ict,cans)
	   call GETINPi(cans,it)
	   if(it.lt.1.or.it.ge.20) goto 2443
	   j1=0  		!for newtext
	   j=10		!for position,font,size
	   do 2441 n1=1,ntext
		n=n1+10
		if(n.eq.it) goto 2441	!skip deleted one
		j=j+1
		j1=j1+1
		newtext(j1)=newtext(n1)
		idraw(j)=idraw(n)
		rx(j)=rx(n)
		rx(j)=ry(n)
		ifnt(j)=ifnt(n)
c		csfac(j)=csfac(n)
		size(j)=size(n)
		iangle(j)=iangle(n)
		ijus(j)=ijus(n)
		lth(j1+30)=lth(n+30)
		do m=1,4
		  rxbox(m,j)=rxbox(m,n)
		  rybox(m,j)=rybox(m,n)
		enddo
2441	   continue
	   idraw(ntext+10)=-2		!so skipped by LOCTEXT
	   ntext=ntext-1
	endif
c
c Continue to add new text:
2411	continue
	ntext=ntext+1		!count titles
	n=ntext+10
c Fonts if=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*if for upright, or ifont=2*if+1 for italic
c ( for greek use 'A', 'B' in place of 10,11)
	call chaFNT(ifnt(n))
	call setsize(size(n))    !sets isize too
c NB Readstring is equivalent to call to FNDCUR followed by GETSTRING
c	call PUTCUR(5000,3500)		!start with cursor central
c	call READSTRING(ix,iy,newtext(ntext),
c     & iangle(n),isize)
	call WDIALOG(1,'Define start position for text with cursors',-ict)
	call CURPOS(xlo,ylo)
	call cursor(ikey,xc,yc)
      ch=char(ikey)	!note new usage
c Define all text string values (do once only, unless text altered), not
c everytime graph redrawn
	call spagra(xc,yc,rx(n),ry(n))
	ijus(n)=left
	idraw(n)=1
	ic=icol(30+ntext)
	if(ic.eq.0) then
	   ic=14
	   icol(30+ntext)=ic
	endif
	call WDIALOG(1,'Now write text: hit F1 anytime for HELP     ',ict)
	call GSTRING(xc,yc,newtext(ntext),ifont,iangle(n),size(n),
     &	ijus(n),ic,icol(71),80,.true.)
	nl=NBLANK(newtext(ntext))
	if(nl.ge.1) then
	   call graspa(rx(n),ry(n),xsc,ysc)
	   call TEXTBOX(xsc,ysc,newtext(ntext)(1:nl),iangle(n),size(n),
     &   ijus(n),rxbox(1,n),rybox(1,n),0)
	else
	   ntext=ntext-1		!don't count empty strings!
	endif
c
c Reset current values
	call CHAFNT(ifont)	!reset current values
	call setsize(size0)    !sets isize too
	goto 221

c==============================================================================
