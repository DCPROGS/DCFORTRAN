	subroutine SETATT(narrow,nline,ntext,isdev,
     & rlth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     & ndimd,ndimc,ifitype,imode)
c To set attributes of various parts of the graph in VPLOT/VHIST
c Modif 07/07/97 09:23pm so setbigj uses j=1,2,..,ncurv, rather than icurv(j)
c Modif 07/07/93 10:17am to use dialog box
c For histograms use icol(10) for end (dashed) bins
c imode=1, -1 to set line thickness
c imode=11 to set thicker line defaults
c imode=12 to set thinner line defaults
c imode=2 to set colour
c imode=-1,-2 excludes options not needed in VHIST
c for data
	dimension icurvd(ndimd)
c for calc curves
	dimension icurvc(ndimc)
	character ch*1
	integer Lb(30)
	character*78 text(18)
	integer icol(100)
c	integer*2 lrow,int2
	real  rlth(100) 		!for line thickness
	logical histo
	character defolt*30,cans*30	!to hold text & result of QDIALOG
	character cnum1*11		! for intconv
	common/DMENU/ifonb,csize,ifont2,nboxlast,nblast
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
c
3	format(i8)
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
	histo=imode.lt.0
	imode=iabs(imode)
	ict=11
c
c Increase line thickness
	if(imode.eq.11) then
	  do i=1,100
	    rlth(i)=rlth(i) + 0.25
	  enddo
 	  rLTH(23)=0.5		!labels
 	  rLTH(24)=0.5		!numbers
 	  rLTH(25)=0.5		!title
 	  rLTH(26)=0.25	!param value text
	  do i=31,50		!extra text
		rlth(i)=0.5
	  enddo
	  RETURN
	endif
c
c Decrease line thickness
	if(imode.eq.12) then
	  do i=1,100
	    if (rlth(i).ge.0.6) rlth(i)=rlth(i)-0.25
	  enddo
 	  rLTH(23)=0.5		!labels
 	  rLTH(24)=0.5		!numbers
 	  rLTH(25)=0.5		!title
 	  rLTH(26)=0.25	!param value text
	  do i=31,50
		rlth(i)=0.5
	  enddo
	  RETURN
	endif
c
	if(imode.eq.2) then
	   call COLBAR(1)
	endif
236	continue
	nbox=15
c	if(histo.and.imode.eq.2) nbox=12
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(nbox,Lb,1)
	if(ncurvd.eq.0) Lb(3)=0
	if(ncurvc.eq.0) Lb(4)=0
	if(narrow.eq.0) Lb(6)=0
	if(nline.eq.0) Lb(7)=0
	if(imode.eq.1) Lb(11)=0
	if(imode.eq.1) Lb(5)=0
	if(histo) Lb(9)=0
	Lb(12)=0
	if(histo) Lb(12)=1	!end bin
	Lb(13)=0	!not yet used
	Lb(14)=0	!not yet used
	Lb(15)=0	!not yet used

c Line thickness for whole page plots in LTH(i) as follows ( actual
c thickness =THICK*lth(i) were THICK=THBIG=1.0 for whole page)
c 1-10= data sets (#1-10) (symbol and joining-line if any);
c 11-20=calc curves (#1-10);
c 21=axes; 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols;
c 28=jump bar (logo) for C-jumps
c 29=jump bar (logo) for V-jumps; 30 spare
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c 71-100 =spare for future
2362	text(1)='1.  AXES      '
	text(2)='2.  FRAME     '
	if(histo) then
	   text(3)='3. HISTO LINES '
	else
	   text(3)='3. DATA LINES '
	endif
	text(4)='4. CALC CURVES'
	text(5)='5.  TEXT . . . '
	text(6)='6. ARROWS     '
	text(7)='7.EXTRA LINES '
	if(histo) then
	   text(8)='8.S(obs) ARROW'
	else
	   text(8)='8. SYMBOLS    '
	endif
c	text(9)='9.JUMP LOGOS'
c	text(10)='10. REDRAW  '
c	text(11)='+.BACKGROUND'
c	text(12)='-. END BINS  '
	text(9)='9. SD BARS'
	text(10)='10. REDRAW  '
	text(11)='+.BACKGROUND'
	if(histo) then
	   text(12)='-. END BINS  '
	else
	   text(12)='9.JUMP LOGOS'
	endif
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
2361	call CKEY(ch,ikey)
	if(ikey.eq.-16) goto 306		!ESC
	if(histo.and.ikey.eq.8) goto 256
c	pause '6'
c           1   2   3   4   5   6   7    8    9   10  11  12
	goto(251,252,253,254,242,256,2560,2561,2562,306,257,258) ikey
	goto 2361	!no valid key
c SET LINE THICKNESS
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
251	continue           		!axes
	if(imode.eq.1) then
	   r=rlth(21)
 	   call DEFOLTr( r,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans, r)
	   rlth(21)=r
	else if(imode.eq.2) then
 	   call DEFOLTi(icol(21),defolt)
	   call QDIALOG(1,
     &	'Colour; at present',defolt,ict,cans)
	   call GETINPi(cans,icol(21))
	endif
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,0,0)		!delete box # key
	goto 2362
252	continue
	if(imode.eq.1) then
	   r=rlth(22)
 	   call DEFOLTr(r,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans,r)
	   rlth(22)=r
	else if(imode.eq.2) then
 	   call DEFOLTi(icol(22),defolt)
	   call QDIALOG(1,
     &	'Colour; at present',defolt,ict,cans)
	   call GETINPi(cans,icol(22))
	endif
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 2361
253	continue
	if(ncurvd.gt.1) then
	   call SELDATA(ncurvd,j1,icol,0)		!choose which data line
	   if(j1.eq.-1) goto 236		!cancel
	else
	   j1=1
	endif
	if(j1.gt.0) then
	   j=icurvd(j1)
	   j2=j1
	else
	   j=icurvd(1)		!j1=0 means 'all sets'
	   j2=1
	endif
c	call SETBIGJ(j,j0)	!returns j0=j if j.le.10
	call SETBIGJ(j2,j0)	!returns j0=j if j.le.10
	if(imode.eq.1) then
	   rlt4=rlth(j0)
 	   call DEFOLTr(rlt4,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans,rlt4)
	  if(j1.gt.0) then
	     rlth(j0)=rlt4
	  else
	     do j=1,ncurvd
	      j1=icurvd(j)
		if(j.le.10) then
		   j0=j1
		else
		   j0=1+mod(j1-1,10)	!so j=11-20, 21-30 all -> j0=1-10 etc
		endif
	      rlth(j0)=rlt4
	     enddo
	  endif
	else if(imode.eq.2) then
	  ic=icol(j0)
 	   call DEFOLTi(ic,defolt)
	   call QDIALOG(1,
     &	'Colour; at present',defolt,ict,cans)
	   call GETINPi(cans,ic)
	  if(j1.gt.0) then
	     icol(j0)=ic
	  else
	     do j=1,ncurvd
	      j1=icurvd(j)
		if(j.le.10) then
		   j0=j1
		else
		   j0=1+mod(j1-1,10)	!so j=11-20, 21-30 all -> j0=1-10 etc
		endif
	      icol(j0)=ic
	     enddo
	  endif
	endif
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 2361
c	goto 2362
c
254	continue		!line thick for calc curves
	if(ncurvc.gt.1) then
	   call SELDATA(ncurvc,j1,icol,1)		!choose which calc line
	   if(j1.eq.-1) goto 236		!cancel
	else
	   j1=1
	endif
	if(j1.gt.0) then
	   j=icurvc(j1)
	   j2=j1
	else
	   j=icurvc(1)
	   j2=1
	endif
c	call SETBIGJ(j,j0)	!returns j0=j if j.le.10
	call SETBIGJ(j2,j0)	!returns j0=j if j.le.10
	if(imode.eq.1) then
	   r=rlth(j0+10)
 	   call DEFOLTr( r,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans, r)
	  if(j1.gt.0) then
	     rlth(j0+10)=r
	  else
	    do j1=1,ncurvd
	      j=icurvd(j1)
c		call SETBIGJ(j,j0)	!returns j0=j if j.le.10
		call SETBIGJ(j1,j0)	!returns j0=j if j.le.10
	      rlth(j0+10)=r
	    enddo
	  endif
	else if(imode.eq.2) then
	  ic=icol(j0+10)
 	   call DEFOLTi(ic,defolt)
	   call QDIALOG(1,
     &	'Colour; at present',defolt,ict,cans)
	   call GETINPi(cans,ic)
	  if(j1.gt.0) then
	     icol(j0+10)=ic
	  else
	     do j1=1,ncurvd
	      j=icurvd(j1)
c		call SETBIGJ(j,j0)	!returns j0=j if j.le.10
		call SETBIGJ(j1,j0)	!returns j0=j if j.le.10
	      icol(j0+10)=ic
	     enddo
	  endif
	endif
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 2361
c	goto 2362
C
2561	continue
	if(imode.eq.1) then
	   r=rlth(27)
 	   call DEFOLTr(r,defolt)
	   call QDIALOG(1,
     &	'Line thickness for symbol (mm); at present',
     &	defolt,ict,cans)
	   call GETINPr(cans,r)
	   if(r.gt.0.) rlth(27)=r
	else if(imode.eq.2) then
 	   call DEFOLTi(icol(27),defolt)
	   call QDIALOG(1,
     &	'Colour for symbols; at present',defolt,ict,cans)
	   call GETINPi(cans,icol(27))
	endif
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 2361
c
2562	continue
	if(isdev.eq.0.or.isdev.eq.1) then
	  if(imode.eq.1) then
	      r=rlth(30)
 	     call DEFOLTr(r,defolt)
	     call QDIALOG(1,
     &	'Line thickness for SD bars',defolt,ict,cans)
	     call GETINPr(cans, r)
	      rlth(30)=r
	  else if(imode.eq.2) then
 	    call DEFOLTi(icol(30),defolt)
	    call QDIALOG(1,
     &	'Colour for SD bars',defolt,ict,cans)
	    call GETINPi(cans,icol(30))
	  endif
	endif
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 2361
c
c Colour for histo end bins
258	continue
	if(histo) then
 	   call DEFOLTi(icol(10),defolt)
	   call QDIALOG(1,
     & 'End bin colour; at present',defolt,ict,cans)
	   call GETINPi(cans,icol(10))
	else
	   if(imode.eq.1) then
		ri=(rlth(28))
		rj=(rlth(29))
		call DEFOLT2r(ri,rj,defolt)
	  	call QDIALOG(1,'Line thickness for C-jump, V-jump logo',
     &	  defolt,ict,cans)
		call GETINP2r(cans,ri,rj)
		if(i.gt.0) then
	 	   rlth(28)=ri
	         rlth(29)=rj
		endif
	   else if(imode.eq.2) then
		call DEFOLT2i(icol(28),icol(29),defolt)
	 	call QDIALOG(1,'Color for C-jump, V-jump logo',
     &	  defolt,ict,cans)
		call GETINP2i(cans,icol(28),icol(29))
	   endif
	   goto 2361
	endif
c
c
c Set line thickness for text
c WHICH TEXT IS TO HAVE LINE THICKNESS CHANGED?
242	continue
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=10		!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	do 2421 i=1,5
2421	if(i.gt.ntext) Lb(i)=0
	if(ifitype.eq.0) Lb(7)=0	!no fitted values
	text(1)='1.NEW TEXT #1 '
	text(2)='2.NEW TEXT #2 '
	text(3)='3.NEW TEXT #3 '
	text(4)='4.NEW TEXT #4 '
	text(5)='5.NEW TEXT #5 '
	text(6)='6.NEW TEXT #n '	!for any number > 5
	text(7)='7.FITTED PARAM'
	text(8)='8.AXIS LABELS '
	text(9)='9.AXIS NUMBERS'
	text(10)='10.PLOT TITLE '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	ikey=nkey()
	if(ikey.eq.-16) goto 236		!ESC
	j=ikey+15	!for axis label,axis numbers,title,ikey=8,9,10; j=23,24,25
	if(ikey.ge.1.and.ikey.le.5) j=ikey+30	!extra text
	if(ikey.eq.6) then            !new text >5
	   call INTCONV(ntext,cnum1)
	   call QDIALOG(1,'New text number (6 - '
     &	  //CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,i)
	   j=i+30
	endif
	if(ikey.eq.7) j=26				!param values
	if(imode.eq.1) then
	    r=rlth(j)
 	   call DEFOLTr( r,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans, r)
	    rlth(j)=r
	else if(imode.eq.2) then
 	  call DEFOLTi(icol(j),defolt)
	  call QDIALOG(1,
     &   'Colour; at present',defolt,ict,cans)
        call GETINPi(cans,icol(j))
	endif
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 236
c
c line thickness for arrows
256	continue
	j=1
	if(histo.and.ikey.eq.8) goto 255	!Sval arrow=#1
	if(narrow.gt.1) then
	   call INTCONV(narrow,cnum1)
	   call QDIALOG(1,'Arrow number (1 - '
     &	  //CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,j)
	endif
255	if(imode.eq.1) then
	    rlt4=( rlth(j+50))
 	   call DEFOLTr( rlt4,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans, rlt4)
	    rlth(j+50)=( rlt4)
	else if(imode.eq.2) then
 	  call DEFOLTi(icol(j+50),defolt)
	  call QDIALOG(1,
     &   'Colour; at present',defolt,ict,cans)
        call GETINPi(cans,icol(j+50))
	endif
	goto 236
c
c line thickness for extra lines
2560	continue
	call INTCONV(nline,cnum1)
	call QDIALOG(1,'Extra line number (0 - '
     &  //CHARNB(cnum1)//')',' ',ict,cans)
	call GETINPi(cans,j)
	if(imode.eq.1) then
	    rlt4=( rlth(j+60))
 	   call DEFOLTr( rlt4,defolt)
	   call QDIALOG(1,
     &	'Line thickness (mm); at present',defolt,ict,cans)
	   call GETINPr(cans, rlt4)
	    rlth(j+60)=( rlt4)
	else if(imode.eq.2) then
c	  print 2,icol(j+60)
c	  call INPUTi(icol(j+60))
 	  call DEFOLTi(icol(j+60),defolt)
	  call QDIALOG(1,
     &   'Colour; at present',defolt,ict,cans)
        call GETINPi(cans,icol(j+60))
	endif
	goto 236
c
257	continue	!set background colour
 	call DEFOLTi(icol(71),defolt)
	call QDIALOG(1,
     &   'Colour; at present',defolt,ict,cans)
      call GETINPi(cans,icol(71))
	goto 236

c
306	continue	!return to redraw
	RETURN
	end

