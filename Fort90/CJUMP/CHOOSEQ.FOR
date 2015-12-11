c=======================================================================
	subroutine chooseq(x,y,nv,klv,kiwv,kihv,ixm,iym,nbut,klm,kihm,kiwm
     &    ,xc,yc,irc,xcm,ycm,icbox,icv,icm,icc,icb,icseq,icon,msol,nsol,
     &     countsec,ixw,iyw)
c=============================================================
c	input  : nv,nbut,icbox,icv,icm,icc,icb,icseq,icon
c     output : x,y,klv,kihv,kiwv,ixm,iym,klm,kihm,kiwm,xc,yc,irc,xcm,ycm,
c              msol,nsol
c=============================================================
	integer*4  	x(nv),y(nv),msol(50),xc(nv),yc(nv),countsec(50)
      integer*4   iym(5),ixm(5),xcm(5),ycm(5),ixbox(4),iybox(4)
	integer*1	itim(:)
	character  		a,getch,UC
	character*11	cnum
	character    	cans*30,def*30
	character*35 	helpst(20)
	allocatable::itim
	logical mouse_on,mous_set,outext,kbhit
	common/mousval/mouse_on,nbutton

	allocate(itim(20000))
	do i=1,nv
		countsec(i)=2
	enddo
	irhi=1
	iclo=45
	nrow=4
	ncol=32
	ibkd=0
	icb1=1
	icqd=14
      call DEFDIALOG(1,irhi,iclo,nrow,ncol,ibkd)
	icw=12
	nhelp=1
	helpst(nhelp+1)='CHOOSE SEQUENCE:'
	helpst(nhelp+2)='Move cursor/mouse to select valve'
	helpst(nhelp+3)='Press <CR> or click L mouse button'
	helpst(nhelp+4)='Press <Delete> or click on "Cancel"'
	helpst(nhelp+5)=' to cancel sequence'
	helpst(nhelp+6)='Press <End> or click on "On/Off" to'
	helpst(nhelp+7)=' start program'
	helpst(nhelp+8)='Press <Esc> or click R mouse button'
	helpst(nhelp+9)=' to exit program'
	nhelp1=nhelp+9

	ixlo=0
	iylo=0
	ixhi=640
	iyhi=480
	call valvebox(ixlo,iylo,ixhi,iyhi,nv,x,y,klv,kihv,kiwv,
     & nbut,ixm,iym,klm,kihm,kiwm,xc,yc,irc,xcm,ycm,
     & icbox,icv,icm,icc,icb,ncol,iyhi1)
	call zsymbol(x(1)+int(0.7*klv),y(1)+int(0.8*kihv),8,6,6,4,12)
	l=1
	k=1
	idy=int(4*kihv)
200	call wrstring18(ixlo+16,iyhi-32,
     &   'ENTER SEQUENCE ( F1 = HELP ) :',icseq,icc)
	ixw=ixlo+16
	iyw=iyhi-48
	isol=1
	ixloh=ixlo+50     !position for help box
	iyhih=iyhi-50
	if(ixhi.gt.639-50) ixloh=ixlo-50
	if(iyhih.gt.479) iyhih=470
	ibk0=icv
100	continue
	if(kbhit()) then
	a=getch(ktype)
	ival=ichar(a)
      if(ktype.eq.0.and.ival.eq.59) then   !help
		call hide_mouse()
		call fillcircle(xcm(2),ycm(2),irc,icb,icseq)
		call fillwin(ixm(2),iym(2),ixm(2)+klm+int(kiwm/2),
     &      iym(2)+kihm+int(kiwm/2),icbox)
		call drawrec(ixm(2)+int(kiwm/2),iym(2)+int(kiwm/2),
     &  	ixm(2)+klm+int(kiwm/2),iym(2)+kihm+int(kiwm/2),icbox,icm)
		call wrstring18(ixm(2)+int(klm/2),iym(2)+int(kihm/2),
     &	'Help',icc,icm)
		call wrstring18(ixm(2)+int(klm/2)+8,iym(2)+int(kihm/2)+16,
     &      '?',icw,icm)
		call POPTEXT(ixloh,-1,iyhih,helpst,nhelp1,14,1,15)	!=popkeys
		call fillcircle(xcm(2),ycm(2),irc,icb,icc)
	      call draw_valve(ixm(2),iym(2),klm,kihm,kiwm,icm,icv,icc)
		call wrstring18(ixm(2)+klm/2-8,iym(2)+kihm/2-16,'Help',
     &      icc,icm)
		call wrstring18(ixm(2)+int(klm/2),iym(2)+int(kihm/2),
     &      '?',icw,icm)
		call show_mouse()
		goto 100
	else if(ktype.eq.4.and.ival.eq.13) then		!enter
		call fillcircle(xcm(3),ycm(3),irc,icb,icc)
		call fillcircle(xcm(2),ycm(2),irc,icb,icc)
	   goto 101
	else if(ival.eq.27) then		!ESC
	   do i=1,nv
		   call fillcircle(xc(i),yc(i),irc,icb,icc)
	   enddo
	   call fillwin(ixlo+8,iyhi1+8,ixhi-8,iyhi-8,icc)
	   goto 99
	else if(ktype.eq.8) then		!up/down arrow to scroll highlight
		call fillcircle(xcm(2),ycm(2),irc,icb,icc)
	   if (ival.eq.83) then 		!delete
		if (isol.lt.2) goto 100
		call hide_mouse()
		call timer(it1)
	      call fillwin(ixlo+16,iyw,ixhi-16,iyw+16,icc)
	      ixw=ixlo+16
		isol=1
	      do i=1,25
		   call fillcircle(xc(i),yc(i),irc,icb,icc)
	      enddo
		call fillwin(ixm(3),iym(3),ixm(3)+klm+int(kiwm/2),
     &      iym(3)+kihm+int(kiwm/2),icbox)
		call drawrec(ixm(3)+int(kiwm/2),iym(3)+int(kiwm/2),
     &  	ixm(3)+klm+int(kiwm/2),iym(3)+kihm+int(kiwm/2),icm,icv)
		call fillcircle(xcm(3),ycm(3),irc,icb,icseq)
		call wrstring18(ixm(3)+klm/2-8,iym(3)+kihm/2+16,'Cancel',
     &	icc,icm)
		call wrstring18(ixm(3)+int(klm/2)+8,iym(3)+int(kihm/2)+24,
     &      'X',4,ibc)
88		call timer(it2)
		idt=it2-it1
		if(idt.lt.30) goto 88
		call fillcircle(xcm(3),ycm(3),irc,7,8)
	      call draw_valve(ixm(3),iym(3),klm,kihm,kiwm,icm,icv,8)
		call wrstring18(ixm(3)+klm/2-16,iym(3)+kihm/2,'Cancel',icc,
     &	icm)
		call wrstring18(ixm(3)+int(klm/2),iym(3)+int(kihm/2)+8,
     &      'X',icw,icm)
		call show_mouse()
	      goto 100
	   endif
	   if (ival.eq.79) then     !start
		if(isol.gt.1) goto 103
	      goto 100
	   endif
	   call movcross(x(1)+int(0.7*klv),y(1)+int(0.8*kihv),6,6,idy,
     &	nbut,int(1.45*klv),10,4,ibk0,8,1,l,k,ival)
		goto 100
	else
		goto 100
	endif
101	if (l.le.nbut.and.k.le.ncol) then
	    msol(isol)=k+(l-1)*ncol
	    if(msol(isol).le.nv.and.isol.le.50) then
		 call intconv(msol(isol),cnum)
		 call wrstring18(ixw,iyw,cnum,icseq,
     &	 icc)
		 call fillcircle(xc(msol(isol)),yc(msol(isol)),irc,icb,
     &       icseq)
		 ixw=ixw+24
		 isol=isol+1
		 if(isol.gt.25) then
		    iyw=iyw-16
		    ixw=ixlo+16
		 endif
	    endif
	else
	    if(l.eq.2) then    		!help
		call fillcircle(xcm(2),ycm(2),irc,icb,icon)
		call fillwin(ixm(2),iym(2),ixm(2)+klm+int(kiwm/2),
     &      iym(2)+kihm+int(kiwm/2),icbox)
		call drawrec(ixm(2)+int(kiwm/2),iym(2)+int(kiwm/2),
     &  	ixm(2)+klm+int(kiwm/2),iym(2)+kihm+int(kiwm/2),icv,icm)
		call wrstring18(ixm(2)+int(klm/2),iym(2)+int(kihm/2),
     &	'Help',icc,icm)
		call wrstring18(ixm(2)+int(klm/2)+8,iym(2)+int(kihm/2)+16,
     &      '?',icw,icm)
		call POPTEXT(ixloh,-1,iyhih,helpst,nhelp1,14,1,15)	!=popkeys
		call fillcircle(xcm(2),ycm(2),irc,icb,icc)
	      call draw_valve(ixm(2),iym(2),klm,kihm,kiwm,icm,icv,icc)
		call wrstring18(ixm(2)+klm/2-8,iym(2)+kihm/2-16,'Help',icc,
     &	icm)
		call wrstring18(ixm(2)+int(klm/2),iym(2)+int(kihm/2),
     &      '?',icc,icm)
		call show_mouse()
	    else if(l.eq.3) then      !cancel
		if(isol.lt.2) goto 100
		call timer(it1)
	      call fillwin(ixlo+16,iyw,ixhi-16,iyw+16,icc)
	      ixw=ixlo+16
		isol=1
	      do i=1,nv
		   call fillcircle(xc(i),yc(i),irc,icb,icc)
	      enddo
		call fillcircle(xcm(3),ycm(3),irc,icb,icon)
		call fillwin(ixm(3),iym(3),ixm(2)+klm+int(kiwm/2),
     &      iym(3)+kihm+int(kiwm/2),icbox)
		call drawrec(ixm(3)+int(kiwm/2),iym(3)+int(kiwm/2),
     &  	ixm(3)+klm+int(kiwm/2),iym(3)+kihm+int(kiwm/2),icv,icm)
		call wrstring18(ixm(3)+klm/2-8,iym(3)+kihm/2+16,'Cancel',
     &	icc,icm)
		call wrstring18(ixm(3)+int(klm/2)+8,iym(3)+int(kihm/2)+24,
     &      'X',icw,icm)
887		call timer(it2)
		idt=it2-it1
		if(idt.lt.30) goto 887
		call fillcircle(xcm(3),ycm(3),irc,icb,icc)
	      call draw_valve(ixm(3),iym(3),klm,kihm,kiwm,icm,icv,icc)
		call wrstring18(ixm(3)+klm/2-16,iym(3)+kihm/2,'Cancel',icc,
     &	icm)
		call wrstring18(ixm(3)+int(klm/2),iym(3)+int(kihm/2)+8,
     &      'X',icw,icm)
		call show_mouse()
	    else
		 if (isol.gt.1) goto 103
	    endif
	endif
	goto 100
	endif		!end of 'if kbhit'
	if(mouse_on) then
         call show_mouse()
22	   call mouse_update(ibutton,ix,iy)
	   if(outext.and.ibutton.ne.0) goto 22
		outext=.false.
	   if(ibutton.eq.0) mous_set=.true.
	endif
	if(mous_set) then
	   if(ibutton.ne.0) mous_set=.false.	!until reset above
	   if(ibutton.eq.1) then
		do in=1,nv
		   call calcbox(x(in),y(in),klv,kihv,ixbox,iybox)
	   	   call inside(ix,iy,ixbox,iybox,flag)
	         if(flag.eq.1) then
		 	call intconv(in,cnum)
		      call wrstring18(ixw,iyw,cnum,icseq,
     &	      icc)
		      call fillcircle(xc(in),yc(in),irc,icb,
     &            icseq)
		      ixw=ixw+24
			msol(isol)=in
		      isol=isol+1
			flag=0
		      goto 100
		   endif
		enddo
		do in=1,nbut
		   call calcbox(ixm(in),iym(in),klm,kihm,ixbox,iybox)
	   	   call inside(ix,iy,ixbox,iybox,flag)
	         if(flag.eq.1) then
			if(in.eq.1) then
			   goto 103
			else if (in.eq.2) then   !help
         		 call hide_mouse()
			 call fillcircle(xcm(2),ycm(2),irc,icb,icon)
			 call fillwin(ixm(2),iym(2),ixm(2)+klm+int(kiwm/2),
     &             iym(2)+kihm+int(kiwm/2),icbox)
			 call drawrec(ixm(2)+int(kiwm/2),iym(2)+int(kiwm/2),
     &  	       ixm(2)+klm+int(kiwm/2),iym(2)+kihm+int(kiwm/2),icv,
     &             icm)
			 call wrstring18(ixm(2)+int(klm/2),iym(2)+int(kihm/2),
     &	       'Help',icc,icm)
			 call wrstring18(ixm(2)+int(klm/2)+8,iym(2)+
     &		 int(kihm/2)+16,
     &             '?',icw,icm)
			 call POPTEXT(ixloh,-1,iyhih,helpst,nhelp1,14,1,15)	!=popkeys
			 call fillcircle(xcm(2),ycm(2),irc,icb,icc)
			 call draw_valve(ixm(2),iym(2),klm,kihm,kiwm,icm,icv,
     &		 icc)
			 call wrstring18(ixm(2)+klm/2-8,iym(2)+kihm/2-16,'Help',
     &	       icc,icm)
			 call wrstring18(ixm(2)+int(klm/2),iym(2)+int(kihm/2),
     &             '?',icc,icm)
			 call show_mouse()
			else                     !cancel
			 if(isol.lt.2) goto 100
         		 call hide_mouse()
			 call timer(it1)
			 call fillwin(ixlo+16,iyw,ixhi-16,iyw+16,icc)
			 ixw=ixlo+16
			 isol=1
			 do i=1,nv
			    call fillcircle(xc(i),yc(i),irc,icb,icc)
			 enddo
			 call fillcircle(xcm(3),ycm(3),irc,icb,icon)
			 call fillwin(ixm(3),iym(3),ixm(2)+klm+int(kiwm/2),
     &             iym(3)+kihm+int(kiwm/2),icbox)
			 call drawrec(ixm(3)+int(kiwm/2),iym(3)+int(kiwm/2),
     &  	       ixm(3)+klm+int(kiwm/2),iym(3)+kihm+int(kiwm/2),icv,
     &		 icm)
			 call wrstring18(ixm(3)+klm/2-8,iym(3)+kihm/2+16,
     &             'Cancel',
     &	       icc,icm)
			 call wrstring18(ixm(3)+int(klm/2)+8,iym(3)+
     &		 int(kihm/2)+24,
     &             'X',icw,icm)
87		       call timer(it2)
			 idt=it2-it1
			 if(idt.lt.30) goto 87
			 call fillcircle(xcm(3),ycm(3),irc,icb,icc)
			 call draw_valve(ixm(3),iym(3),klm,kihm,kiwm,icm,icv,
     &		 icc)
			 call wrstring18(ixm(3)+klm/2-16,iym(3)+kihm/2,
     &	       'Cancel',icc,icm)
			 call wrstring18(ixm(3)+int(klm/2),iym(3)+int(kihm/2)
     &		 +8,'X',icw,icm)
			 call show_mouse()
			 goto 100
			endif
		   endif
		enddo
		goto 100
	   else if(ibutton.eq.2) then		!right button
	      do i=1,nv
		   call fillcircle(xc(i),yc(i),irc,icb,icc)
	      enddo
	      call fillwin(ixlo+8,iyhi1+8,ixhi-8,iyhi-8,icc)
		goto 99
	   else
		goto 100
	   endif
	endif
	goto 100
103	continue
      call hide_mouse()
	nsol=isol-1
	isol=1

	call fillcircle(xcm(1),ycm(1),irc,icb,icon)
	call rdgwin1(320,iyhi1+8,ixhi-8,iyhi-8,itim)
	call opendialog(1,icb1,.true.)
	call wdialog(1,'Enter time [2 to 65535] :',icqd)
106   continue
	call defolti(countsec(isol),def)
	call intconv(msol(isol),cnum)
	call qdialog(1,'Valve '//cnum,
     &    def,icqd,cans)
	call getinpi(cans,countsec(isol))
	if(countsec(isol).lt.2.or.countsec(isol).gt.65535) then
		  call wdialog(1,'Time too big !',icw)
		  goto 106
	endif
	isol=isol+1
	if(isol.le.nsol) goto 106
	a='Y'
	isol=1
	call defolta(a,def)
	call qdialog(1,'Are these values OK ',def,icqd,cans)
	call getinpa(cans,a)
	if(UC(a).eq.'N') goto 106
	call clrdialog(1,0)
	call enddialog(1,0)
	call wrgwin1(320,iyhi1+8,ixhi-8,iyhi-8,itim)
	deallocate(itim)
	call wrstring18(ixlo+16,iyw-16,'START PROGRAM : ',icon,icc)
	iyw=iyw-32
	ixw=ixlo+16
	call intconv(msol(isol),cnum)
      call wrstring18(ixw,iyw,cnum,icon,icc)
	ixw=ixw+24
	call fillwin(x(msol(isol)),y(msol(isol)),x(msol(isol))+klv+
     &   int(kiwv/2),y(msol(isol))+kihv+int(kiwv/2),icv)
	call drawframe(x(msol(isol))+int(kiwv/2),y(msol(isol))+int(kiwv/2)
     &   ,x(msol(isol))+klv+int(kiwv/2),y(msol(isol))+kihv+int(kiwv/2),
     &   icbox)
	call fillcircle(xc(msol(isol)),yc(msol(isol)),irc,icb,icon)
	call wrstring18(x(msol(isol))+klv/2+8,y(msol(isol))+kihv,
     &   cnum,icc,icv)
	iakx=ixm(1)+klm+int(kiwm/2)
	iaky=iym(1)+kihm+int(kiwm/2)
	call fillwin(ixm(1),iym(1),iakx,iaky,3)
	call drawrec(ixm(1)+int(kiwm/2),iym(1)+int(kiwm/2),
     &  ixm(1)+klm+int(kiwm/2),iym(1)+kihm+int(kiwm/2),icv,icm)
	call wrstring18(ixm(1)+int(klm/2)-8,iym(1)+int(kihm/2),'On/Off',
     &  icc,icm)
	call wrstring18(ixm(1)+int(klm/2)-8,iym(1)+int(kihm/2)+16,'Timer',
     &  icc,icm)
	call show_mouse()
	GOTO 999
99	continue
	call wrstring18(ixlo+16,iyhi-32,
     &   'DO YOU WANT TO HAVE ANOTHER GO ? ',icseq,icc)
	a=getch(ktype)
	call wrstring18(ixlo+296,iyhi-32,UC(a),icseq,icc)
	if(a.eq.'y'.or.a.eq.'Y') then
	   call fillwin(ixlo+8,iyhi1+40,ixhi-8,iyhi-8,icc)
	   goto 200
	endif
	call timer(it1)
	call wrstring18(ixlo+16,iyhi-48,
     &   'EXIT PROGRAM',icw,icc)
66		call timer(it2)
		idt=it2-it1
		if(idt.lt.30) goto 66
	call EXIT(2)
999	end
