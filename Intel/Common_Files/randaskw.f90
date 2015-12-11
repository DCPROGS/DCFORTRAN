	subroutine RANDSKV1(ix,iy,iz,iwrite,repeat,main)
! To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from D:\
! (1) Read: (a) Asks whether to repeat last sequence. If NO then output
!		ix,iy,iz and set REPEAT=false. If YES output ixlast,
!		iylast,izlast and set REPEAT=true.
! (2) Write: If REPEAT is true on entry then write nothing to disc
!		in case another repeat is needed. If REPEAT false on
!		entry then (a) replace current ixlast etc by ix etc from
!		disc, and (b) replace ix etc by input values

! (3) iwrite=-1 skips question and does NOT repeat previous sequence
!
use menu_f90
!use gino_f90
	USE DFLIB
	logical repeat,present
	character*30 ranfile,ranpath,ranfilt
	character*11 cnum
	character*10 istr(10)
	logical discprt
	character*60 text(10)
	common/dp/discprt
	common/ranlast/ixlast,iylast,izlast
	TYPE (FILE$INFO) info
	SAVE ranfile
	
	ranpath='.'
	ranfilt='*.dat'
	n=len_trim(ranfile)
	if(n.ge.10) then
	   if(ranfile(n-9:n).ne.'random.dat') then
		ranfile='random.dat'  !file for seeds
	   endif
	else
	   ranfile='random.dat'  !file for seeds
	endif
22	INQUIRE(file=ranfile,exist=present)
	
	if(present) then
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(ranfile, info, ihandle)
		nLEN=info%length
	    if(nlen.lt.10) then
		  imes=gmdisplaymessagebox(' ','RANDOM.DAT has 0 lenght.Browse for file?',&
		  gquestion,gyesno)
	      if(imes.eq.gyesbutton) then
				CALL gmFileBROWSER(ranFILE,ranpath,ranfilt,gmBrowseType=0)
		  else
		     call RANSEED()
			 ix=ixlast
			 iy=iylast
			 iz=izlast
			 call RANSEED()
			 OPEN(unit=25,file=ranfile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=256)
			 write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
			 close(unit=25)
		  endif
	   endif
	 else
		imes=gmdisplaymessagebox(' ','RANDOM.DAT not found in local directory.Browse for file?',&
		  gquestion,gyesno)
	      if(imes.eq.gyesbutton) then
			CALL gmFileBROWSER(ranFILE,ranpath,ranfilt,gmBrowseType=0)
		  else
			 call RANSEED()
			 ix=ixlast
			 iy=iylast
			 iz=izlast
			 call RANSEED()
			 OPEN(unit=25,file=ranfile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=256)
			 write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
			 close(unit=25)
		  endif
	 endif

     OPEN(unit=25,file=ranfile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=256)
	 if(iwrite.eq.1) goto 10
	 if(iwrite.eq.2) goto 20
	 read(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
	 ix0=ix
	 iy0=iy
	 iz0=iz
	 
	 if (iwrite.eq.-1) then 
	     if(ix.ne.0.and.iy.ne.0.and.iz.ne.0) then
			repeat=.false.
			goto 20
		 else
			iwrite=1
		 endif
	 endif
	 text(1)='Use the same as above'
	text(2)='Repeat preceding random number sequence'
	text(3)='Choose ''random'' seeds from system time'
	text(4)='Enter three non-zero integers'
	iwid=16
	inswin = gmCreateComplexDialogueBox(Main,13 ,7 ,iwid ,16 , GALL, ' ', &
         gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	iyesnop=gmCreatePanel(inswin, 0, 0,iwid , 16, gmHpos=GCENTRE, gmtitle=' Seeds for random:',&
		gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY,gmFillCol=142, &
		gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
	
	call intconv(ix,cnum)

	itext1 = gmCreateTextEntry(iyesnop, 4, 1, 6, 1,'ix='//cnum, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gtop, gmExpand=GOFF)
	call intconv(iy,cnum)
	itext2=gmCreateTextEntry(iyesnop, 4, 2, 6, 1,'iy='//cnum, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gtop, gmExpand=GOFF)
	call intconv(iz,cnum)
	itext3=gmCreateTextEntry(iyesnop, 4, 3, 6, 1,'iz='//cnum, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gtop, gmExpand=GOFF)
	ist_Radio1 = gmCreateRadioBox(iyesnop, 1, 4,iwid-2 , 6, &
		gmType=GFREEFORM, gmBorderType=GPROJECTED, &
		gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
		gmTitle=' ', gmVpos=GTOP)

	if(ix.eq.0.or.iy.eq.0.or.iz.eq.0) then
		repeat=.false.
	iyes1 = gmCreateToggleButton(ist_Radio1, 1, 1 , iwid-3, 1, text(1),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=1)		

	iyes2 = gmCreateToggleButton(ist_Radio1, 1, 2 , iwid-3, 1, text(2),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=2)	
	iyes3 = gmCreateToggleButton(ist_Radio1, 1, 3 , iwid-3, 1, text(3),1 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=3)
	iyes4 = gmCreateToggleButton(ist_Radio1, 1, 4 , iwid-3, 1, text(4),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=4)
    else
		if(repeat) then
			iyes1 = gmCreateToggleButton(ist_Radio1, 1, 1 , iwid-4, 1, text(1),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=1)		

	iyes2 = gmCreateToggleButton(ist_Radio1, 1, 2 , iwid-4, 1, text(2),1 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=2)	
	iyes3 = gmCreateToggleButton(ist_Radio1, 1, 3 , iwid-4, 1, text(3),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=3)
	iyes4 = gmCreateToggleButton(ist_Radio1, 1, 4 , iwid-4, 1, text(4),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=4)
		else
	iyes1 = gmCreateToggleButton(ist_Radio1, 1, 1 , iwid-4, 1, text(1),1 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=1)		

			yes2 = gmCreateToggleButton(ist_Radio1, 1, 2 , iwid-4, 1, text(2),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=2)	
	iyes3 = gmCreateToggleButton(ist_Radio1, 1, 3 , iwid-4, 1, text(3),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=3)
	iyes4 = gmCreateToggleButton(ist_Radio1, 1, 4 , iwid-4, 1, text(4),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=4)
		endif
	endif

    i1=gmCreateValueEntry(iyesnop,4 ,11 , 4, 1, ix, 10, 0,gedit,&
		gmVpos=Gtop,gmtextcol=11)
	i2=gmCreateValueEntry(iyesnop,4 ,12 , 4, 1, iy, 10, 0,gedit,&
		gmVpos=Gtop,gmtextcol=11)
	i3=gmCreateValueEntry(iyesnop,4 ,13, 4, 1, iz, 10, 0,gedit,&
		gmVpos=Gtop,gmtextcol=11)
	
	ivb=gmCreatePushButton(iyesnop,1,0, iwid-2, 1,'Continue',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=5)
	
call gmdrawwindow(inswin)
!call gmSetWidgetStatus(i1, GunSELECTABLE)
!call gmSetWidgetStatus(i2, GunSELECTABLE)
!call gmSetWidgetStatus(i3, GunSELECTABLE)

icall=0

7	icall=gmAction(icall)
	if(icall.eq.1) then
		call gmsetvaluesetting(i1,ix0)
		call gmsetvaluesetting(i2,iy0)
		call gmsetvaluesetting(i3,iz0)
		repeat=.false.
		goto 7
	else if(icall.eq.2) then
		repeat=.true.
		ix=ixlast		!if repeat
		iy=iylast		!if repeat
		iz=izlast
		call gmsetvaluesetting(i1,ix)
		call gmsetvaluesetting(i2,iy)
		call gmsetvaluesetting(i3,iz)
		goto 7
	else if(icall.eq.3) then
		 call RANSEED()
		 repeat=.false.
		 ikik=1
		 call gmsetvaluesetting(i1,ix)
		 call gmsetvaluesetting(i2,iy)
		 call gmsetvaluesetting(i3,iz)
		 goto 7
	else if(icall.eq.4) then
		repeat=.false.
		goto 7
	else if(icall.eq.5) then
	    
		isat3=gmenqtoggleswitch(iyes3)
		isat4=gmenqtoggleswitch(iyes4)
		
		if (isat3.eq.1.and.ikik.eq.0) then
		 call RANSEED()
		 repeat=.false.
		 ikik=1
		 call gmsetvaluesetting(i1,ix)
		 call gmsetvaluesetting(i2,iy)
		 call gmsetvaluesetting(i3,iz)
		endif
		continue 
		ix2=gmenqvaluesetting(i1)
		iy2=gmenqvaluesetting(i2)
		iz2=gmenqvaluesetting(i3)
		if(ix2.eq.0.or.iy2.eq.0.or.iz2.eq.0) then
		   imes=gmdisplaymessagebox(' ','Wrong numbers;try again',ginformation,gok)
		   goto 7
		else
		    if(isat4.eq.1.and.ix2.eq.ix.and.iy2.eq.iy.and.iz2.eq.iz) then
			imes=gmdisplaymessagebox(' ','Same values;try again',ginformation,gok)
		   goto 7
		endif
			ix=ix2
			iy=iy2
			iz=iz2
			call gmremovewindow(inswin)
			goto 20
		endif
	else
		goto 7
	endif
	icall=0
10	continue
	if(repeat) goto 9		!no write
	read(25,rec=1) ix1,iy1,iz1,ixlast,iylast,izlast
	ixlast=ix1
	iylast=iy1
	izlast=iz1
	write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
	goto 9

	! the new write option: iwrite=2. Leave ixlast etc unchanged
20	continue
	read(25,rec=1) ix1,iy1,iz1,ixlast,iylast,izlast
	write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast


9	close(unit=25)
    if(discprt) write(7,16)ix,iy,iz
16	format(' Seeds for RANDOM= ',3i8)
	icall=0
	call gmflushcallbackqueue()
	RETURN
	end
