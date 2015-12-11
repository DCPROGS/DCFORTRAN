subroutine cellarray(main,icell,icellarray,celltitle,nrows,ncols,ctitle,textcell,&
iwcell,ncombo,comboname,ncomboname,icombo,icall,k,valcell1,ka,irate)

! cellular array with posibilities of combo boxes

use menu_f90
integer iwcell(10),ncomboname(10),ictype(10),icellarray(10),irate(200)
character*20 ctitle(10),textcell(10,100),comboname(10,100)
character	xtext*100,celltitle*30
logical icombo(10)
real valcell1(10,100)
type(GARRAYCELL) arrayattribs

	call gDefineRGB(112,0.96,0.82,0.82)
	call gDefineRGB(113,0.93,0.78,0.78)
	call gDefineRGB(114,0.90,0.74,0.74)
	do ic=115,120		!background col for columns 5-10
		call gDefineRGB(ic,0.87,0.70,0.70)
	enddo
	call gDefineRGB(111,0.98,0.92,0.92)
	call gDefineRGB(38,1.,0.5,0.25)	!orange
	call gmSetGuiGridMode(GOFF)
	ix=100
	iw=ix
	xtext=' '
	ih=336
	ih=nrows*25+48
	if(ih.gt.700) ih=700
	do j=1,ncols
		nx=len_trim(xtext)
		nc=len_trim(ctitle(j))
		xtext=xtext(1:nx)//ctitle(j)(1:nc)//char(124)
		iw=iw+iwcell(j)
	enddo
	icell=gmCreateComplexDialogueBox(Main,150,100, iw, ih, GALL, celltitle, &
              	 gmIconFormat=GDLLICON,gmIconFile='Gee',gmhpos=gleft,gmvpos=gtop)
	ipanel = gmCreatePanel(icell, 0, 24, iw, ih-48, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				
	icellarray(k) = gmCreateTextArray(iPanel, 0, 0, iw, ih-48, ncols, nrows, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=Gleft, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmExpand=GOFF)
	l=0
	
	if(ncombo.gt.0) then
		do i=1,ncols
			if (icombo(i)) then
				l=l+1
				ictype(l) = gmCreateComboBox(icellarray(k),0,0,iwcell(i),96,Gnone, 1, &
				gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
				do j=1,ncomboname(l)
					call gmSetListEntry(ictype(l), GADD, gmString=comboname(l,j))
				enddo
			endif
		enddo
	endif
	l=0
	call gmenqcellattribs(icellarray(k),1,1,arrayattribs)
	do i=1,ncols
		arrayattribs%width=iwcell(i)
		arrayattribs%flag = 0
		if(icombo(i)) l=l+1
		if(k.eq.1) then
!			if(i.lt.3) then
			if(i.le.2.or.i.eq.6) then
				arrayattribs%display=gdisplay
				arrayattribs%textcol=0
			else
				arrayattribs%display=gedit
				arrayattribs%textcol=7
			endif
		endif
		arrayattribs%backcol=110+i
		do j=1,nrows
			if(icombo(i)) 	then
				if(k.eq.1.and.i.eq.3) then
					if(textcell(3,j).eq.'MR') then
						arrayattribs%display=gdisplay
					else
						arrayattribs%display=gedit
					endif
				endif
				if(i.le.7) then
				    call gmSetCellSetting(icellarray(k), i,j ,gmString=textcell(i,j))
				
				    arrayattribs%flag = ictype(l)
				    call gmSetCellAttribs(icellarray(k), i, j, arrayattribs)
				    call gmSetListEntry(ictype(l),GSELECT,gmString=textcell(i,j))
			    else if (irate(j).le.ka.and.mod(j,2).eq.1) then
			        call gmSetCellSetting(icellarray(k), i,j ,gmString=textcell(i,j))
				
				    arrayattribs%flag = ictype(l)
				    call gmSetCellAttribs(icellarray(k), i, j, arrayattribs)
				    call gmSetListEntry(ictype(l),GSELECT,gmString=textcell(i,j))
			    endif
			
			else
				if(k.eq.1.and.i.eq.4) then
					call gmSetCellAttribs(icellarray(k), i, j, arrayattribs)
					if(valcell1(4,j).ne.0.00) call gmSetCellSetting(icellarray(k), i,j ,gmvalue=valcell1(i,j))			
				else
					call gmSetCellAttribs(icellarray(k), i, j, arrayattribs)
					if(i.le.8) then
					call gmSetCellSetting(icellarray(k), i,j ,gmString=textcell(i,j))
					else 
					if (irate(j).le.ka.and.mod(j,2).eq.1) then
					arrayattribs%display=gedit
					arrayattribs%backcol=115
					call gmSetCellAttribs(icellarray(k), i, j, arrayattribs)
					call gmSetCellSetting(icellarray(k), i,j ,gmString=textcell(i,j))
					else
					arrayattribs%display=gdisplay
					arrayattribs%backcol=0
					call gmSetCellAttribs(icellarray(k), i, j, arrayattribs)
					endif
					endif 
				endif
			endif
		enddo
   enddo
   ivb=gmCreatePushButton(icell,0,0, iw/2, 24,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=2032)
   ivb=gmCreatePushButton(icell,iw/2,0, iw/2, 24,'Continue',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
   call gmSetGuiGridMode(GOn)
99 continue   
!call gmDrawWidget(icellarray(k))
call gmdrawwindow(icell)
 end	

