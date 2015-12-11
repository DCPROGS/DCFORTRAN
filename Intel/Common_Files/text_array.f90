subroutine text_array(igraph2,Main,Form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext,isw,&
namelink,nlink,statname,dgamma,titlem,titlep,nbound,ligname,nlig,micro,ligand,jfix,theta0,iformtext)
use menu_f90

! Widget identifier definitions
integer :: Form1(25,4),iformText(25,4)
integer :: Form1_Panel1(25,4)

integer :: Form1_Txtarray1(25,4)
logical nodata,nofit,autosim,curvonly
integer iwcell(10),jfix(200)
character*10 text(ncols,nrows)
character  xtext*100,cnum*11
real*8 dgamma(200),theta0(200),thtrue(200)
character*15 statname(200)
character *10 titlep(200)
character*20 micro(200)
integer nlink(200)
character*40 namelink(200)
character*20 ligand(200) 
CHARACTER*74 titlem
character*100 titly
integer nbound(200,10)
character*20 ligname(10)
integer irate(200),jrate(200)
real*4 sdlog(200),sdlog1(200)
common/iniset/nodata,nofit,autosim,curvonly
type(GARRAYCELL) arrayattribs
integer ie(200),je(200),if(200),jf(200)
real efac(200)
COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
common/ir/irate,jrate
real*8 ec50d,pmax
logical monotd
common/ece/ec50d,pop0,pmax,curmax,concmax,monotd
common/cvsd/sdlog,sdlog1
common/extras/thtrue
common/ecg/ival1,ival2,ival3,iqbt,iqbt1,ibkta

ico=0
  call gmSetGuiGridMode(GOFF)
  ix=100
  ix=30
  if(isw.eq.1) titly='Properties of states in the mechanism'
  if(isw.eq.2) titly='Rate constants in the mechanism'
  if(isw.eq.3) titly='Fix (or unfix) rate constants,'
  
  do i=1,ncols
	!ix=ix+iwcell(i)
	ix=ix+110 !120
  enddo
  ihta=25*nrows+150 !366,288
!for 1024 x 768 screen the initial guesses box (isw=4) has bottom of screen
! with 22 rates	-now set iyma below
!if(ihta.gt.700) then
!	ihta=700
!	iyma=60
!endif
icall=0
itik=75
igog=0
  	if(isw.eq.4) then
		titly='Initial guesses for: '//titlem
		!ihta=ihta+50
		!itik=125
		igog=90
	endif
! Set up master window Form1
if(isw.eq.-3.or.isw.eq.-10) then
    titly='Results (no fit)'
	if(isw.eq.-3) then 
	   titly='Results of fit:'
	   nbut=4
	endif
		isw=3
	icall=4401
endif
if(isw.eq.-4) then
	isw=4
	titly='Results of fit with errors:'
	icall=4460
endif
if(isw.eq.1) then
ixma=400
iyma=100
else if(isw.eq.2) then
iyma=100
ixma=400
else if(isw.ge.3) then

iyma=100
ixma=200
endif
if(ihta.ge.650) then
	ihta=650	
	iyma=30		   !this keeps 22-row window just on screen for 1024 x 768
endif



 call gDefineRGB(112,0.96,0.82,0.82)
	 call gDefineRGB(113,0.93,0.78,0.78)
	 call gDefineRGB(114,0.90,0.74,0.74)
	 call gDefineRGB(115,0.87,0.70,0.70)
	 call gDefineRGB(111,0.98,0.92,0.92)

	 call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)
call gDefineRGB(38,1.,0.5,0.25)	!orange
   Form1(igraph2,isw)= gmCreateComplexDialogueBox(Main,ixma,iyma, ix, ihta+igog, GALL, titly, &
              	 gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='Gee')

   

! Create panel Form1_Panel1 child of Form1_MainPanel
   Form1_Panel1(igraph2,isw) = gmCreatePanel(Form1(igraph2,isw), 0, 0, ix, ihta-itik+30, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=ibkta, gmFillBorder=0, &
              	gmScrollMode=GNOBARS, gmVpos=GTOP, gmExpand=GOFF)
if(isw.eq.1.or.isw.eq.2) then
   iformText(igraph2,isw) = gmCreateTextEntry(form1_Panel1(igraph2,isw), 20, 5, 300, 25,titlem, 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
   endif
! Create text array Form1_TxtArray1 child of Form1_Panel1
   Form1_TxtArray1(igraph2,isw) = gmCreateTextArray(Form1_Panel1(igraph2,isw), &
                0, 30, ix, ihta-75, ncols, nrows+10, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=Gleft, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP, gmExpand=GOFF)
  
   call gmEnqCellAttribs(Form1_TxtArray1(igraph2,isw), 1, 1, arrayattribs)
 
   if(isw.eq.2) then
		ictype = gmCreateComboBox(Form1_TxtArray1(igraph2,isw),0,0,iwcell(4),(nlig+1)*32,Gnone, 1, &
         gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
		call gmSetListEntry(ictype, GADD, gmString='none')
		do i=1,nlig
			call gmSetListEntry(ictype, GADD, gmString=ligname(i))
		enddo
		ictype1 = gmCreateComboBox(Form1_TxtArray1(igraph2,isw),0,0,iwcell(5),(nlig+1)*32,Gnone, 1, &
        gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
		call gmSetListEntry(ictype1, GADD, gmString='none')
		arrayattribs%textcol=1
   else if(isw.eq.3) then
		ictype1 = gmCreateComboBox(Form1_TxtArray1(igraph2,isw),0,0,iwcell(5),2*32,Gnone, 1, &
          gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
		call gmSetListEntry(ictype1, GADD, gmString='no')
		call gmSetListEntry(ictype1, GADD, gmString='fixed')
else if(isw.eq.4) then
		ictype1 = gmCreateComboBox(Form1_TxtArray1(igraph2,isw),0,0,iwcell(5),2*32,Gnone, 1, &
          gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
		call gmSetListEntry(ictype1, GADD, gmString='no')
		call gmSetListEntry(ictype1, GADD, gmString='yes')
		arrayattribs%textcol=7
 else if(isw.ge.5) then
		ictype1 = gmCreateComboBox(Form1_TxtArray1(igraph2,isw),0,0,iwcell(5),6*32,Gnone, 1, &
          gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
		call gmSetListEntry(ictype1, GADD, gmString='none')
		call gmSetListEntry(ictype1, GADD, gmString='MR')
			call gmSetListEntry(ictype1, GADD, gmString='Multiplicative')
		call gmSetListEntry(ictype1, GADD, gmString='Aditive')
			call gmSetListEntry(ictype1, GADD, gmString='EC50')
				call gmSetListEntry(ictype1, GADD, gmString='Fixed')
		arrayattribs%textcol=0
   endif
  
    
   
   arrayattribs%height=ihcell
   arrayattribs%field=20
   if(isw.eq.-1) then
   do i=1,ncols
	do j=1,nrows
		arrayattribs%width=iwcell(i)
		call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), i, j, arrayattribs)
		call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), i,j ,gmString=text(i,j))
    enddo
   enddo
   endif
    call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)
   
   do j=1,nrows
		arrayattribs%textcol=7
		arrayattribs%flag = 0
	
		arrayattribs%backcol=122
		if(isw.eq.1) then
			arrayattribs%width=100
			arrayattribs%display=gedit
		else
			arrayattribs%width=70
			arrayattribs%display=gedit
			if(isw.ge.3) then
			    arrayattribs%backcol=112
				arrayattribs%display=gdisplay
				arrayattribs%textcol=0
			if(icall.eq.4401)	arrayattribs%backcol=122
			endif
		endif
	
		call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 1,j, arrayattribs)
		n1=len_trim(statname(j))
		call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 1,j ,gmString=statname(j)(1:n1))
	
		arrayattribs%backcol=123
		arrayattribs%display=gedit
		arrayattribs%textcol=7
		if(isw.eq.1) then
			arrayattribs%width=90
			
		else
			arrayattribs%width=90
			if(isw.ge.3)then
				arrayattribs%display=gdisplay
				arrayattribs%textcol=0
				arrayattribs%backcol=113
					if(icall.eq.4401)	arrayattribs%backcol=123
			endif
			
		endif
		if(isw.eq.1) then
			
			x=dgamma(j)
			if (x.lt.10e-5.or.x.gt.10e5) then
			arrayattribs%format=gscientific
			else
			arrayattribs%format=0
			endif
			arrayattribs%ndp=5
			call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 2,j, arrayattribs)
			call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 2,j ,gmValue=x)
		else
	
 		
			call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 2,j, arrayattribs)
			call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 2,j ,gmString=titlep(j))
		endif
		
		arrayattribs%backcol=124
		arrayattribs%textcol=7
		arrayattribs%display=gedit
		if(isw.eq.1) then
		
			arrayattribs%width=50
			arrayattribs%format=0
			arrayattribs%ndp=0
			x=nlink(j)
		else
		
			arrayattribs%width=100
			x=dgamma(j)
			if (x.lt.10e-5.or.x.gt.10e5) then
			arrayattribs%format=gscientific
			else
			arrayattribs%format=0
			endif
			arrayattribs%ndp=5

			if(isw.eq.3) then
				arrayattribs%textcol=0
				arrayattribs%display=gdisplay
			endif
			if(isw.ge.3) arrayattribs%backcol=114
				if(icall.eq.4401)	arrayattribs%backcol=124
		endif
		
		call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 3,j, arrayattribs)
		call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 3,j ,gmValue=x)
		
			arrayattribs%flag = 0
			arrayattribs%backcol=122
			arrayattribs%textcol=7
			arrayattribs%display=gedit
			if(isw.eq.1) then
				arrayattribs%width=90

			else
				arrayattribs%width=90
			endif
			call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 4,j, arrayattribs)
			if(isw.eq.1) then
		
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 4,j ,gmString=namelink(j))
			else if(isw.eq.2) then
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 4,j ,gmString=ligand(j))

		
				arrayattribs%flag = ictype
			
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 4,j, arrayattribs)
				call gmSetListEntry(ictype,GSELECT,gmString=ligand(j))
			else if(isw.ge.3 ) then
				if(icall.eq.4460) then
			
					arrayattribs%width=120
					arrayattribs%backcol=125
					arrayattribs%display=gdisplay
			 		arrayattribs%flag =0
					x=theta0(j)
					if (x.lt.10e-5.or.x.gt.10e5) then
					arrayattribs%format=gscientific
		   
					else
					arrayattribs%format=0
					endif
				
				arrayattribs%ndp=5
				arrayattribs%textcol=2
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 4,j, arrayattribs)
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 4,j ,gmValue=x)
				else
				arrayattribs%backcol=112
				arrayattribs%textcol=0
				arrayattribs%display=gdisplay
				if(icall.eq.4401)	arrayattribs%backcol=122
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 4,j, arrayattribs)
				
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 4,j ,gmString=micro(j))
				endif
			endif
	
			arrayattribs%textcol=7
			arrayattribs%display=gedit
			arrayattribs%backcol=123
			arrayattribs%flag = ictype1
			if(isw.eq.1) then
				arrayattribs%width=60
			else
				arrayattribs%width=60
			endif

		
			if(isw.eq.2) then
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 5,j ,gmString='none')

				arrayattribs%flag = ictype1
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 5,j, arrayattribs)
			else if(isw.ge.3) then
				arrayattribs%backcol=113
				if(isw.eq.4)	then 
					arrayattribs%textcol=0
					arrayattribs%display=gdisplay
					arrayattribs%backcol=113
				endif
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 5,j, arrayattribs)

				if(isw.eq.3.and.icall.eq.4401)then
					arrayattribs%width=120
					arrayattribs%backcol=125
					arrayattribs%display=gdisplay
			 		arrayattribs%flag =0
					x=theta0(j)
					if (x.lt.10e-5.or.x.gt.10e5) then
					arrayattribs%format=gscientific
		   
					else
					arrayattribs%format=0
					endif
					arrayattribs%ndp=5
					arrayattribs%textcol=2
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 5,j, arrayattribs)
					call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 5,j ,gmValue=x)
				else if(isw.eq.4.and.icall.eq.4460) then
				
					cv=100.*sdlog(j)
					sx=sdlog(j)*sngl(theta0(j))
					!	 cv=100.*sdlog(j)/sngl(theta0(j))
					!cv=100.*sdlog(j)
					!sx=sdlog(j)*sngl(theta0(j))
			
					arrayattribs%width=60
					arrayattribs%backcol=124
					arrayattribs%textcol=0
					arrayattribs%width=120
					arrayattribs%backcol=125
					arrayattribs%display=gdisplay
			 		arrayattribs%flag =0
					x=sx
					if (x.lt.10e-5.or.x.gt.10e5) then
					arrayattribs%format=gscientific
		   
					else
					arrayattribs%format=0
					endif
					arrayattribs%ndp=5
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 6,j, arrayattribs)
					if(x.gt.0) call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 6,j ,gmvalue=x)
						x=cv
					if (x.lt.10e-5.or.x.gt.10e5) then
					arrayattribs%format=gscientific
		   
					else
					arrayattribs%format=0
					endif
					arrayattribs%ndp=5
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 5,j, arrayattribs)
					if(x.gt.0)	call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 5,j ,gmvalue=x)
		
				
				else



					if(jfix(j).eq.1) then
					call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 5,j ,gmString='fixed')
					arrayattribs%flag = ictype1
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 5,j, arrayattribs)
					call gmSetListEntry(ictype1,GSELECT,gmString='fixed')
					else
					call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 5,j ,gmString='no')
					arrayattribs%flag = ictype1
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 5,j, arrayattribs)
					call gmSetListEntry(ictype1,GSELECT,gmString='no')
					endif
				endif
			endif
			arrayattribs%flag = 0
			if(ncols.ge.6) then
			if(isw.eq.2) then
				arrayattribs%width=iwcell(6)
				arrayattribs%backcol=121
				arrayattribs%textcol=7
			    arrayattribs%display=gedit
				if(isw.eq.1) then
					arrayattribs%width=90
				else
					arrayattribs%width=90
				endif
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 6,j, arrayattribs)
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 6,j ,gmString=micro(j))
				if(ncols.eq.9) then
				
					do m=1,neq
					if(irate(j).eq.ie(m).and.jrate(j).eq.je(m)) then
					ico=ico+1
					arrayattribs%width=iwcell(7)
					 arrayattribs%backcol=122
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 7,j, arrayattribs)

					call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 7,j ,gmvalue=efac(ico))
					arrayattribs%width=iwcell(8)
					 arrayattribs%backcol=123
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 8,j, arrayattribs)
					do l=1,nrows
							if(irate(l).eq.if(ico).and.jrate(l).eq.jf(ico)) then
								call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 8,j ,gmString=titlep(l))
					
							endif
					enddo
					endif
				enddo
		
				arrayattribs%width=iwcell(9)
				arrayattribs%backcol=124
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 9,j, arrayattribs)
				x=sngl(theta0(j))
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 9,j ,gmvalue=x)
			endif    
		else if(isw.eq.3.and.icall.eq.4401.or.isw.eq.4.and.icall.eq.4460) then
			if(autosim.and.isw.eq.3) then
				x=thtrue(j)
					if (x.lt.10e-5.or.x.gt.10e5) then
					arrayattribs%format=gscientific
		   
					else
					arrayattribs%format=0
					endif
				arrayattribs%width=120
					arrayattribs%backcol=124
					arrayattribs%textcol=0	
				arrayattribs%ndp=5
			!	arrayattribs%textcol=2
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 6,j, arrayattribs)
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 6,j ,gmValue=x)
			else
				
					do m=1,neq
					if(irate(j).eq.ie(m).and.jrate(j).eq.je(m)) then
					ico=ico+1
					arrayattribs%width=80
					arrayattribs%backcol=124
					arrayattribs%textcol=0
					if(icall.eq.4460) then
						arrayattribs%format=0
				
					arrayattribs%ndp=3
					call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 8,j, arrayattribs)
				
					call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 8,j ,gmvalue=efac(ico))
				
					
					else
						call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 6,j, arrayattribs)
					call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 6,j ,gmvalue=efac(ico))
					endif
					endif
					enddo
			endif
			endif
		
		endif
		if(isw.eq.4.and.icall.eq.4460) then
			arrayattribs%backcol=112
				arrayattribs%textcol=0
				arrayattribs%display=gdisplay
				if(icall.eq.4401)	arrayattribs%backcol=122
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), 7,j, arrayattribs)
				
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), 7,j ,gmString=micro(j))
	
		endif
   enddo
   call gmDrawWidget(Form1_TxtArray1(igraph2,isw))
 
   if(isw.eq.1) then
		arrayattribs%format=0
		arrayattribs%ndp=0
		arrayattribs%width=90
		
		
		k=1
		do 	i=5,ncols
			arrayattribs%backcol=116+i
			do j=1,nrows
				X=NBOUND(J,K)
				call gmSetCellAttribs(Form1_TxtArray1(igraph2,isw), i,j, arrayattribs)
				call gmSetCellSetting(Form1_TxtArray1(igraph2,isw), i,j ,gmValue=x)
				enddo
			K=K+1
		enddo
   endif
   call gmDrawWidget(Form1_TxtArray1(igraph2,isw))
 
10	continue   
	if(isw.eq.1) then
		icall=	1350+igraph2
	else if (isw.eq.2) then
		icall=1450+igraph2
	else
		if(icall.ne.4401.and.icall.ne.4460) icall=2042
	endif
	!	ibx=int(float(ix)/2.)
		ibx=ix/2
		idiv=2
	

!	if(isw.eq.3) icall=2042
	if(icall.eq.4401) then
		iqbt=gmCreatePushButton(Form1(igraph2,isw),0,0, int(float(ix)/4.), 24,'Plot now',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom,gmOffcol=122, gmHpos=Gright,gmExpand=GOFF,gmCallback=icall)
		if(.not.autosim) then
		iqbt1=gmCreatePushButton(Form1(igraph2,isw),0,0,int(float(ix)/4), 24,'Redo fit',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom,gmOffcol=123, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=4402)
		endif
		if(.not.nofit) then
		iqbt2=gmCreatePushButton(Form1(igraph2,isw),int(float(ix)/2.),0,&
		        int(float(ix)/4.), 24,'Estimate errors',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom,gmOffcol=123, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=4403)
		iqbt=gmCreatePushButton(Form1(igraph2,isw),int(float(ix)/4.),0, int(float(ix)/4.),&
		        24,'Store fitted rates',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom,gmOffcol=122, gmHpos=Gleft,gmExpand=GOFF,gmCallback=4406)
		endif
	else
	if(isw.eq.4) then
	if(icall.eq.4460) then
	iqbt=gmCreatePushButton(Form1(igraph2,isw),0,0, int(float(ix)/3.), 24,'Exit ',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=122,gmExpand=GOFF,gmCallback=2045)
	iqbt=gmCreatePushButton(Form1(igraph2,isw),0,0, int(float(ix)/3.), 24,'Do plots',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gright,gmOffcol=122,gmExpand=GOFF,gmCallback=icall)
	iqbt=gmCreatePushButton(Form1(igraph2,isw),int(float(ix)/3.),0, int(float(ix)/3.),&
		        24,'Store mec. with fitted rates',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom,gmOffcol=122, gmHpos=Gleft,gmExpand=GOFF,gmCallback=4406)
	else
	idiv=3
	ibx=int(float(ix)/3.)
	iqbt=gmCreatePushButton(Form1(igraph2,isw),0,0, ibx, 24,'Cancel ',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=122,gmExpand=GOFF,gmCallback=2045)

	
	iqbt=gmCreatePushButton(Form1(igraph2,isw),ibx,0,ibx, 24,'Save to mec file ',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=122,gmExpand=GOFF,gmCallback=4433)
	iqbt=gmCreatePushButton(Form1(igraph2,isw),(idiv-1)*ibx,0, ibx, 24,'Continue',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=122,gmExpand=GOFF,gmCallback=icall)
	
	endif
	else if(isw.eq.1) then
		iqbt=gmCreatePushButton(Form1(igraph2,isw),(idiv-1)*ibx,0, ibx, 24,'Store changes ',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=122,gmExpand=GOFF,gmCallback=icall)
	
		iqbt1=gmCreatePushButton(Form1(igraph2,isw),0,0,ibx, 24,'New/change mechanism title',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=123,gmExpand=GOFF,gmCallback=1950)
	else if(isw.eq.2) then
		iqbt=gmCreatePushButton(Form1(igraph2,isw),(idiv-1)*ibx,0, ibx, 24,'Store changes',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=123,gmExpand=GOFF,gmCallback=icall)
	
		iqbt1=gmCreatePushButton(Form1(igraph2,isw),0,0,ibx, 24,'Enter/change rate title',&

              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=123,gmExpand=GOFF,gmCallback=1960)

	
		nmr=0.5*nrows-k+1
	else if(isw.eq.3) then
	iqbt=gmCreatePushButton(Form1(igraph2,isw),(idiv-1)*ibx,0, ibx, 24,'Continue ',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=38,gmExpand=GOFF,gmCallback=icall)

iqbt=gmCreatePushButton(Form1(igraph2,isw),0,0, ibx, 24,'Cancel ',&
              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=122,gmExpand=GOFF,gmCallback=2045)
	endif
	endif
	if(isw.eq.1.or.isw.eq.2.or.isw.eq.3) call gmDrawWidget(Form1_TxtArray1(igraph2,isw))
	if(isw.eq.4.and.icall.eq.2042) then
		itext1 = gmCreateTextEntry(Form1(igraph2,isw), 10, 110, 400, 20,&
			'Press this button after changing guesses to recalculate EC50', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=Gbottom, gmExpand=GOFF)
		iqbt1=gmCreatePushButton(Form1(igraph2,isw),400,110,100, 20,'Recalculate',&

              	gmType=GUSERDEFINED, gmSize=80,gmTextCol=2, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmOffcol=123,gmExpand=GOFF,gmCallback=2044)

			nb=len_trim(ligname(1))
			itext1 = gmCreateTextEntry(Form1(igraph2,isw), 10, 90, 300, 20,&
			'At zero concentration of '//ligname(1)(1:nb)//' P(open) =', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF)
			
				ival1=gmCreateValueEntry(Form1(igraph2,isw), 300, 90, 150, 20, pop0, 20,5, gdisplay,&
		gmVpos=Gbottom)
		call gmsetvaluesetting(ival1,pop0)
		if(monotd) then
		itext2 = gmCreateTextEntry(Form1(igraph2,isw), 10, 60, 450, 25,&
			'Equilibrium response-concentration curve is monotonic, Maximum Popen = ', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF)
		ival3=gmCreateValueEntry(Form1(igraph2,isw), 460, 60, 150, 25, pmax, 10,5, gdisplay,&
		gmVpos=Gbottom)
		   itext2 = gmCreateTextEntry(Form1(igraph2,isw), 10, 30, 450, 25,&
			'EC50 = Conc of '//ligname(1)(1:nb)//' for 50% of this maximum (muM) =', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF)
			ival2=gmCreateValueEntry(Form1(igraph2,isw), 370, 30, 150, 25, ec50d*1.e6, 12,5, gdisplay,&
		gmVpos=Gbottom)
		else
		itext2 = gmCreateTextEntry(Form1(igraph2,isw), 10, 60, 350, 25,&
			'Equilibrium response has a maximum, Maximum Popen = ', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF)
		ival3=gmCreateValueEntry(Form1(igraph2,isw), 370, 60, 150, 25, pmax, 20,10, gdisplay,&
		gmVpos=Gbottom)

		itext2 = gmCreateTextEntry(Form1(igraph2,isw), 10, 30, 360, 25,&
			'Conc of '//ligname(1)(1:nb)//' for 50% of this max current (muM) (left of max)=', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF)
			ival2=gmCreateValueEntry(Form1(igraph2,isw), 370, 30, 150, 25, ec50d*1.e6, 12,5, gdisplay,&
		gmVpos=Gbottom)
		endif
	endif
	if(icall.ne.2042) &
		 itext2 = gmCreateTextEntry(Form1(igraph2,isw), 1, 25, ix-2, 20,&
			'To copy/paste highlight the required area in the table with the mouse(left button) and then click &
			the right button', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=Gbottom, gmExpand=GOFF)
999	call gmSetGuiGridMode(GOn)
call gmdrawwindow(Form1(igraph2,isw))
!istat=gmsavetexttofile(Form1_TxtArray1(igraph2,isw),'hjcfit.txt')
	end
