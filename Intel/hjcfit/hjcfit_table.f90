subroutine hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
	tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
	fc,ffilt,trise,autosim,icall,val9,idatyp)
	use gino_f90
	use menu_f90
	integer panel1,hjcfitform,panel2(10),nfile(10)
	integer edit1(20,10),edit2(20,10),edit3(20,10),edit4(20,10),edit5(20,10),edit6(20,10)
	character*40 tedit1(20,10),tedit2(20,10),tedit3(20,10),tedit4(20,10),tedit5(20,10),tedit6(20,10)
	integer val1(20,10),val2(20,10),val3(20,10),val4(20,10),val5(20,10),val6(20,10)
	real tval1(20,10),tval2(20,10),tval3(20,10),tval4(20,10),tval5(20,10),tval6(20,10)
	integer toggle7_1(10,10),toggle7_2(10,10),toggle7_3(10,10),toggle7_4(10,10),toggle7_5(10,10)
	integer toggle14_1(10),toggle14_2(10),toggle14_3(10),valt(5)
	character*14 ptype(5)
	character*60 pfiles(20,10)
	real tvalt(5),tval7(20,10),tval8(20,10),tval9(20,10),tval10(20,10)
	integer val8(20,10),val7(20,10),val9(20,10),panel14
	logical discprt,autosim
	real fc(10),ffilt(10),trise(10)
	common/dp/discprt
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
   iwidstat=7
   iwidstat1=7
   isum=0
   if(autosim) then
			nmax=0
			do j=1,nset
				nfile(j)=1	!always for simulation
			enddo
   endif
   do i=1,nset
	isum=isum+nfile(i)
   enddo
	isum=iwidstat*(isum)+iwidstat1+2
	isum1=isum
	if (isum.gt.38) isum1=28
	iminus=29
	iminus1=20
	if(icall.eq.-154) then
	iminus=25
	iminus1=16
	endif
	if(autosim) then
	iminus=12
	iminus1=8
	endif 

   HjcfitForm=gmCreateComplexDialogueBox(Main, 1, 5, isum1, iminus, GALL, 'Experiment;check the concentration(s) ', &
              	gmIconFormat=GDLLICON,gmvpos=gtop,gmIconFile='MBIG1052')
! Create main panel for form
  iHjcfitForm=gmCreatePanel(HjcfitForm, 0, 0, isum, iminus-2, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmScrollMode=Ghorizontal,gmFillCol=150, gmFillBorder=0)
 Panel1=gmCreatePanel(iHjcfitForm, 0, 0, iwidstat1, iminus1, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=Gnone, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0)

   iStatic1_1 = gmCreateTextEntry(Panel1, 1, 1, iwidstat1-1, 1,'File', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
 
  iStatic1_2 = gmCreateTextEntry(Panel1, 1, 2, iwidstat1-1, 1,'DATEW ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   iStatic1_3 = gmCreateTextEntry(Panel1, 1, 3, iwidstat1-1, 1,'Title ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic1_3a = gmCreateTextEntry(Panel1, 1, 4, iwidstat1-1, 1,'Concentration [um]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   iStatic1_11 = gmCreateTextEntry(Panel1, 1, 5, iwidstat1-1, 1,'Full amplitude (pA) ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
   if(.not.autosim) then
   
	iStatic1_4 = gmCreateTextEntry(Panel1, 1, 6, iwidstat1-1, 1,'Tape details ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
 
   
	iStatic1_5 = gmCreateTextEntry(Panel1, 1, 7, iwidstat1-1, 1,'Nr of transitions', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
if(idatyp.eq.5) then
    iStatic1_6 = gmCreateTextEntry(Panel1, 1, 8, iwidstat1-1, 1,'Sample rate', 32768, GDISPLAY, &
        	    gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)



	iStatic1_7 = gmCreateTextEntry(Panel1, 1, 9, iwidstat1-1, 1,'Raw data file ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
else
	iStatic1_6 = gmCreateTextEntry(Panel1, 1, 8, iwidstat1-1, 1,'Patch number ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)



	iStatic1_7 = gmCreateTextEntry(Panel1, 1, 9, iwidstat1-1, 1,'Patch type ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
 endif
 
 !  do i=1,5	 
 !	 iStatic1_7_1 = gmCreateTextEntry(Panel1, 1, 7+i, iwidstat1, 1,ptype(i), 32768, GDISPLAY, &
 !             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
  !            	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
   !           	gmVpos=GTOP, gmExpand=GOFF)
   !enddo
   iStatic1_8 = gmCreateTextEntry(Panel1, 1, 10, iwidstat1-1, 1,'Membrane potential [mV] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)


   iStatic1_9 = gmCreateTextEntry(Panel1, 1, 11 ,iwidstat1-1, 1,'Temperature [o C] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)


   iStatic1_10 = gmCreateTextEntry(Panel1, 1, 12, iwidstat1-1, 1,'Filter (kHz, -3 dB) ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)


   
				
   iStatic1_12 = gmCreateTextEntry(Panel1, 1, 13, iwidstat1-1, 1,'RMS noise (pA) ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

   iStatic1_13 = gmCreateTextEntry(Panel1, 1, 14, iwidstat1-1, 1,'Calibration ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

	if(icall.ne.-154) then

	iStatic1_14 = gmCreateTextEntry(Panel1, 1, 15, iwidstat1-1, 1,'Risetime setting: ', 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic1_14_1 = gmCreateTextEntry(Panel1, 2, 16, iwidstat1-1, 1,'Nominal filter setting',&
				 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic1_14_1 = gmCreateTextEntry(Panel1, 2, 17, iwidstat1-1, 1,'Effect of prefilter+tape', 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic1_14_1 = gmCreateTextEntry(Panel1, 2, 18, iwidstat1-1, 1,'Specify value here',32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)
 	endif
   Panel14=gmCreatePanel(iHjcfitForm, 0, iminus1, isum, 7, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=152, gmFillBorder=0)

  
   iStatic1_14 = gmCreateTextEntry(Panel14, 1, 1, iwidstat1, 1,'Total nr of transitions ', 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              	gmVpos=GTOP, gmExpand=GOFF)
	iStatic1_14_1 = gmCreateTextEntry(Panel14, 1, 2, iwidstat1, 1,'Temperature (mean)',&
				 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              	gmVpos=GTOP, gmExpand=GOFF)
	iStatic1_14_1 = gmCreateTextEntry(Panel14, 1, 3, iwidstat1, 1,'Full amplitude (mean)', 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic1_14_1 = gmCreateTextEntry(Panel14, 1, 4, iwidstat1, 1,'RMS noise (mean)',32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              	gmVpos=GTOP, gmExpand=GOFF)

   iStatic1_14_1 = gmCreateTextEntry(Panel14, 1, 5, iwidstat1, 1,'Potential (mV) (mean)',32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              	gmVpos=GTOP, gmExpand=GOFF)
  endif


ixwi=iwidstat1
do j=1,nset
    if(j.eq.1) then
   Panel2(j) = gmCreatePanel(iHjcfitForm,iwidstat1, 0,&
    iwidstat*nfile(j), iminus1, &
              	gmType=GinverseCHISEL,gmtitle='Set:'//char(48+j) ,&
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   else
 ixwi=ixwi+iwidstat*nfile(j-1)
   Panel2(j) = gmCreatePanel(iHjcfitForm,ixwi, 0,&
    iwidstat*nfile(j), iminus1, &
              	gmType=GinverseCHISEL,gmtitle='Set:'//char(48+j) ,&
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   endif
   do i=1,nfile(j)
  if(.not.autosim) tedit1(i,j)=pfiles(i,j)				  
   Edit1(i,j) = gmCreateTextEntry(Panel2(j), (i-1)*iwidstat+1, 1, iwidstat-2, 1,tedit1(i,j), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   Edit2(i,j) = gmCreateTextEntry(Panel2(j), (i-1)*iwidstat+1, 2, iwidstat-2, 1,tedit2(i,j), 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, gmscrollable=gon,&
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   Edit3(i,j) = gmCreateTextEntry(Panel2(j), (i-1)*iwidstat+1, 3, iwidstat-2, 1,tedit3(i,j), 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   val9(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 4, iwidstat-2, 1,tval9(i,j), 10,4, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF) 
			!	call gmsetvaluesetting(val9(i,j),tval9(i,j))
   
  val6(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 5, iwidstat-2, 1,tval6(i,j), 10,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 
		!			call gmsetvaluesetting(val6(i,j),tval6(i,j))
   if(.not.autosim) then
   Edit4(i,j) = gmCreateTextEntry(panel2(j),(i-1)*iwidstat+1, 6, iwidstat-2, 1,tedit4(i,j), 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   val1(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 7, iwidstat-2, 1,tval1(i,j), 10,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)         
   	call gmsetvaluesetting(val1(i,j),tval1(i,j))

	
   val2(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 8, iwidstat-2, 1,tval2(i,j), 10,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 
					call gmsetvaluesetting(val2(i,j),tval2(i,j))
   
   Edit5(i,j) = gmCreateTextEntry(panel2(j),(i-1)*iwidstat+1, 9, iwidstat-2, 1,tedit5(i,j), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   val3(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 10, iwidstat-2, 1,tval3(i,j), 10,2, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)         
   
	call gmsetvaluesetting(val3(i,j),tval3(i,j))
	
   val4(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 11, iwidstat-2, 1,tval4(i,j), 10,2, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
					call gmsetvaluesetting(val4(i,j),tval4(i,j))
   val5(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 12, iwidstat-2, 1,tval5(i,j), 10,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)         
   	call gmsetvaluesetting(val5(i,j),tval5(i,j))

	
   
   val7(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 13, iwidstat-2, 1,tval7(i,j), 10,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)         
   	call gmsetvaluesetting(val7(i,j),tval7(i,j))

	
   val8(i,j) = gmCreateValueEntry(panel2(j),(i-1)*iwidstat+1, 14, iwidstat-2, 1,tval8(i,j), 10,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 


	call gmsetvaluesetting(val8(i,j),tval8(i,j))
	endif
   enddo
   
   fc(j)=ffilt(j)		!kHz (-3dB)
   trise(j)=332.1/fc(j)		!rise time (microsec)
if(.not.autosim) then
	if(icall.ne.-154) then
	if(ffilt(j).le.0.) then
      
    if(discprt) write(7,45) j
45		format(' Filtering and rise-time not defined for set ',i3)
		idest=0
 jEdit= gmCreateTextEntry(panel2(j),1, 16, iwidstat-2, 1,'not defined', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   else
	   
  

   Toggle14_1(j) = gmCreateToggleButton(Panel2(j), 2, 16, 1, 1, &
				'', 1, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=3060+j)

   Toggle14_2(j) = gmCreateToggleButton(Panel2(j), 2, 17, 1, 1, &
                '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=3070+j)

   Toggle14_3(j) = gmCreateToggleButton(Panel2(j), 2, 18, 1, 1, &
				'', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=3080+j)
   endif
endif
endif
enddo
if(.not.autosim) then
valt(1) = gmCreateValueEntry(panel14,(iwidstat1+isum)/2-2, 1, iwidstat, 1,tvalt(1),15,0, GEDIT, &
              	gmType=GDecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmHpos=Gleft,gmVpos=GTOP, gmExpand=GOFF) 
				call gmsetvaluesetting(valt(1),tvalt(1))
do k=2,5
 valt(k) = gmCreateValueEntry(panel14,(iwidstat1+isum)/2-2, k, iwidstat, 1,tvalt(k),15,3, GEDIT, &
              	gmType=GDecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmHpos=Gleft,gmVpos=GTOP, gmExpand=GOFF) 
				call gmsetvaluesetting(valt(k),tvalt(k))
enddo
endif
ijb=gmCreatePushButton(hjcfitform,10,1, 5, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
ijb2=gmCreatePushButton(hjcfitform,0,1, 10, 1,'Check concentration(s) and save',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall-1)
call gmdrawwindow(HjcfitForm)
call gmsetwidgetstatus(ijb2,gunselectable)

end  
