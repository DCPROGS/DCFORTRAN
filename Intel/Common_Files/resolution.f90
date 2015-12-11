!     Last change:  D    15 Jan 104    4:41 pm
subroutine resolution(main,iresoform,nset,tvalres1,tvalres2,tvalres3,tvalres4,tvalres5,&
	tvalres6,tvalres7,tvalres8,tvalres9,valres1,valres2,valres3,valres4,valres5,&
	valres6,valres7,valres8,valres9,autosim)	
	
	use menu_f90
	
	integer panel1,iresoform,panel2,panel3
	integer valres1(10),valres2(10),valres3(10),valres4(10),valres5(10)
	integer valres6(10),valres7(10),valres8(10),valres9(10)
	real tvalres1(10),tvalres2(10),tvalres3(10),tvalres4(10),tvalres5(10)
	real tvalres6(10),tvalres7(10),tvalres8(10),tvalres9(10)
	integer toggleres1(10),toggleres2(10),toggleres3(10)
	logical autosim
    character*11 cnum
	iwidstat=5
    iwidstat1=8
   
    call gDefineRGB(38,1.,0.5,0.25)	!orange
	isum=iwidstat*nset+iwidstat1+1
	isum1=isum
	if (isum.gt.32) isum1=28
    iysum=20
	if(nset.eq.1) isum=isum+2
	if(autosim) iysum=8
call gDefineRGB(220, 1.000000, 1.000000, 0.807843) !yellow
call gDefineRGB(161,0.80,1.,0.8) !green
call gDefineRGB(122,0.80,0.89,0.87) !blue

iResoForm=gmCreateComplexDialogueBox(Main, 6, 4, isum1+2, iysum+2, GALL, 'Set the resolution for fit', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
do j=1,nset
call intconv(j,cnum)
iStatic = gmCreateTextEntry(iresoform, iwidstat1+(j-1)*iwidstat+2, 0, iwidstat-1, 1,'Set no:'//cnum(1:3),&
                32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
enddo
Panel1=gmCreatePanel(iResoForm, 1, 1, isum, 4, &
      gmHpos=GLEFT, gmVpos=gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
      gmLineCol=0, gmLineBorder=0, gmFillCol=220, gmFillBorder=0)
iStatic1_5 = gmCreateTextEntry(Panel1, 1, 1, iwidstat1-1, 1,'Resolution [microsec]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
do j=1,nset
	valres3(j) = gmCreateValueEntry(panel1,iwidstat1+(j-1)*iwidstat, 1, iwidstat-1, 1,tvalres3(j), 7,3, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			
enddo

 
iStatic1_12 = gmCreateTextEntry(Panel1, 1, 2, iwidstat1-1, 1,'pA for real difference ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
do j=1,nset
	valres5(j) = gmCreateValueEntry(panel1,iwidstat1+(j-1)*iwidstat, 2, iwidstat-1, 1,tvalres5(j), 7,3, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &               
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 
              	call gmsetvaluesetting(valres5(j),tvalres5(j))
				 
enddo

if(autosim) then
Panel2=gmCreatePanel(iResoForm, 1, 6, isum, 3,&                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
              	gmHpos=GLEFT, gmVpos=gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=161, gmFillBorder=0)
iStatic1_6 = gmCreateTextEntry(Panel2, 1, 1, iwidstat1-1, 1,'Full amplitude [pA]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
do j=1,nset
valres4(j) = gmCreateValueEntry(panel2,iwidstat1+(j-1)*iwidstat, 1, iwidstat-1, 1,tvalres4(j), 7,3, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			
enddo
else
Panel2=gmCreatePanel(iResoForm, 1, 6, isum, 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         , &
              	gmHpos=GLEFT, gmVpos=gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=161, gmFillBorder=0)
iStatic1_6 = gmCreateTextEntry(Panel2, 1,1 , iwidstat1, 1,'Filter [kHz]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

do j=1,nset
valres_1 = gmCreateValueEntry(panel2,iwidstat1+(j-1)*iwidstat, 1, iwidstat-1, 1,tvalres8(j), 7,3, Gdisplay, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			
enddo
iStatic1_6 = gmCreateTextEntry(Panel2, 1,2 , iwidstat1, 1,'Full amplitude [pA]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

do j=1,nset
valres4(j) = gmCreateValueEntry(panel2,iwidstat1+(j-1)*iwidstat, 2, iwidstat-1, 1,tvalres4(j), 7,3, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			

enddo
iStatic1_6 = gmCreateTextEntry(Panel2, 1,3 , iwidstat1, 1,'RMS noise [pA]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

do j=1,nset
valres_2 = gmCreateValueEntry(panel2,iwidstat1+(j-1)*iwidstat, 3, iwidstat-1, 1,tvalres9(j), 7,3, Gdisplay, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			

enddo
ijb=gmCreatePushButton(panel2,1,4, 14, 1,'Recalculate false event rate (if resolution changed)',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=122,gmTextCol=7, &
              	gmVpos=Gtop, gmHpos=Gleft,gmExpand=GOFF,gmCallback=3041)
!iStatic1_2 = gmCreateTextEntry(Panel2, 1, 4, 10, 1,&
!'Recalculate false event rate (if resolution changed)', 32768, GDISPLAY, &
!              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
!              	gmVpos=GTOP, gmExpand=GOFF)

!irad= gmCreateRadioBox(Panel2, 11, 4, 4, 1, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
!      gmLineBorder=GOUTEREDGE, gmFillCol=161, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)
!iTog = gmCreateToggleButton(irad, 0, 0, 2, 1, 'Yes', 0, &
!      gmType=G3DRADIO, gmVpos=GTOP,gmCallback=3041)
	  
!iTog = gmCreateToggleButton(irad, 2, 0, 2, 1, 'No', 1, &
!      gmType=G3DRADIO, gmVpos=GTOP,gmCallback=3042)


iStatic1_8 = gmCreateTextEntry(Panel2, 2, 5, iwidstat1-2, 1,'false event rate(per sec)', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)


iStatic1_9 = gmCreateTextEntry(Panel2, 2, 6 ,iwidstat1-2, 1,'filter rise time (microsec)', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
do j=1,nset
valres6(j) = gmCreateValueEntry(panel2,iwidstat1+(j-1)*iwidstat, 5, iwidstat-1, 1,tvalres6(j), 9,2, GEDIT, &
              	gmType=gscientific, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  

valres7(j) = gmCreateValueEntry(panel2,iwidstat1+(j-1)*iwidstat, 6, iwidstat-1, 1,tvalres7(j), 9,2, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
enddo

Panel3=gmCreatePanel(iResoForm, 1, 15, isum, 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         , &
              	gmHpos=GLEFT, gmVpos=gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=122, gmFillBorder=0)



iStatic1_2 = gmCreateTextEntry(Panel3, 1, 1, 10, 1,'Exclude amplitudes in a specified range', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
irad1= gmCreateRadioBox(Panel3, 11, 1, 4, 1, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=122, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)
iTog = gmCreateToggleButton(irad1, 0, 0, 2, 1, 'Yes', 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmCallback=3043)
	  
iTog = gmCreateToggleButton(irad1, 2, 0, 2, 1, 'No', 1, &
      gmType=G3DRADIO, gmVpos=GTOP,gmCallback=3044)   
iStatic1_3 = gmCreateTextEntry(Panel3, 2, 2, iwidstat1-2, 1,'low amplitude [pA]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic1_4 = gmCreateTextEntry(Panel3, 2, 3, iwidstat1-2, 1,'high amplitude [pA]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

do j=1,nset
valres1(j) = gmCreateValueEntry(panel3,iwidstat1+(j-1)*iwidstat, 2, iwidstat-1, 1,tvalres1(j), 10,2, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			
valres2(j) = gmCreateValueEntry(panel3,iwidstat1+(j-1)*iwidstat, 3, iwidstat-1, 1,tvalres2(j), 7,3, GEDIT, &
              	gmType=gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)  
			
enddo
endif


ijb=gmCreatePushButton(iresoform,1,0, isum1-4, 1,'Continue',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=3051)
call gmdrawwindow(iresoform)
end
