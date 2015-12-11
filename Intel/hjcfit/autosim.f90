subroutine autosim1(main,iautosim,nset,nintt,nsim,simfile1,simfile,conc,nlig,&
			iatext1,iatext2,nvalc,ivals,ivalsim,ligname,isimtog,simfile3,iatext3,&
			iresw,resw,itogrec)

use menu_f90
character*40 simfile,simfile1,simfile3

integer :: nvalc(10,10),ivals(10),iresw(10)

integer :: iaText1
integer :: iaText2,panel1,panel2

real*4 conc(10,10),resw(10)
integer nintt(10)
character*20 ligname(10)
character*11 cnum

if(itogrec.eq.8) then
    ihg=10+2*nset
else
   ihg=14+3*nset
endif
call gDefineRGB(38,1.,0.5,0.25)	!orange
iautosim=gmCreateComplexDialogueBox(Main, 4, 4, 20,ihg, GALL, 'Simulate SCAN files ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)

Panel1=gmCreatePanel(iautosim, 0, 0, 20, 8+2*nset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         , &
              	gmHpos=GLEFT, gmVpos=gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)


iStatic1_2 = gmCreateTextEntry(Panel1, 1, 1, 13, 1,'Concentration (micromolar):', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
do j=1,nset
call intconv(j,cnum)
iStatic1_2 = gmCreateTextEntry(Panel1, 2, j+1, 2, 1,'Set '//cnum, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
do i=1,nlig
	iStatic1_2 = gmCreateTextEntry(Panel1,4*(2*i-1) , j+1, 2, 1,ligname(i), 20, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
	xs=1.e6*conc(i,j)
	nvalc(i,j) = gmCreateValueEntry(panel1,8*i, j+1, 3, 1,xs, 10,4, GEDIT, &
              	gmType=Gdecimal, gmJustify=Gleft, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF) 
enddo
enddo   

iStatic1_3 = gmCreateTextEntry(Panel1, 1, nset+2, 13, 1,' Number of intervals to be simulated:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
 do i=1,nset
 call intconv(i,cnum)
iStatic1_2 = gmCreateTextEntry(Panel1, 2, nset+2+i, 2, 1,'Set '//cnum, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

ivals(i) = gmCreateValueEntry(panel1,5, i+2+nset, 3, 1,nintt(i), 10,0, GEDIT, &
              	gmType=Gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF) 
enddo 


iStatic1_4 = gmCreateTextEntry(Panel1, 1, 2*nset+3, 10, 1,'File name for binary output of simulations', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

iatext1= gmCreateTextEntry(Panel1, 11, 2*nset+3, 8, 1,simfile1, 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic1_4 = gmCreateTextEntry(Panel1, 1, 2*nset+4, 10, 1,'File name for text output of simulations', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
 
iatext2 = gmCreateTextEntry(Panel1, 11, 2*nset+4, 8, 1,simfile, 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)  

iStatic1_4 = gmCreateTextEntry(Panel1, 1, 2*nset+5, 10, 1,'Number of experiments to be simulated ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

ivalsim = gmCreateValueEntry(panel1,11, 2*nset+5, 5, 1,nsim, 10,0, GEDIT, &
              	gmType=Gdecimal, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF) 
              	
iStatic1_4 = gmCreateTextEntry(Panel1, 1, 2*nset+6, 10, 1,'File name for text output of seeds (& dwt)', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=Gtop, gmExpand=GOFF)
 
iatext3 = gmCreateTextEntry(Panel1, 11, 2*nset+6, 8, 1,simfile3, 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, gmstatus=gselectable,&
              	gmVpos=Gtop, gmExpand=GOFF)
 
 if(itogrec.eq.8) goto 9
Panel2=gmCreatePanel(iautosim, 0, 8+2*nset, 20, 4+nset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         , &
              	gmHpos=GLEFT, gmVpos=gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=15, gmFillBorder=0)             	  
isimTog= gmCreateToggleButton(panel2, 1, 1, 18, 1, ' Write result to QUB (DWT) file', 0,&
    gmType=G3DCHECKBOX, gmVpos=Gtop,gmstatus=gselectable,gmcallback=3016)
 
iStatic1_4 = gmCreateTextEntry(Panel2, 1, 2, 10, 1,'Resolution for ASCII output[us]:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gtop, gmExpand=GOFF)
 do i=1,nset
 call intconv(i,cnum)
iStatic1_2 = gmCreateTextEntry(Panel2, 2, 2+i, 2, 1,'Set '//cnum, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmVpos=GTOP, gmExpand=GOFF)

iresw(i) = gmCreateValueEntry(panel2,5, i+2, 3, 1,resw(i), 10,2, GEDIT, &
              	gmType=Gdecimal, gmJustify=GCENTRE, &
              	gmstatus=gunselectable, &
              	gmVpos=GTOP, gmExpand=GOFF) 
enddo 

9 continue
initwin = gmCreatePushButton(iautosim, 1, 0, 18, 1, 'Continue', &
    gmVpos=Gbottom,gmType=Guserdefined,gmOffcol=38,gmcallback=3019) !-40)

call gmdrawwindow(iautosim)

end
