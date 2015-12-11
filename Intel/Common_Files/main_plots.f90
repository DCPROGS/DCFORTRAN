subroutine main_plots(main,miplot0,mtoggle,kplot,kset,iplot_Toggle0,nlig,ligname,conc,nset)

use menu_f90
integer mToggle(10)
character*(80) radio_text3(10),text
character*20 ligname(10)
real*4 conc(10,10)
character*11 cnum,cnum0
integer iplot_Toggle0(10)
call gDefineRGB(38,1.,0.5,0.25)	!orange
	radio_text3(1)=' (1) Plot histogram of apparent OPEN periods, with fit'
	radio_text3(2)=' (2) Plot histogram of apparent SHUT times, with fit'
	radio_text3(3)=' (3) Open time pdf conditional on adjacent gap'
	radio_text3(4)=' (4) Mean open vs adjacent gap plot, with fit' 
	radio_text3(5)=' (5) Dependency plot'
	radio_text3(6)=' (6) P(open) curves (ideal and HJC)'
miplot0 = gmCreateComplexDialogueBox(main,1, 9, 14+nset,11 , GALL, 'Select Plot', &
   gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')


! Create main panel for form
iplot_MainPanel=gmCreatePanel(miplot0, 0, 0, 17, 14+nset, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=220, gmScrollMode=Gnobars)

 iradio01 = gmCreateRadioBox(iplot_mainPanel, 1, 1, 15, nset+2, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=101,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Display set:',&
	  gmVpos=Gtop)

	do i=1,nset
		call intconv(i,cnum)
		call realtoch(conc(1,i)*1000000,cnum0,11)
		text=cnum(1:3)//ligname(1)//' Concentration [uM] ='//cnum0
		if(nlig.eq.2) then
			nl=len_trim(text)
			call realtoch(conc(2,i),cnum0,11)
			text=text(1:nl)//ligname(2)//' Concentration ='//cnum0
		endif
		iplot_Toggle0(i) = gmCreateToggleButton(iradio01, 1, i , 13, 1, text, 0, &
			gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
	enddo
	call gmSetToggleSwitch(	iplot_Toggle0(kset),Gon)

iradio0 = gmCreateRadioBox(iplot_mainpanel, 1, nset+4, 15, 8, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=102,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Plot type:',&
	  gmVpos=Gtop)

do i=1,6
   mToggle(i) = gmCreateToggleButton(iradio0, 1, i , 13, 1, radio_text3(i), 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
enddo
call gmSetToggleSwitch(	mtoggle(kplot),Gon)
iradio_Button1 = gmCreatePushButton(iplot_MainPanel,9,0 , 7, 1, 'Continue',gmType=GUSERDEFINED, &
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=4509)
   iradio_Button1 = gmCreatePushButton(iplot_MainPanel,1,0 , 7, 1, 'Exit plots', gmType=GUSERDEFINED,&
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=123, gmTextCol=1,&
   gmcallback=4510)
   call gmdrawwindow(miplot0)
end