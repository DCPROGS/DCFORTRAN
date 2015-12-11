subroutine array_table(main,ixa,iya,i_array_form,i_array,xtext,ncols,nrows,array_value,array_text,&
				itypeval,icall,ixgrid,iygrid)

   use menu_f90
   real array_value(10,20)
   character*30 array_text(10,20)
   integer itypeval(10),i_array(10),i_array_form(10)
   type(GARRAYCELL) arrayattribs
   character  xtext*100,cnum*11,title*20
   
call gDefineRGB(112,0.96,0.82,0.82)
call gDefineRGB(113,0.93,0.78,0.78)
call gDefineRGB(114,0.90,0.74,0.74)
call gDefineRGB(115,0.87,0.70,0.70)
call gDefineRGB(111,0.98,0.92,0.92)
call gDefineRGB(38,1.,0.5,0.25)	!orange
ind=icall-4530
call intconv(ind,cnum)
title='set: '//cnum
iaw=ixgrid*2*ncols+80
iah=25*(nrows+4)
call gmSetGuiGridMode(GOFF)
   i_array_form(ind)=  gmCreateComplexDialogueBox(Main,ixa*24,iya*24,iaw , iah, GALL, title, &
              	 gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='Gee')
   i_array_pan= gmCreatePanel(i_array_form(ind), 0, 0,iaw , iah-iygrid, &
              	gmType=GINVERSECHISEL, gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmScrollMode=Gnobars, gmVpos=GTOP, gmExpand=GOFF)


   i_array(ind) = gmCreateValueArray(i_array_pan, 0, 0, iaw, iah-iygrid, ncols, nrows, &
              	gmAxisW=40, gmAxisH=24, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=Gcentre, gmYjust=GCENTRE, gmVpos=GTOP,gmScrollMode=GBOTHBARS, gmExpand=GOFF)
   
   call gmEnqCellAttribs(i_array(ind), 1, 1, arrayattribs)
   arrayattribs%width=ixgrid*2
   arrayattribs%height=25
   arrayattribs%display=gedit
   arrayattribs%justify=Gleft
   
   do i=1,ncols
	  arrayattribs%backcol=110+i
      do j=1,nrows
		call gmSetCellAttribs(i_array(ind), i,j , arrayattribs)
		if(itypeval(i).eq.-1) then
			call gmSetCellSetting(i_array(ind), i,j ,gmstring= array_text(i,j) )
		else
			if (x.ne.0.0.and.(x.lt.10e-5.or.x.gt.10e5)) then
			arrayattribs%format=gscientific
			else
			arrayattribs%format=0
			endif
		!	arrayattribs%ndp=itypeval(i)
			arrayattribs%ndp=3
			call gmSetCellAttribs(i_array(ind), i,j , arrayattribs)
			x=array_value(i,j)
			call gmSetCellSetting(i_array(ind), i,j ,gmValue= x )
		endif
	  enddo
   enddo
   ible=ncols*4+3

   call gmSetGuiGridMode(GON)
   iradB = gmCreatePushButton(i_array_form(ind),6,0 ,5, 1, 'Help cut and paste', gmType=GUSERDEFINED,&
   gmhpos=gright,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=4545)
   iradB = gmCreatePushButton(i_array_form(ind),3,0 ,3 , 1, 'Save to ini', gmType=GUSERDEFINED,&
   gmhpos=gright,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=4544)
   iradB = gmCreatePushButton(i_array_form(ind),0,0 ,3 , 1, 'Continue', gmType=GUSERDEFINED,&
   gmhpos=gright,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=icall)
	call gmdrawwindow(i_array_form(ind))
end