subroutine select_sets(main,nset,select_set,itogset)


use menu_f90
integer :: select_set,DATA_SETS,itogset(20)

ix=6
iy=nset+2

select_set = gmCreateComplexDialogueBox(Main, 4, 1, ix, iy, GALL,'Select sets:', &
				 gmIconFormat=GDLLICON,gmIconFile='MBIG1040',gmCallback=410,gmvpos=gtop)

isets = gmCreatePanel(select_set, 0, 0, ix, iy, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=Gpressed, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


do i=1,nset
	itogset(i)=gmCreateToggleButton(isets,1,i,3,1,'Set '//CHAR(48+i),&
	GOFF,gmToggleType=GTICKBOX,gmVpos=GTOP,gmCallback=i+410)
enddo
call gmdrawwindow(select_set)
end