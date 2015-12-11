
subroutine progress_table(main)
use menu_f90
integer :: MainPanel,toggle_main(20),progressform
character*20 toggle_text(20)

toggle_text(1)='Model'
toggle_text(2)='File'
toggle_text(3)='Risetime'
toggle_text(4)='Resolution'
iheight=10
progressForm=gmCreatemdiComplexDialogueBox(Main, 35, 1, 4, iheight, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052')
mainPanel=gmCreatePanel(progressform, 0, 0, 4, iheight, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

 do i=1,4
		Toggle_main(i) = gmCreateToggleButton(mainPanel, 0, i, 4, 1, &
				toggle_text(i), 0, &
              	gmType=G3DCHECKBOX, gmsize=80,gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)
 enddo
 end