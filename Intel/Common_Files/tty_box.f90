subroutine tty_box(imainpanel,itty,ixpos,iypos,iwid,ilg)
use menu_f90
itty = gmCreateComplexDialogueBox(Main, ixpos, iypos, iwid, ilg, GALL, 'Fitting ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
ittypanel=gmCreatePanel(itty, 0, 0, iwid, ilg, &
              	gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

call gmdrawwindow(itty)
end