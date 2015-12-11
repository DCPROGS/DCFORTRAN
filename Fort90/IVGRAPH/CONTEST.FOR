	program contest
	character*1 getch,a
c============================================================================
	call sethg()
c----------------------------------------


	call INIPLT(0,.false.,1.0)
	call coltyp18(11)
	call clears(0,480,2)
	ixviewlo=5000
	iyviewlo=3500
	ixviewhi=9000
	iyviewhi=7000
	ixgraphlo=1000
	iygraphlo=1000
	ixgraphhi=3000
	iygraphhi=3000
	xscalelo=-10.
	xscalehi=-4.
	yscalelo=2.
	yscalehi=10.

	xaxislo=-9.
	xaxishi=-5.
	yaxish=6.

	xaxisv=-7.
	yaxislo=3.
	yaxishi=9.
	call plot(ixviewlo,iyviewhi,0)
	call plot(ixviewlo,7500,1)

	call VIEWPORT(ixviewlo,ixviewhi,iyviewlo,iyviewhi)
	call GRAPHBOUNDARY(ixgraphlo,ixgraphhi,iygraphlo,iygraphhi)
	call scale(xscalelo,xscalehi,yscalelo,yscalehi)
c draw viewport borders:
	call plot(ixviewlo,iyviewlo,0)
	call plot(ixviewlo,iyviewhi,1)
	call plot(ixviewhi,iyviewhi,1)
	call plot(ixviewhi,iyviewlo,1)
	call plot(ixviewlo,iyviewlo,1)
	call plot(ixviewhi,iyviewhi,0)
	call plot(ixviewhi,7500,1)
c test for pixels:
	a=getch(b)
	call hgtopix(ixviewlo,iyviewlo,ixpixlo,iypixlo)
	call hgtopix(ixviewhi,iyviewhi,ixpixhi,iypixhi)
	call hline(ixpixlo,ixpixhi,iypixlo,12)
	call hline(ixpixlo,ixpixhi,iypixhi,12)
	call vline(ixpixlo,iypixlo,iypixhi,12)
	call vline(ixpixhi,iypixlo,iypixhi,12)
c test for inverse conversion
	a=getch(b)
	call pixtohg(ixpixlo,iypixlo,ixviewlo,iyviewlo)
	call pixtohg(ixpixhi,iypixhi,ixviewhi,iyviewhi)
	call plot(ixviewlo,iyviewlo,0)
	call plot(ixviewlo,iyviewhi,1)
	call plot(ixviewhi,iyviewhi,1)
	call plot(ixviewhi,iyviewlo,1)
	call plot(ixviewlo,iyviewlo,1)


	a=getch(b)
	call coltyp18(15)
c draw graphboundary borders
	call moveto(xscalelo,yscalelo,0,0)
	call moveto(xscalelo,yscalehi,1,0)
	call moveto(xscalehi,yscalehi,1,0)
	call moveto(xscalehi,yscalelo,1,0)
	call moveto(xscalelo,yscalelo,1,0)
c draw axis
	call coltyp18(4)
	call moveto(xaxislo,yaxish,0,0)
	call moveto(xaxishi,yaxish,1,0)
	call moveto(xaxisv,yaxislo,0,0)
	call moveto(xaxisv,yaxishi,1,0)
c test for pixels
	a=getch(b)
	call wdtopix(xscalelo,yscalelo,ixpixlo,iypixlo)
	call wdtopix(xscalehi,yscalehi,ixpixhi,iypixhi)
	call hline(ixpixlo,ixpixhi,iypixlo,13)
	call hline(ixpixlo,ixpixhi,iypixhi,13)
	call vline(ixpixlo,iypixlo,iypixhi,13)
	call vline(ixpixhi,iypixlo,iypixhi,13)
	call wdtopix(xaxislo,yaxislo,ixpixlo,iypixlo)
	call wdtopix(xaxishi,yaxishi,ixpixhi,iypixhi)
	call wdtopix(xaxisv,yaxish,ixpixv,iypixh)
	call hline(ixpixlo,ixpixhi,iypixh,11)
	call vline(ixpixv,iypixlo,iypixhi,11)
c test for inverse conversion
	a=getch(b)
	call pixtowd(ixpixlo,iypixlo,xaxislo,yaxislo)
	call pixtowd(ixpixhi,iypixhi,xaxishi,yaxishi)
	call pixtowd(ixpixv,iypixh,xaxisv,yaxish)
	call moveto(xaxislo,yaxish,0,0)
	call moveto(xaxishi,yaxish,1,0)
	call moveto(xaxisv,yaxislo,0,0)
	call moveto(xaxisv,yaxishi,1,0)
c test to know pixelwise graphboundary point (ixgraphlo,iygraphlo) related
c to this viewport:
	call gbtopix(ixgraphlo,iygraphlo,ixviewlo,iyviewlo,ixpix,iypix)
	print*,'1. ixpix :',ixpix
	print*,'1. iypix :',iypix
	call wrstring18(ixpix,iypix,'A',1,2)
	call hline(ixpix,ixpix+20,iypix,1)
	a=getch(b)
c===================================================================
c  change viewport:
c===================================================================
	ixviewlo=500
	iyviewlo=2000
	ixviewhi=4500
	iyviewhi=6000
	call VIEWPORT(ixviewlo,ixviewhi,iyviewlo,iyviewhi)
	call GRAPHBOUNDARY(ixgraphlo,ixgraphhi,iygraphlo,iygraphhi)
	call scale(xscalelo,xscalehi,yscalelo,yscalehi)
c draw viewport borders:
	call plot(ixviewlo,iyviewlo,0)
	call plot(ixviewlo,iyviewhi,1)
	call plot(ixviewhi,iyviewhi,1)
	call plot(ixviewhi,iyviewlo,1)
	call plot(ixviewlo,iyviewlo,1)
c test for pixels:
	a=getch(b)
	call hgtopix(ixviewlo,iyviewlo,ixpixlo,iypixlo)
	call hgtopix(ixviewhi,iyviewhi,ixpixhi,iypixhi)
	call hline(ixpixlo,ixpixhi,iypixlo,12)
	call hline(ixpixlo,ixpixhi,iypixhi,12)
	call vline(ixpixlo,iypixlo,iypixhi,12)
	call vline(ixpixhi,iypixlo,iypixhi,12)

c test for inverse conversion
	a=getch(b)
	call pixtohg(ixpixlo,iypixlo,ixviewlo,iyviewlo)
	call pixtohg(ixpixhi,iypixhi,ixviewhi,iyviewhi)
	call plot(ixviewlo,iyviewlo,0)
	call plot(ixviewlo,iyviewhi,1)
	call plot(ixviewhi,iyviewhi,1)
	call plot(ixviewhi,iyviewlo,1)
	call plot(ixviewlo,iyviewlo,1)
	a=getch(b)
	call coltyp18(15)
c draw graphboundary borders
	call moveto(xscalelo,yscalelo,0,0)
	call moveto(xscalelo,yscalehi,1,0)
	call moveto(xscalehi,yscalehi,1,0)
	call moveto(xscalehi,yscalelo,1,0)
	call moveto(xscalelo,yscalelo,1,0)
	call coltyp18(4)
	call moveto(xaxislo,yaxish,0,0)
	call moveto(xaxishi,yaxish,1,0)
	call moveto(xaxisv,yaxislo,0,0)
	call moveto(xaxisv,yaxishi,1,0)
c test for pixels
	a=getch(b)
	call wdtopix(xscalelo,yscalelo,ixpixlo,iypixlo)
	call wdtopix(xscalehi,yscalehi,ixpixhi,iypixhi)
	call hline(ixpixlo,ixpixhi,iypixlo,13)
	call hline(ixpixlo,ixpixhi,iypixhi,13)
	call vline(ixpixlo,iypixlo,iypixhi,13)
	call vline(ixpixhi,iypixlo,iypixhi,13)
	call wdtopix(xaxislo,yaxislo,ixpixlo,iypixlo)
	call wdtopix(xaxishi,yaxishi,ixpixhi,iypixhi)
	call wdtopix(xaxisv,yaxish,ixpixv,iypixh)
	call hline(ixpixlo,ixpixhi,iypixh,11)
	call vline(ixpixv,iypixlo,iypixhi,11)
c test for inverse conversion
	a=getch(b)
	call pixtowd(ixpixlo,iypixlo,xaxislo,yaxislo)
	call pixtowd(ixpixhi,iypixhi,xaxishi,yaxishi)
	call pixtowd(ixpixv,iypixh,xaxisv,yaxish)
	call moveto(xaxislo,yaxish,0,0)
	call moveto(xaxishi,yaxish,1,0)
	call moveto(xaxisv,yaxislo,0,0)
	call moveto(xaxisv,yaxishi,1,0)
c test to know pixelwise graphboundary point (ixgraphlo,iygraphlo) related
c to this viewport:
	call gbtopix(ixgraphlo,iygraphlo,ixviewlo,iyviewlo,ixpix,iypix)
	call set_cursor(0,25)
	print*,'2. ixpix :',ixpix
	print*,'2. iypix :',iypix
	call wrstring18(ixpix,iypix,'A',1,2)
	call hline(ixpix,ixpix+20,iypix,1)
	a=getch(b)
	call ENDPLT
c====================================================


	end


