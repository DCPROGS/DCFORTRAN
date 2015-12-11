	program testgino
	character*15 printfil


	call gino
	call VGA
	call gsetcols(0)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call mode(18)
	icol=2
	call papenq(xp,yp,ipap)
	ifnt=2
	vxlo=0.
	vxhi=213.
	vylo=0.
	vyhi=150.
	idev=0
	call GRAPHBOUNDARY(10.,200.,10.,150.)
	call gSCALE(0.,20.,-2.,2.,is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)
C	TEST CHARACTERS
	call lincols(1,idev)
	call chafnt(ifnt)
	call gramov(1.,1.)
	call chastr('Test the wmf files')
C 	TEST LINES
	call gramov(5.,-1.)
	call lincols(2,idev)
	call gralin(15.,-1.)
	call lincols(3,idev)
	call gralin(15.,1.)
	call lincols(4,idev)
	call gralin(5.,1.)
	call lincols(5,idev)
	call gralin(5.,-1.)
	pause
	call devend
	call winmet()

	end
	subroutine winmet()
	call wmfp(100,0,0,640,480)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call GRAPHBOUNDARY(10.,200.,10.,150.)
	call gSCALE(0.,20.,-2.,2.,is)
	call mode(18)
	call FILLWIN(0,0,639,387,1)		!graph area
	call DEFDIALOG(1,1,2,4,60,8)	!define dialog box #1
	call DEFDIALOG(2,1,65,4,14,8)	!define dialog box #2
	call OPENDIALOG(1,3,.true.)		!draw dialog box #1
	call OPENDIALOG(2,4,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',14)
	call WDIALOG(2,'F2=HELP INDEX',14)
C	TEST CHARACTERS
C	TEST CHARACTERS
	idev=6
	call lincols(1,idev)
	call chafnt(ifnt)
	call gramov(1.,1.)
	call chastr('Test the wmf files')
C 	TEST LINES
	call gramov(5.,-1.)
	call lincols(2,idev)
	call gralin(15.,-1.)
	call lincols(3,idev)
	call gralin(15.,1.)
	call lincols(4,idev)
	call gralin(5.,1.)
	call lincols(5,idev)
	call gralin(5.,-1.)
	call gramov(4.,-0.8)
	call lincols(6,idev)
	call broken(1)
	call linwid(1.)
	call gralin(14.,-0.8)
	call lincols(7,idev)
	call gralin(14.,0.8)
	call lincols(8,idev)
	call gralin(4.,0.8)
	call lincols(9,idev)
	call gralin(4.,-0.8)
	call WDIALOG(1,'Windows metafile done. . .',12)
	call devend
	call COPY('wmfp.out','meta.wmf')

	call VGA
	call gsetcols(0)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call mode(18)
	icol=2
	ifnt=2
	vxlo=0.
	vxhi=213.
	vylo=0.
	vyhi=150.
	idev=0
	call GRAPHBOUNDARY(10.,200.,10.,150.)
	call gSCALE(0.,20.,-2.,2.,is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)
C	TEST CHARACTERS
	call lincols(1,idev)
	call chafnt(ifnt)
	call gramov(1.,1.)
	call chastr('Test the wmf files')
C 	TEST LINES
	call gramov(5.,-1.)
	call lincols(2,idev)
	call gralin(15.,-1.)
	call lincols(3,idev)
	call gralin(15.,1.)
	call lincols(4,idev)
	call gralin(5.,1.)
	call lincols(5,idev)
	call gralin(5.,-1.)
	pause
c	call ginend
	call mode(3)
	end
