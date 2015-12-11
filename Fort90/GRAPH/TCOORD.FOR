	program tcoord
c to test Hgraph/pixel conversion
	print 1
1	format(' idev = ')
	read*,idev
	call INIPLT(idev,.false.,1.0)
	ifont=4
	csize=4.
	call SETFONT1(ifont)
	call SETCSIZE(csize,isize)    !sets isize too
	call COLTYP1(14)
c	ivxlo=0		! for VIEWPORT -landscape
c	ivxhi=10000
c	ivylo=0
c	ivyhi=7484
c
	ivxlo=0		! for VIEWPORT -portrait
	ivxhi=9900
	ivylo=0
	ivyhi=7000
	call COLTYP1(14)
	call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
	call IFRAME(ivxlo,ivxhi,ivylo,ivyhi)
	call JUSTIFYSTRING(5000,3750,'Text here',0,isize,2,2)	!2=centered
c	call anykey
c	call COLTYP1(0)
c	call IFRAME(0,10000,0,7400)	!erase
c	call anykey
c	call COLTYP1(14)
c	call IFRAME(0,10000,0,7000)
c	call anykey
	call ENDPLT
	end
