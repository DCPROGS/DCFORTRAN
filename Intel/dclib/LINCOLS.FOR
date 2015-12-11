	subroutine lincols(ic,idev)
c idev=0: screen
c idev=1: .wmf
c idev=2: .cgm
c idev=3,4: postscript printer
c idev=5  : laserjet
c idev=6  : color deskjet
c See also GSETCOLS.FOR
c
	if(idev.eq.0) then
		call lincol(ic)
		goto 100
	endif
	kap=0
	if(idev.eq.-1) then
	   idev=0
	   kap=-1
	endif
	if(idev.eq.-6) then
	   idev=6
	   kap=-6
	endif
	select case(ic)
		case(100)
			if(idev.eq.1) ica=104
			if(idev.eq.2) ica=104
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=0
			if(idev.eq.6) ica=13
		case(101)
			if(idev.eq.1) ica=100
			if(idev.eq.2) ica=100
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=10
			if(idev.eq.6) ica=7
		case(102)
			if(idev.eq.1) ica=102
			if(idev.eq.2) ica=102
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=6
			if(idev.eq.6) ica=5
		case(103)
			if(idev.eq.1) ica=111
			if(idev.eq.2) ica=111
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=8
			if(idev.eq.6) ica=6
		case(104)
			if(idev.eq.1) ica=106
			if(idev.eq.2) ica=106
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=2
			if(idev.eq.6) ica=2
		case(105)
			if(idev.eq.1) ica=102
			if(idev.eq.2) ica=102
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=12
			if(idev.eq.6) ica=8
		case(106)
			if(idev.eq.1) ica=1
			if(idev.eq.2) ica=1
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=4
			if(idev.eq.6) ica=9
		case(107)
			if(idev.eq.1) ica=7
			if(idev.eq.2) ica=107
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=14
			if(idev.eq.6) ica=0
		case(108)
			if(idev.eq.1) ica=8
			if(idev.eq.2) ica=108
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=15
			if(idev.eq.6) ica=11
		case(109)
			if(idev.eq.1) ica=9
			if(idev.eq.2) ica=100
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=9
			if(idev.eq.6) ica=7
		case(110)
			if(idev.eq.1) ica=10
			if(idev.eq.2) ica=110
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=5
			if(idev.eq.6) ica=5
		case(111)
			if(idev.eq.1) ica=11
			if(idev.eq.2) ica=111
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=7
			if(idev.eq.6) ica=6
		case(112)
			if(idev.eq.1) ica=12
			if(idev.eq.2) ica=106
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=1
			if(idev.eq.6) ica=2
		case(113)
			if(idev.eq.1) ica=13
			if(idev.eq.2) ica=102
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=11
			if(idev.eq.6) ica=8
		case(114)
			if(idev.eq.1) ica=14
			if(idev.eq.2) ica=101
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=3
			if(idev.eq.6) ica=4
		case(115)
			if(idev.eq.1) ica=15
			if(idev.eq.2) ica=105
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=13
			if(idev.eq.6) ica=10
	end select
	if(kap.eq.-1) then
	   ic=ica
	   idev=-1
	else if(kap.eq.-6) then
	   ic=ica
	   idev=-6
	else
	   call lincol(ica)
	endif

100	end

