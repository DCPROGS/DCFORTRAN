	subroutine gsetcols(iset)


	select case(iset)
	   case(0)
		call rgbdef(0,0.,0.,0.)
		call rgbdef(1,0.,0.,0.66)
		call rgbdef(2,0.,0.66,0.)
		call rgbdef(3,0.,0.66,0.66)
		call rgbdef(4,0.66,0.,0.)
		call rgbdef(5,0.66,0.,0.66)
		call rgbdef(6,0.66,0.33,0.)
		call rgbdef(7,0.66,0.66,0.66)
		call rgbdef(8,0.33,0.33,0.33)
		call rgbdef(9,0.,0.,1.)
		call rgbdef(10,0.,1.,0.)
		call rgbdef(11,0.,1.,1.)
		call rgbdef(12,1.,0.,0.)
		call rgbdef(13,1.,0.,1.)
		call rgbdef(14,1.,1.,0.)
		call rgbdef(15,1.,1.,1.)
	   case(1)
	      call rgbdef(0,0.,0.,0.)
		call rgbdef(1,0.,0.,0.2)
		call rgbdef(2,0.,0.,0.4)
		call rgbdef(3,0.,0.,0.6)
		call rgbdef(4,0.,0.,0.8)
		call rgbdef(5,0.,0.2,1.)
		call rgbdef(6,0.,0.4,1.)
		call rgbdef(7,0.,0.6,1.)
		call rgbdef(8,0.,0.8,1.)
		call rgbdef(9,0.,0.,1.)
		call rgbdef(10,0.,1.,1.)
		call rgbdef(11,0.2,0.,1.)
		call rgbdef(12,0.4,0.,1.)
		call rgbdef(13,0.6,0.,1.)
		call rgbdef(14,0.8,0.,1.)
		call rgbdef(15,1.,1.,1.)
	   case(2)
	      call rgbdef(0,0.,0.,0.)
		call rgbdef(1,0.,0.2,0.)
		call rgbdef(2,0.,0.4,0.)
		call rgbdef(3,0.,0.6,0.)
		call rgbdef(4,0.,0.8,0.)
		call rgbdef(5,0.,1.,0.2)
		call rgbdef(6,0.,1.,0.4)
		call rgbdef(7,0.,1.,0.6)
		call rgbdef(8,0.,1.,0.8)
		call rgbdef(9,0.,1.,1.)
		call rgbdef(10,0.2,1.,1.)
		call rgbdef(11,0.4,1.,1.)
		call rgbdef(12,0.5,1.,1.)
		call rgbdef(13,0.6,1.,1.)
		call rgbdef(14,0.8,1.,1.)
		call rgbdef(15,1.,1.,1.)
	   case(4)
	      call rgbdef(0,0.,0.,0.)
		call rgbdef(1,0.2,0.,0.)
		call rgbdef(2,0.4,0.,0.)
		call rgbdef(3,0.6,0.,0.)
		call rgbdef(4,0.8,0.,0.)
		call rgbdef(5,1.,0.,0.)
		call rgbdef(6,1.,0.,0.2)
		call rgbdef(7,1.,0.,0.4)
		call rgbdef(8,1.,0.,0.6)
		call rgbdef(9,1.,0.,0.8)
		call rgbdef(10,1.,0.,1.)
		call rgbdef(11,1.,0.2,1.)
		call rgbdef(12,1.,0.4,1.)
		call rgbdef(13,1.,0.6,1.)
		call rgbdef(14,1.,0.8,1.)
		call rgbdef(15,1.,1.,1.)
	   case(5)
		call rgbdef(0,0.,0.,0.)
		call rgbdef(1,0.,0.,1.)
		call rgbdef(2,0.,1.,0.)
		call rgbdef(3,0.,1.,1.)
		call rgbdef(4,1.,0.,0.)
		call rgbdef(5,1.,0.,1.)
		call rgbdef(6,1.,1.,0.)
		call rgbdef(7,1.,1.,1.)
		call rgbdef(9,0.,0.,0.66)
		call rgbdef(10,0.,0.66,0.)
		call rgbdef(11,0.,0.66,0.66)
		call rgbdef(12,0.66,0.,0.)
		call rgbdef(13,0.66,0.,0.66)
		call rgbdef(14,0.66,0.33,0.)
		call rgbdef(15,0.66,0.66,0.66)
		call rgbdef(8,0.33,0.33,0.33)
	   case(100)
		call rgbdef(100,0.,0.,0.)	     !black
		call rgbdef(101,0.,0.,0.66)        !blue
		call rgbdef(102,0.,0.66,0.)        !green
		call rgbdef(103,0.,0.66,0.66)      !cyan
		call rgbdef(104,1.,0.,0.4)         !red
		call rgbdef(105,0.66,0.,0.66)      !violet
		call rgbdef(106,0.66,0.33,0.)      !brown
		call rgbdef(107,0.66,0.66,0.66)    !grey
		call rgbdef(108,0.33,0.33,0.33)    !greu
		call rgbdef(109,0.,0.,1.)          !blue
		call rgbdef(110,0.,1.,0.)          !green
		call rgbdef(111,0.,1.,1.)          !cyan
		call rgbdef(112,1.,0.,0.)          !red
		call rgbdef(113,1.,0.,1.)          !violet
		call rgbdef(114,1.,1.,0.)          !yellow
		call rgbdef(115,1.,1.,1.)          !white
c		call rgbdef(116,0.2,0.4,0.6)       !blue
		call rgbdef(116,1.,1.,0.8)         !pale yellow
		call rgbdef(117,0.2,0.8,0.6)       !green
		call rgbdef(118,0.6,1.0,1.0)       !blue
		call rgbdef(119,0.8,0.2,0.4)       !red
		call rgbdef(120,1.0,0.2,0.6)       !pink
		call rgbdef(121,0.8,0.4,0.2)       !brown
		call rgbdef(122,1.,0.4,0.2)        !orange
		call rgbdef(123,1.,0.6,0.2)        !orange
		call rgbdef(124,1.,0.8,0.0)        !gold
		call rgbdef(125,0.8,0.4,0.6)       !rose
		call rgbdef(126,0.5,0.75,0.96)     !sky
		call rgbdef(127,0.4,0.6,0.76)      !sky
		call rgbdef(128,0.31,0.48,0.63)    !sky
c		call rgbdef(129,0.22,0.33,0.43)    !sky
		call rgbdef(129,0.22,0.35,0.49)    !sky
		call rgbdef(130,0.13,0.2,0.25)     !sky
		call rgbdef(131,0.08,0.11,0.14)    !sky
	      call rgbdef(132,0.9,0.9,0.9)
	end select
	end
