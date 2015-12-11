	program ALPHA
c To reset screen to alpha mode (eg after crash of program while
c in graphics)
c	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c For Toshiba VIDEOTYP()=18 for graphics mode, =3 for alpha mode
	integer*2 i1,i2,videotyp
c
	i1=videotyp()
c	if(videotyp().eq.18) call VIDEOMOD(3)
	call VIDEOMOD(3)
	i2=videotyp()
	print 1,i1,i2
1	format(' video mode changed from ',i3,' to ',i3)
c
	end


