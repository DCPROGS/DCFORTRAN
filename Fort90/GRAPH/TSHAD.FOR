	ixlo=50
	ixhi=100
	iylo=100
	iyhi=150
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo=8*(ixlo/8)		!round down
	ixhi=8*(ixhi/8)		!round down
c
	call mode(18)
c	call FILLWIN(0,0,639,479,14)
	call FILLWIN(0,0,639,479,5)
	call SHADOW(ixlo,ixhi,iylo,iyhi,3,7)
	icb=12
	call FILLWIN(ixlo,iylo,ixhi,iyhi,icb)
	pause
	ixlo=250
	ixhi=500
	iylo=200
	iyhi=450
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo=8*(ixlo/8)		!round down
	ixhi=8*(ixhi/8)		!round down
	call SHADOW(ixlo,ixhi,iylo,iyhi,5,2)
	icb=9
	call FILLWIN(ixlo,iylo,ixhi,iyhi,icb)
	pause
	ixlo=450
	ixhi=600
	iylo=350
	iyhi=470
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo=8*(ixlo/8)		!round down
	ixhi=8*(ixhi/8)		!round down
	call SHADOW(ixlo,ixhi,iylo,iyhi,4,3)
	icb=14
	call FILLWIN(ixlo,iylo,ixhi,iyhi,icb)
	pause
	ixlo=50
	ixhi=500
	iylo=130
	iyhi=180
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo=8*(ixlo/8)		!round down
	ixhi=8*(ixhi/8)		!round down
	call SHADOW(ixlo,ixhi,iylo,iyhi,0,7)
	icb=9
	call FILLWIN(ixlo,iylo,ixhi,iyhi,icb)
	pause
	ixlo=450
	ixhi=500
	iylo=100
	iyhi=130
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo=8*(ixlo/8)		!round down
	ixhi=8*(ixhi/8)		!round down
	call SHADOW(ixlo,ixhi,iylo,iyhi,2,7)
	icb=9
	call FILLWIN(ixlo,iylo,ixhi,iyhi,icb)
	pause
	ixlo=450
	ixhi=500
	iylo=50
	iyhi=80
c NB at present ixlo,ixhi should be integer multiples of 8 for FILLWIN!
	ixlo=8*(ixlo/8)		!round down
	ixhi=8*(ixhi/8)		!round down
	call SHADOW(ixlo,ixhi,iylo,iyhi,0,7)
	icb=9
	call FILLWIN(ixlo,iylo,ixhi,iyhi,icb)
	pause
	call mode(3)
	end

