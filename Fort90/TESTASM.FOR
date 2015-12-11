	program testasm
c passing args changed!!!!!!!!!!1
c	vline? integer*2?????????????????????????


	integer*2 x,y1,y2

	call mode(18)
	call clears(0,480,1)

	pause

	call fillwin(100,100,300,300,12)

	pause

	call hline(100,300,100,0)

	pause
	x=300
	y1=100
	y2=200
	call vline(100,100,300,0)
	call vline(x,y1,y2,10)
	call hline(100,228,50,0)

	pause
c	call fillcircle(200,200,50,14)

	pause
	call mode(3)
	a=3.33
	b=3.58
	print*,ifixr(a),',',ifixr(b)

	pause
	end
