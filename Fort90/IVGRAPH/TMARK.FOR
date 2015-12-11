	program TMARK
c To test icon that marks transitions in SCAN
c -if icon is to be moveable using rdwin1,wrwin1 need it to be defined
c in a window that is exact multiple of 8 -prevents marker being placed
c precisely?
	call MODE(18)
	ixlo=
	ixlo=ixlo-mod(ixlo,8)
	iylo=iylo-mod(iylo,16)
	ibk=7
	ic=15
	call FILLWIN(0,0,639,480,ibk)
	call wrstring18(x,y,'5',ic,ibk)

