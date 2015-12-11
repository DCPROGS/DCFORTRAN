	program tsetline
	integer*1 isavlin(2000)	!should need 4*80 bytes for 640 pixels
	character ans*1,UC*1
	logical mouse_on,mous_set,horizont
	common/mousval/mouse_on,nbutton
c
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	print *,' Horizontal ? '
	read 101,ans
101	format(a1)
	horizont=.false.
	if(UC(ans).ne.'N') horizont=.true.
	call INIPLT(0,.false.,scalfac)
	call SCALE(-50.,50.,-50.,50.)
	ibk=1
	call FILLWIN(0,0,639,479,ibk)

	ix=1

	do i=1,10
	   call VLINE(ix,0,479,12)
	   ix=ix+63
	enddo
	val=5.
	eps=.05
	icol=14
	irhi=3
	iclo=66
	amin=-45.
	amax=45.
	call SETLINE(horizont,val,eps,6,2,amin,amax,
     & icol,irhi,iclo,isavlin,ibk,ikey)
	print*,' value = ',val,':   ikey = ',ikey
	print*,' amin, amax = ',amin,amax
	call ENDPLT()
	end

