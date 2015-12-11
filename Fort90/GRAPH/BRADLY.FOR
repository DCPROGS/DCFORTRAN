	program bradly

	character*6 name
	call gino
	call wmf(60,640,480)
	call devinf(name,l,iver,rval)
	print *,name,'driver is version ', iver,'l=',l,'rval=', rval
	call devend
	call wmfp(60,0,0,640,480)
	call devinf(name,l,iver,rval)
	print *,name,'driver is version ', iver,'l=',l,'rval=', rval
	call devend
	stop
	end
