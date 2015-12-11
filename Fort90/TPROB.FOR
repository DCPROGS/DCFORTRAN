5	print 1
1	format(' Student''s t = ')
	call INPUTr(tval)
	print 3
3	format('&d.f. = ')
	call INPUTi(ndf)
2	format(i8)
	vr=tval*tval
	n1=1
	call FDIST(VR,n1,ndf,P)
	print 4,p
4	format(' P = ',g13.4)
	goto 5
	end

