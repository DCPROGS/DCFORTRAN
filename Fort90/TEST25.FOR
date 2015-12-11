	program TEST25
c to test CONV25.SRC
	common/DBOX/Lblast,textlast,nblast,cslast,iflast,ihb
	common/dp/x
	common/DBOX/Lblast,textlast,nblast,cslast,iflast,ihb
c
	call TITENT1()
	input 3,m
3	format(i8)
	call DRAWBOX(5,Lb,text,2.,ifont)
	input 4,x,y
4	format(2g13.6)
	call TITENT1()
	end


