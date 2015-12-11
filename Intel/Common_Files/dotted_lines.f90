subroutine dotted_lines(linetyp,xmin,xmax,y,dxs)
real zseq(10)


x=xmin

select case(linetyp)
	case(1,6,7,8,9)
	kseq=2
	zseq(1)=0.3*dxs*1.2
	zseq(2)=0.5*dxs*1.2
	
	do while(x.lt.xmax)	
	call movto2(x,y)
	x=x+zseq(1)
	call linto2(x,y)
	x=x+zseq(2)
	
	enddo
case(2)
	kseq=2
	zseq(1)=1.5*dxs*1.2
	zseq(2)=2.*dxs*1.2
	do while(x.lt.xmax)	
	call movto2(x,y)
	x=x+zseq(1)
	call linto2(x,y)
	x=x+zseq(2)
	enddo
case(3)
	kseq=2
	zseq(1)=5.*dxs*1.2
	zseq(2)=2.5*dxs*1.2
	do while(x.lt.xmax)	
	call movto2(x,y)
	x=x+zseq(1)
	call linto2(x,y)
	x=x+zseq(2)
	enddo
case(4)
	kseq=4
	zseq(1)=6.*dxs*1.2
	zseq(2)=2.*dxs*1.2
	zseq(3)=1.5*dxs*1.2
	zseq(4)=2.*dxs*1.2
	do while(x.lt.xmax)	
	call movto2(x,y)
	x=x+zseq(1)
	call linto2(x,y)
	x=x+zseq(2)
	call movto2(x,y)
	x=x+zseq(3)
	call linto2(x,y)
	x=x+zseq(4)
	enddo
case(5)
	kseq=6
	zseq(1)=6.*dxs*1.2
	zseq(2)=2.*dxs*1.2
	zseq(3)=1.5*dxs*1.2
	zseq(4)=2.*dxs*1.2
	zseq(5)=1.5*dxs*1.2
	zseq(6)=2.*dxs*1.2
	do while(x.lt.xmax)	
	call movto2(x,y)
	x=x+zseq(1)
	call linto2(x,y)
	x=x+zseq(2)
	call movto2(x,y)
	x=x+zseq(3)
	call linto2(x,y)
	
	x=x+zseq(4)
	call movto2(x,y)
	x=x+zseq(5)
	call linto2(x,y)
	x=x+zseq(6)
	enddo
end select




end