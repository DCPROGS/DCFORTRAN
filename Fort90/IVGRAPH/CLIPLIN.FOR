	subroutine cliplin(ix1,iy1,ix2,iy2,ixlo,iylo,ixhi,iyhi)

	real*4	ix(4),iy(4),d(4),ix0(4),iy0(4)
	real*4	ix1,iy1,ix2,iy2,ixlo,iylo,ixhi,iyhi

	if(ixlo.gt.ixhi) then
	   itemp=ixlo
	   ixlo=ixhi
	   ixhi=itemp
	endif
	if(iylo.gt.iyhi) then
	   itemp=iylo
	   iylo=iyhi
	   iyhi=itemp
	endif
	if((ix1.lt.ixlo.and.ix2.lt.ixlo).or.
     &	(ix1.gt.ixhi.and.ix2.gt.ixhi)) goto 1
	if((iy1.lt.iylo.and.iy2.lt.iylo).or.
     &	(iy1.gt.iyhi.and.iy2.gt.iyhi)) goto 1
	if((ix1.gt.ixlo.and.ix1.lt.ixhi).and.
     &	(ix2.lt.ixhi.and.ix2.gt.ixlo).and.
     &      (iy1.gt.iylo.and.iy1.lt.iyhi).and.
     &	(iy2.gt.iylo.and.iy2.lt.iyhi)) goto 1
	ix(1)=ixlo
	iy(1)=iylo
	ix(2)=ixhi
	iy(2)=iylo
	ix(3)=ixhi
	iy(3)=iyhi
	ix(4)=ixlo
	iy(4)=iyhi
	do i=1,4
	   if(ix1.eq.ix2) then
	      d(i)=ix(i)-ix1
	   else
		d(i)=iy(i)-(((iy1-iy2)*(ix(i)-ix1)-iy1*ix1+iy1*ix2)/
     &	(ix2-ix1))
	   endif
	enddo
	if((d(1).gt.0.and.d(2).gt.0.and.d(3).gt.0.and.d(4).gt.0).or.
     &   (d(1).lt.0.and.d(2).lt.0.and.d(3).lt.0.and.d(4).lt.0)) goto 1
	do i=1,4
	   ix(i)=-100.
	   iy(i)=-100.
	   ix0(i)=-100.
	   iy0(i)=-100.
	enddo
	ixlx=ixlo
	iylx=iy1+(ixlo-ix1)*(iy2-iy1)/(ix2-ix1)
	if(iylx.ge.iylo.and.iylx.le.iyhi) then
	   ix(1)=ixlx
	   iy(1)=iylx
	endif
	ixhx=ixhi
	iyhx=iy1+(ixhi-ix1)*(iy2-iy1)/(ix2-ix1)
	if(iyhx.ge.iylo.and.iyhx.le.iyhi) then
	   ix(2)=ixhx
	   iy(2)=iyhx
	endif
	iyly=iylo
	ixly=ix1+(iylo-iy1)*(ix2-ix1)/(iy2-iy1)
	if(ixly.ge.ixlo.and.ixly.le.ixhi) then
	   ix(3)=ixly
	   iy(3)=iyly
	endif
	iyhy=iyhi
	ixhy=ix1+(iyhi-iy1)*(ix2-ix1)/(iy2-iy1)
	if(ixhy.ge.ixlo.and.ixhy.le.ixhi) then
	   ix(4)=ixhy
	   iy(4)=iyhy
	endif
	j=0
	do i=1,4
	   if(ix(i).ne.-100.) then
		j=j+1
		ix0(j)=ix(i)
		iy0(j)=iy(i)
	    endif
	enddo
	ix1=ix0(1)
	iy1=iy0(1)
	if(ix0(2).ne.-100.) then
		ix2=ix0(2)
		iy2=iy0(2)
	endif
1     return
	end
