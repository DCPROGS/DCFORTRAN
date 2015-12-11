	subroutine ivline(ixlo,iylo,ixhi,iyhi,ic,idot)
c==========================================================================
c	draws a dotted line similar with dc curve lines;
c	works fine with horizontal,vertical and step lines with type 0,3,4,5
c	some problems with step lines 1 and 2 :works perfect only at 45 degrees
c==========================================================================

	axis=400
c here you can enter other further types:
	select case(idot)
		case(0)
			call iline(ixlo,iylo,ixhi,iyhi,ic)
			goto 1
		case(1)
			zseq1=0.3
			zseq2=0.5
		case(2)
			zseq1=1.5
			zseq2=2.
		case(3)
			zseq1=5.
			zseq2=2.5
		case(4)
			zseq1=6.
			zseq2=2.
			zseq3=1.5
			zseq4=2.
		case(5)
			zseq1=6.
			zseq2=2.
			zseq3=1.5
			zseq4=2.
			zseq5=1.5
			zseq6=2.
	end select
	dl1=(0.01*zseq1*axis)
	dl2=(0.01*zseq2*axis)
	dl3=(0.01*zseq3*axis)
	dl4=(0.01*zseq4*axis)
	dl5=(0.01*zseq5*axis)
	dl6=(0.01*zseq6*axis)
	if (ixlo.eq.ixhi) then
		alpha=1.57
	else
		alpha=atan(float(iyhi-iylo)/float(ixhi-ixlo))
	endif
	idx1=ifixr(cos(alpha)*dl1)
	idy1=ifixr(sin(alpha)*dl1)
	idx2=ifixr(cos(alpha)*(dl1+dl2))
	idy2=ifixr(sin(alpha)*(dl1+dl2))
	idx3=ifixr(cos(alpha)*(dl1+dl2+dl3))
	idy3=ifixr(sin(alpha)*(dl1+dl2+dl3))
	idx4=ifixr(cos(alpha)*(dl1+dl2+dl3+dl4))
	idy4=ifixr(sin(alpha)*(dl1+dl2+dl3+dl4))
	idx5=ifixr(cos(alpha)*(dl1+dl2+dl3+dl4+dl5))
	idy5=ifixr(sin(alpha)*(dl1+dl2+dl3+dl4+dl5))
	idx6=ifixr(cos(alpha)*(dl1+dl2+dl3+dl4+dl5+dl6))
	idy6=ifixr(sin(alpha)*(dl1+dl2+dl3+dl4+dl5+dl6))
2     continue
	ix=ixlo
	iy=iylo
10	continue
	if(ix+idx1.gt.ixhi.or.iy+idy1.gt.iyhi) then
		call iline(ix,iy,ixhi,iyhi,ic)
		goto 1
	endif
	if(idot.eq.1) then
		idx1=0
		idy1=0
	endif
	call iline(ix,iy,ix+idx1,iy+idy1,ic)
	if (idot.le.3) then
		ix=ix+idx2
		iy=iy+idy2
		goto 3
	endif
	if(ix+idx2.gt.ixhi.or.iy+idy2.gt.iyhi) goto 1
	if(ix+idx3.gt.ixhi.or.iy+idy3.gt.iyhi) then
		call iline(ix+idx2,iy+idy2,ixhi,iyhi,ic)
		goto 1
	endif
	call iline(ix+idx2,iy+idy2,ix+idx3,iy+idy3,ic)
	if(idot.eq.4) then
		ix=ix+idx4
		iy=iy+idy4
		goto 3
	endif
	if(ix+idx4.gt.ixhi.or.iy+idy4.gt.iyhi) goto 1
	if(ix+idx5.gt.ixhi.or.iy+idy5.gt.iyhi) then
		call iline(ix+idx4,iy+idy4,ixhi,iyhi,ic)
		goto 1
	endif
	call iline(ix+idx4,iy+idy4,ix+idx5,iy+idy5,ic)
	ix=ix+idx6
	iy=iy+idy6
3	if(ix.le.ixhi.and.iy.le.iyhi) goto 10
1	end
