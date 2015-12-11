	subroutine SMOOTH3D(z,znew,nbx,nby,power,ndx,ndy)
c To do smoothing of 3D data in z(i,j) as in Magleby -inverse distance
c weighting. Normally power=1.0 but increase it to give less weight to
c different bins.  This is 3x3 smoothing -could also add option for 5x5 smoothing
c which would include the next 'annulus' of bins round the central one.
	real*4 z(ndx,ndy),znew(ndx,ndy)
c
	w1=1.0**power	!weight for values that share a face with z(i,j)
	w2=1.0/(sqrt(2.)**power) !weight for values that share only a corner with z(i,j)
c
c First do all bins except those on edges/corners
	do i=2,nbx-1
	   do j=2,nby-1
		zn=z(i,j) + w1*(z(i+1,j)+z(i-1,j)+z(i,j+1)+z(i,j-1)) +
     &	w2*(z(i+1,j+1)+z(i+1,j-1)+z(i-1,j+1)+z(i-1,j-1))
		den=1.0 + 4.*w1 + 4.*w2
		znew(i,j)=zn/den
	   enddo
	enddo
c
c Next the edges (excluding corners)
	den=1.0 + 3.*w1 + 2.*w2
	do i=2,nbx-1
c	 (1) Left edge (j=1)
	   zn=z(i,1) + w1*(z(i+1,1)+z(i-1,1)+z(i,2)) +
     &	w2*(z(i+1,2)+z(i-1,2))
	   znew(i,1)=zn/den
c	 (2) Right edge (j=nby)
	   zn=z(i,nby) + w1*(z(i+1,nby)+z(i-1,nby)+z(i,nby-1)) +
     &	w2*(z(i+1,nby-1)+z(i-1,nby-1))
	   znew(i,nby)=zn/den
	enddo
c
	den=1.0 + 3.*w1 + 2.*w2
	do j=2,nby-1
c	 (1) Top edge (i=1)
	   zn=z(1,j) + w1*(z(1,j+1)+z(1,j-1)+z(2,j)) +
     &	w2*(z(2,j+1)+z(2,j-1))
	   znew(1,j)=zn/den
c	 (2) Bottom edge (i=nbx)
	   zn=z(nbx,j) + w1*(z(nbx,j+1)+z(nbx,j-1)+z(nbx-1,j)) +
     &	w2*(z(nbx-1,j+1)+z(nbx-1,j-1))
	   znew(nbx,j)=zn/den
	enddo
c
c Finally the four corners
	den=1.0 + 2.*w1 + w2
c (1) Top left (i=1,j=1)
	zn=z(1,1) + w1*(z(1,2)+z(2,1)) + w2*z(2,2)
	znew(1,1)=zn/den
c (2) Top right (i=1,j=nby)
	zn=z(1,nby) + w1*(z(1,nby-1)+z(2,nby)) + w2*z(2,nby-1)
	znew(1,nby)=zn/den
c (3) Bottom left (i=nbx,j=1)
	zn=z(nbx,1) + w1*(z(nbx,2)+z(nbx-1,1)) + w2*z(nbx-1,2)
	znew(nbx,1)=zn/den
c (4) Bottom right (i=nbx,j=nby)
	zn=z(nbx,nby) + w1*(z(nbx,nby-1)+z(nbx-1,nby)) +
     & w2*z(nbx-1,nby-1)
	znew(nbx,nby)=zn/den
c
	RETURN
	end


