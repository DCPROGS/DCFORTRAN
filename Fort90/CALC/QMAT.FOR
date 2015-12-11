	subroutine QMAT(Q,Amat,k,eigenval,ifail)
c Calculation of eigenvalues and spectral matrices using the NAG library
c routine F02AGF
c INPUT:
c	Q=double precision matrix (kxk)
c	k=size of Q
c OUTPUT:
c	eigenval(i)=eigenvalues of Q (i=1,...,k)
c	A(m)=m'th spectral matrix of Q (m=1,..,k). Each A(m) is kxk in
c	  size and Amat(i,j,m) is value in ith row anf jth column of A(m)
c	ifail=0 if there are no errors in the NAG routine, F02AGF
c
	IMPLICIT double precision (A-H,O-Z)
	double precision Q(10,10),Amat(10,10,10),eigenval(10)
	double precision EM(10,10),EN(10,10)
c next 2 lines for F02AGF
	double precision QD(10,10),eigimag(10),EMimag(10,10)
	integer*4 iwork(10)
c
c Make copy of Q for F02AGF so input value of Q not lost
	do i=1,k
	   do j=1,k
		QD(i,j)=Q(i,j)
	   enddo
	enddo
c
	km=10		!maximum k defined by declarations
	ifail=0
	call F02AGF(QD,km,k,eigenval,eigimag,EM,km,EMimag,km,
     & iwork,ifail)
c
C EM has columns that are the real parts of the k column eigenvectors
c of Q.
c Want fastest tau (=largest neg eigenvalue) first, so sort into ascending
c order.
	do i=1,k
	   iwork(i)=i	!keep track of sorted order
	enddo
	call SORTD(eigenval,iwork,k,km)		!SORTS EIGENVALUES
c Reorder columns of EM accordingly
	do j=1,k
	   m=iwork(j)
	   do i=1,k
		QD(i,j)=EM(i,m)	!EM with columns reordered in QD temporarily
	   enddo
	enddo
	do i=1,k
	   do j=1,k
		EM(i,j)=QD(i,j)	!Copy reordered EM back to EM(i,j)
	   enddo
	enddo
c
c Now invert EM; result in EN.
	call MATINV(EM,k,km,EN,km)
c Calculate the spectral matrices, A(m), from EM and EN.
	do m=1,k
	   do i=1,k
		do j=1,k
		   Amat(i,j,m)=EM(i,m)*EN(m,j)
		enddo
	   enddo
	enddo
	RETURN
	end


