c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     Prims spanning tree algorithm
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      subroutine PRIM(JCON,ICspec,nc,Iedge,nedge,k,ndim)
c Modif 10/23/03 04:44pm to get input and output in my form
c Indices of IEDGE interchanged for this reason
c
c     label stores the distance of a node form the current spanning tree
c     mabel stores the nearest spanning tree node
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     Prims spanning tree algorithm
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c CODE AS SENT BY KATH DOWSLAND 23-Oct-03
c
	integer*4 ICspec(2,200) !input routes to be in tree
	integer*4 JCON(ndim,ndim)
c
c      integer*4 label(100),iedge(100,2)
      integer*4 label(100),iedge(2,200)
      common /a/ mabel(100)		!common needed for CIRCUIT.for
      common /b/ matrix(100,100),n  !common needed for CIRCUIT.for
c      common /f/ iedge(100,2),nedge
c
c Preliminary bit to get my notation into form required for Prim
	do i=1,k
	   do j=1,k
		matrix(i,j)=999		!not an edge
		if(jcon(i,j).eq.1) matrix(i,j)=2	!any old edge
	   enddo
	enddo
c Set 'cost'=1 for the nc edges that MUST be included
	do n=1,nc
	   i=ICspec(1,n)
	   j=ICspec(2,n)
	   matrix(i,j)=1
	   matrix(j,i)=1
	enddo
	n=k
c Check
	print 2
	write(8,2)
2	format(' Matrix in Prim routine')
	do i=1,k
	   print 1,(matrix(i,j),j=1,k)
	   write(8,1) (matrix(i,j),j=1,k)
1	   format(20i5)
	enddo
c
c Start of Prim code
c         initialise
c
      is = 1
      label(is) = 10000
      mabel(is) = is
      do j = 2,n
         label(j) = matrix(is,j)
         mabel(j) = 1
	enddo
      nedge = 0
c
c           find nearest vertex to tree
c
   30 min = 10000
      nedge = nedge + 1
      do j = 2,n
         if(label(j) .le. min) then
      	index = j
	      min = label(j)
	   endif
	enddo
c
c           add edge to tree
c
      label(index) = 10000
      iedge(1,nedge) = index
      iedge(2,nedge) = mabel(index)
      is = index
c
c         update labels
c
      do j = 2,n
         if(label(j) .ne. 10000) then
      	if(matrix(index,j) .lt. label(j)) then
      	   label(j) = matrix(index,j)
      	   mabel(j) = is
      	endif
	   endif
	enddo
c
c          test stopping condition
c
      if(nedge .lt. n-1) goto 30
      return
      end
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c          finding the fundamental circuit associated with non-tree
c          edge(i,j)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      subroutine circuit(i,j,incirc,ncirc)
      dimension inuse(100),inpath(100),incirc(100)
      common /a/ mabel(100)
      common /b/ matrix(100,100),n
c
c         find path from i to the root vertex
c
      do 10 k = 1,n
      inuse(k) = 0
   10 continue
      k = 1
      next = i
   20 inpath(k) = next
      inuse(next) = k
      if(mabel(next) .ne. next)then
         next = mabel(next)
         k = k + 1
         goto 20
      endif
c
c      find path from j to above path
c
      k = 0
      next = j
   30 if(inuse(next) .eq. 0)then
          k = k + 1
          incirc(k) = next
          next = mabel(next)
          goto 30
      endif
c
c      add relevant part of path in reverse order into circuit
c
      l = inuse(next)
   40 k = k + 1
      incirc(k) = inpath(l)
      if(l .ne. 1)then
         l = l - 1
         goto 40
      endif
	ncirc=k	!sometimes get duplicated element
      return
      end

	subroutine NCONS(Jcon,k,ncon,nstate,ndim)
c counts the number of connections in any Jcon type matrix by counting
c the number of 1s in lower triangular part of it
c And the number of states in the tree=number of rows (or cols) that
c contain a non-zero entry
	integer Jcon(ndim,ndim)
	logical sflag
c
	ncon=0
	do i=2,k	!check lower triangle only
	   do j=1,i-1
	     if(jcon(i,j).eq.1) ncon=ncon+1
	   enddo
	enddo
	nstate=0
	do i=1,k
	   sflag=.false.
	   do j=1,k
		if(jcon(i,j).eq.1) sflag=.true.	!at least one non-zero in row i
	   enddo
	   if(sflag) nstate=nstate+1
	enddo
	RETURN
	end


