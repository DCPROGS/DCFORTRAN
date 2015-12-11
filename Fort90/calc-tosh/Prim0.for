      common /b/ matrix(100,100),n
      common /f/ idge(100,2),nedge
      open(7,file='wedgeg.dat',status='old')
      read(7,2)n
      write(6,2)n
    2 format(100i4)
      do 100 i = 1,n
      read(7,2)(matrix(i,j),j=1,n)
 100  continue
      call prim
      open(8,file='tree.dat',status='unknown')
      do 10 i = 1,nedge
      write(8,3)idge(i,1),idge(i,2)
    3 format(2i4)
   10 continue
      end
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     Prims spanning tree algorithm
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      subroutine prim
c
c     label stores the distance of a node form the current spanning tree
c     mabel stores the nearest spanning tree node
c
      dimension label(100)
      common /a/ mabel(100)
      common /b/ matrix(100,100),n
      common /f/ idge(100,2),nedge
c
c         initialise
c
      is = 1
      label(is) = 10000
      mabel(is) = is
      do 10 j = 2,n
      label(j) = matrix(is,j) 
      mabel(j) = 1
   10 continue
      nedge = 0
c
c           find nearest vertex to tree
c
   30 min = 10000
      nedge = nedge + 1
      do 20 j = 2,n
      if(label(j) .gt. min)goto 20
      index = j
      min = label(j)
   20 continue
c
c           add edge to tree
c
      label(index) = 10000
      idge(nedge,1) = index
      idge(nedge,2) = mabel(index)
      is = index
      do 40 j = 2,n
c
c         update labels
c
      if(label(j) .eq. 10000)goto 40
      if(matrix(index,j) .lt. label(j))then
          label(j) = matrix(index,j)
          mabel(j) = is
      endif
   40 continue
c
c          test stopping condition
c
      if(nedge .lt. n-1)goto 30
      return
      end  
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c          finding the fundamental circuit associated with non-tree
c          edge(i,j)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      subroutine circuit(i,j)
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
      return
      end