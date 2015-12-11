c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     Prims spanning tree algorithm
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      subroutine PRIM3(JCON,ICin,ncin,ICout,ncout,Iedge,nedge,
     & nerr1,nerr2,nerr3,nerr4,k,ndim,mabel,matrix,n)
c Version of PRIM in which we specify also routs that we do NOT want
c in the tree (ie to be determined by MR) in ICout,ncout.  This are
c given higher 'cost'
c cost=1 for routes specified in ICin
c cost=2 for routes that are not specified to be either in or out of tree
c cost=10 (at present) for those in ICout
c
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
	integer*4 ICin(2,200) !input routes to be in tree
	integer*4 ICout(2,200) !input routes to be out of tree (MR routes)
	integer*4 JCON(100,100)
	logical discprt
	integer mabel(100),matrix(100,100)
c
c      integer*4 label(100),iedge(100,2)
      integer*4 label(100),iedge(2,200)
        !common needed for CIRCUIT.for
	common/dp/discprt
c      common /f/ iedge(100,2),nedge
c
c Preliminary bit to get my notation into form required for Prim
	nerr1=0
	nerr2=0
	nerr3=0
	nerr4=0
	do i=1,k
	   do j=1,k
		matrix(i,j)=999		!not an edge
		if(jcon(i,j).eq.1) matrix(i,j)=2	!any old edge
	   enddo
	enddo
c Set 'cost'=1 for the ncin edges that MUST be included
	do n=1,ncin
	   i=ICin(1,n)
	   j=ICin(2,n)
	   matrix(i,j)=1
	   matrix(j,i)=1
	enddo
c Set 'cost'=10 for the ncout edges that should be excluded
	do n=1,ncout
	   i=ICout(1,n)
	   j=ICout(2,n)
	   matrix(i,j)=10
	   matrix(j,i)=10
	enddo
c
	n=k
	nsav=k

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
c         upDATEW labels
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
	if(nedge.ne.n-1) then
	   
	   nerr1=1
	 
	   if(discprt) write(7,10) nedge,n-1
10	   format(' Number edges = ',i3,' not equal to nstates - 1 = ',i3)
	endif
c Check that every vertex is included as the end point of at least one edge
	ifound=0
	do is=1,n	!check all states are in
	   do ie=1,nedge
		if(is.eq.iedge(1,ie).or.is.eq.iedge(2,ie)) then
		   ifound=ifound+1
		   goto 70	!look for next state
		endif
	   enddo
70	   continue
	enddo
	if(ifound.ne.n) then
	  
	   nerr2=1
	  
	   if(discprt) write(7,11) ifound,n
11	   format(' Only ',i3,' states found in the tree: should be ',i3)
	endif
c Incorporate checks for included and excluded routes
	if(ncin.gt.0) then
	   do n=1,ncin
		i=ICin(1,n)
		j=ICin(2,n)
		do m=1,nedge	!is i,j in tree?
		   if((iedge(1,m).eq.i.and.iedge(2,m).eq.j).or.
     &		(iedge(1,m).eq.j.and.iedge(2,m).eq.i)) then
			goto 91		!route is in, so OK
		   endif
		enddo
	
		nerr3=1
		
		if(discprt) write(7,12) i,j
12		format(' Route ',i3,'-',i3,' should be in tree but is not')
91		continue
	   enddo
	endif
c
	if(ncout.gt.0) then
	   do n=1,ncout
		i=ICout(1,n)
		j=ICout(2,n)
		do m=1,nedge	!is i,j in tree?
		   if((iedge(1,m).eq.i.and.iedge(2,m).eq.j).or.
     &		(iedge(1,m).eq.j.and.iedge(2,m).eq.i)) then
		
			nerr4=1
		
			if(discprt) write(7,13) i,j
13			format(
     &	' Route ',i3,'-',i3,' should not be in tree but is')
		   endif
		enddo
	   enddo
	endif
c
	
      
      return
      end



	subroutine GETM(i,j,m,npar,irate,jrate)
c to get parameter number, m, for q(i,j)
	integer irate(200),jrate(200)
c
	do m1=1,npar
	   if(irate(m1).eq.i.and.jrate(m1).eq.j) then
		m=m1
		goto 9
	   endif
	enddo
9	return
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
40	k = k + 1
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


	subroutine IC_JCON(IC,ncon,nd1,JCON,k,nd2)
c Converts connections from IC(2,ncon) form to JCON(k,k) form
c Input IC, ncon,k
c Output JCON
	integer*4 IC(2,nd1),JCON(nd2,nd2)
c
	do i=1,nd2
	   do j=1,nd2
		JCON(i,j)=0
	   enddo
	enddo
c
	do i=1,k
	   do j=1,k
		do m=1,ncon
		   if((IC(1,m).eq.i.and.IC(2,m).eq.j).or.
     &	      (IC(1,m).eq.j.and.IC(2,m).eq.i)) JCON(i,j)=1
		enddo
	   enddo
	enddo
	RETURN
	end


	subroutine JCON_IC(IC,ncon,nd1,JCON,k,nd2)
c Converts connections from JCON(k,k) form to IC(2,ncon) form
c Input JCON,k
c Output IC, ncon

	integer*4 IC(2,nd1),JCON(nd2,nd2)
c
	do i=1,2
	   do j=1,nd1
		IC(i,j)=0
	   enddo
	enddo
c
	ncon=0
	do i=1,k-1	!check upper triangle only
	   do j=i+1,k
		if(jcon(i,j).eq.1) then
		   ncon=ncon+1
		   ic(1,ncon)=i
		   ic(2,ncon)=j
		endif
	   enddo
	enddo
c	do i=2,k	!check lower triangle only
c	   do j=1,i-1
c		if(jcon(i,j).eq.1) then
c		   ncon=ncon+1
c		   ic(1,ncon)=i
c		   ic(2,ncon)=j
c		endif
c	   enddo
c	enddo
	RETURN
	end


