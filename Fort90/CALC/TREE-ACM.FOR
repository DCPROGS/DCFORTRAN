	subroutine ***(dm,n,mst,imst,cst)
c Parameters:
c   dm(i,j) is length of edge from node i to node j, 1.le.i and j.le.in
c	If no connection between i and j set dm(i,j)=10.**10
c   n=number of nodes
c   mst=array in which edge list is placed
c    	mst(1,i)=original node #
c	mst(2,i)=terminal node of edge i (i=1,2,...,imst)
c   imst=number of edges in array mst
c   cst=sum of edge lengths for tree
c
c Internal
c	nit()=array of nodes not yet in tree
c	nitp=number of entries in nit
c	ji(i)=node of partial MST closest to node nit(i)
c	ui(i)=length of edge from nit(i) to ji(i)
c	kp=next node to be added to MST
c
c
	cst=0.
	nitp=n-1
	kp=n
	imst=0
	do i=1,nitp
	   nit(i)=i
============
c===	   ui(i)=dm(i,kp)		!=length of i to kp (=10^10 if no connection)
===	   ici(i)=jtree(i,kp)	!=1 if i connected to kp, 0 otherwise
===========
	   ji(i)=kp
	enddo
c
200	continue	!loop back to here until all nodes in tree
c update labels of nodes not yet in tree
	do i=1,nitp
	   ni=nit(i)
	   d=dm(ni,kp)
	   if(ui(i).gt.d) then		===always if no con bet i,kp
===	   if(d.le.ui(i)) then		===never if no con bet ni,kp
		ui(i)=d
		ji(i)=kp
	   endif
	enddo
c find node outside tree, nearest to tree
	uk=ui(1)	!==ui(i)? i not def!
	do i=1,nitp
	   if(ui(i).le.uk) then  !=====never true if no con
		uk=ui(i)
		k=i
	   endif
	enddo
c
c put nodes of appropriate edge into array mst
	imst=imst+1
	mst(1,imst)=nit(k)
	mst(2,imst)=ji(k)
	cst=cst+uk
	kp=nit(k)
c
c delete new tree nodes from array IT
	ui(k)=ui(nitp)
	nit(k)=nit(nitp)
	ji(k)=ji(nitp)
	nitp=nitp-1
	if(nitp.gt.0) goto 200
c
	RETURN
	end



