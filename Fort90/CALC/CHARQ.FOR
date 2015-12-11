	subroutine CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
c	subroutine CHARQ(charmod,ilast,jlast)	!args for TMODWIN
c To decode the character 'mechanism' input by CHARMOD to get connections,
c cycles etc. The section for identifying cycles now moved to separate
c subrroutine CYCQ.FOR
c Note only valid connections out of cell ij , if ij is a state, are:
c           |
c       -- ij -- if ij is a state;
c           |
c but if ij=a connexion then it can join on to appropriate corners
c Note that no diagonals allowed in this version
c
c Modified 01/23/01 12:44pm to deal with 2 digit numbers (=closed states)
c
	character*2 charmod(25,40)
	character*1 s
	integer is(100),js(100)
	INTEGER IC(2,200)		!for connections
c	integer IM(8,15),JM(8,15)	!for cycles (final output)
cc make arrays for cycles bigger, NSC particularly, so can collect many
cc cycles before rejecting the repeated ones; make ICYC 2-D internally
cc to store several cycles
c	integer nsc0(8)			!for output of nsc()
c	integer NSC(20),icyc(20),icycsav(20,20)
c	integer nsi(4),ns(10,4),nck(10),nc(10,4)	!for search for cycles
c	integer last(200),nod(200),nsc1(100),ind(100),melim(100),mcyc(100)	!ditto
	integer iout(100),jout(100),icon(100)
	integer iout1(100),jout1(100),icon1(100)
	logical caplock,debug
c
	integer nsrow(25)		!number of states per row (for statmod)
	common/sm/nsmax		!for getqd/charq
	debug()=caplock()
c
	nrow=25
	ncol=40
c ilast,jlast (found in CQLAST) are last row and col of CHARMOD that contain
c info.
c Count number of open and closed states, and record which i,j specify
c the CHARMOD(i,j) elements in which they occur
	kA=0
	kF=0
	nsmax=0
	do i=1,ilast
	   nsrow(i)=0	
	   do j=1,jlast
		s=charmod(i,j)(1:1)
		if(s.eq.'o'.or.s.eq.'O') then
		   kA=kA+1
		   m=ichar(charmod(i,j)(2:2))-48		!state number=2nd char
		   if(m.eq.0) m=10
		   nsrow(i)=nsrow(i)+1
		   is(m)=i
		   js(m)=j
		else if(s.eq.'c'.or.s.eq.'C'.or.
     &	   (ichar(s).ge.48.and.ichar(s).le.57)) then
		   kF=kF+1
		   if(s.eq.'c'.or.s.eq.'C') then
		      m=ichar(charmod(i,j)(2:2))-48		!state number=2nd char
			if(m.eq.0) m=10
		   else	!2 digit number
		      call CHTOREAL(charmod(i,j)(1:2),real)
			m=ifixr(real)
		   endif
		   nsrow(i)=nsrow(i)+1
		   is(m)=i
		   js(m)=j
		endif
	   enddo
	   if(nsrow(i).gt.nsmax) nsmax=nsrow(i)
	enddo
c
	k=kA+kF
c	if(debug()) then
c	   print 11,ilast,jlast,kA,kF
c11	   format(' ilast,jlast,kA,kF = ',4i5)
c	   do 12 i=1,k
c12	   print 13,i,is(i),js(i)
c13	   format(' i,is(i),js(i) = ',3i5)
c	endif
c
c Now find CONNECTIONS
c Following method finds each connection twice, as i,j and as j,i; this is
c fixed below
c
c######eg CK+block
c	ncon=3
c	ic(1,1)=1	!=i
c	ic(2,1)=2	!=j
c	ic(1,2)=2	!=i
c	ic(2,2)=4	!=j
c	ic(1,3)=1	!=i
c	ic(2,3)=3	!=j
c
c Go through each state in turn, and find all ways out of it by inspecting
c the 8 adjacent cells for a valid connection
c Then follow each of the valid connections, through any further valid
c connection-characters, until the state to which it leads is identified
	ncon=0
	do 20 ks=1,k	!loop over all states
c	ic(1,ncon+1)=ks		!start connection at current state
	i=is(ks)		!i,j=element of CHARMOD with this state
	j=js(ks)
	call LOOK(charmod,i,j,iout,jout,nout,icon,0,0,
     &	nrow,ncol)
c	print 21,ks,nout
c21	format(' ks,nout= ',2i5)
c	if(nout.ge.1) then
c	   do 22 m=1,nout
c22	   print 23,m,iout(m),jout(m),icon(m)
c23	   format(' m,iout(m),jout(m),icon(m)= ',4i5)
c	endif
c Way(s) out of state # ks now found- follow each of these ways out, via
c any further valid connection-characters if present, until the state to which
c each leads is identified. Look at the 8 cells that surround the way out- if
c there is a second connection then there should be only one way out (to
c the continued connection- connections cannot branch!)- and if the 1st
c connection leads directly to the next state then there should also
c be only one way out from the 1st connection (one connection can lead
c only to one state)
	do 24 n=1,nout
	   i=iout(n)
	   j=jout(n)
	   iprev=i        !record so can exclude call we are coming from
	   jprev=j
	   iexc=is(ks)    !exclude cell we are coming from
	   jexc=js(ks)
	   call LOOK(charmod,i,j,iout1,jout1,nout1,icon1,iexc,jexc,
     &	nrow,ncol)
31	   continue		!return here to follow long connections
	   if(nout1.gt.1) then
		call bell(2)
		print 26,i,j
26		format(' MORE THAN ONE WAY OUT OF CELL i,j= ',2i5)
c	      do 25 m=1,nout1
c25	      print 23,m,iout1(m),jout1(m),icon1(m)
	   endif
	   if(icon1(1).ge.1.and.icon(1).le.100) then      !next state found
		ks1=icon1(1)
c check here if this route has already been located (in opp direction); if
c so do not record it again
c		if(ncon.gt.1) then
		if(ncon.ge.1) then
		   do 30 n1=1,ncon
30		   if(ic(1,n1).eq.ks1.and.ic(2,n1).eq.ks) goto 24	!skip
		endif
		ncon=ncon+1
		ic(1,ncon)=ks		!state from which we started
		ic(2,ncon)=ks1		!end connection at state located
	   else if(icon1(1).eq.-1) then			!connection continues
c look for next connection now,
	      iexc=iprev    !exclude cell we are coming from
	      jexc=jprev
	      i=iout1(1)
	      j=jout1(1)
		iprev=i        !record so can exclude call we are coming from
		jprev=j
c		if(debug()) print 33,iexc,jexc,i,j
c33		format(' iexc,jexc,i,j = ',4i5)
		call LOOK(charmod,i,j,iout1,jout1,nout1,icon1,iexc,jexc,
     &	nrow,ncol)
c		if(debug()) print 341,nout1,iout1(1),jout1(1),icon1(1)
c341		format(' nout1,iout1(1),jout1(1),icon1(1) = ',4i5)
		goto 31		!check if reached a state yet
	   else
		call BELL(3)
		print 32
32		format(' ERROR')
	   endif
24	continue
20	continue
c	do 27 m=1,ncon
c27	print 28,m,ic(1,m),ic(2,m)
c28	format(' m,ic(1,m),ic(2,m) = ',3i5)
c
	RETURN
	end

	subroutine LOOK(charmod,i,j,iout,jout,nout,icon,iexc,jexc,
     &	nrow,ncol)
c To look at the 8 cells adjacent to CHARMOD(i,j) and find those that
c are not blank (NOUT values of CHARMOD(i1,j1) where i1=iout(m),m=1,2,..nout,
c j1=similar, and ICON(m)=-1 if the corresp value is a connection (rather
c than a state), otherwise ICON(m)=0-9=state number
c One cell (i=iexc,j=jexc) can be excluded from search (iexc=0 prevents
c any exclusion)
c Note only valid connections out of cell ij , if ij is a state, are:
c           |
c       -- ij -- if ij is a state;
c           |
c but if ij=a connexion then it can join on to appropriate corners
c Note that no diagonals allowed in this version
c
	integer iout(100),jout(100),icon(100)
	logical sij,OK,debug,caplock
	character*2 charmod(25,40)
c
	debug()=caplock()
c
	nout=0	!initially
	mij=IVAL(charmod,i,j)
	sij=mij.ge.1.and.mij.le.100		!cell i,j contains a state
c Look at elements above and below ij, and each side of ij (not diagonals)
	do 1 i1=i-1,i+1
	do 1 j1=j-1,j+1
	if(i1.eq.i.and.j1.eq.j) goto 1		!skip central cell
	if(i1.eq.i+1.and.j1.eq.j+1) goto 1		!bot R diag
	if(i1.eq.i-1.and.j1.eq.j-1) goto 1		!top L diag
	if(i1.eq.i+1.and.j1.eq.j-1) goto 1		!bot L diag
	if(i1.eq.i-1.and.j1.eq.j+1) goto 1		!top R diag
	if(i1.lt.1.or.i1.gt.nrow) goto 1		!skip out of bounds
	if(j1.lt.1.or.j1.gt.ncol) goto 1		!skip out of bounds
	if(iexc.ge.1.and.i1.eq.iexc.and.j1.eq.jexc) goto 1		!skip
	m=IVAL(charmod,i1,j1)
c	if(debug()) print 200,mij,m,i1,j1,sij
c200	format(' mij,m,i1,j1,sij = ',4i8,l4)
c First case where ij contains a state- only horiz or vert connections allowed
	if(sij) then
		if((i1.eq.i.and.m.eq.196).or.		!--
     &	(j1.eq.j.and.m.eq.179)) then        ! |
		   nout=nout+1
		   iout(nout)=i1
		   jout(nout)=j1
		   icon(nout)=-1
		endif
	else		!ij contains a connection
	   OK=.false.
c if ij=hor or vert then next cell found (in same row, col respectively) may be
c a state rather than another connection
	  if(mij.eq.196.and.i1.eq.i.and.(m.ge.1.and.m.le.100)) OK=.true.
	  if(mij.eq.179.and.j1.eq.j.and.(m.ge.1.and.m.le.100)) OK=.true.
c if ij=horizontal then can join onto horizontal, or to any corner, on
c the same level (same row, i1=i)
        if(mij.eq.196.and.i1.eq.i.and.(m.eq.196.or.m.eq.191.or.
     &   m.eq.192.or.m.eq.217.or.m.eq.218)) OK=.true.
c if ij=vertical then can join onto vertical, or to any corner, in
c then same column (j1=j)
        if(mij.eq.179.and.j1.eq.j.and.(m.eq.179.or.m.eq.191.or.
     &   m.eq.192.or.m.eq.217.or.m.eq.218)) OK=.true.
c if ij=top left corner can join to horizontal at i1=i,j1=j+1, or
c to vertical at i1=i+1,j1=j
        if(mij.eq.218.and.i1.eq.i.and.j1.eq.j+1.and.m.eq.196)OK=.true.
        if(mij.eq.218.and.i1.eq.i+1.and.j1.eq.j.and.m.eq.179)OK=.true.
c if ij=bot left corner can join to horizontal at i1=i,j1=j+1, or
c to vertical at i1=i-1,j1=j
        if(mij.eq.192.and.i1.eq.i.and.j1.eq.j+1.and.m.eq.196)OK=.true.
        if(mij.eq.192.and.i1.eq.i-1.and.j1.eq.j.and.m.eq.179)OK=.true.
c if ij=top right corner can join to horizontal at i1=i,j1=j-1, or
c to vertical at i1=i+1,j1=j
        if(mij.eq.191.and.i1.eq.i.and.j1.eq.j-1.and.m.eq.196)OK=.true.
        if(mij.eq.191.and.i1.eq.i+1.and.j1.eq.j.and.m.eq.179)OK=.true.
c if ij=bot right corner can join to horizontal at i1=i,j1=j-1, or
c to vertical at i1=i-1,j1=j
        if(mij.eq.217.and.i1.eq.i.and.j1.eq.j-1.and.m.eq.196)OK=.true.
        if(mij.eq.217.and.i1.eq.i-1.and.j1.eq.j.and.m.eq.179)OK=.true.
c
	  if(OK) then
		   nout=nout+1
		   iout(nout)=i1
		   jout(nout)=j1
		   if(m.ge.1.and.m.le.100) then
			icon(nout)=m	!state 1-100
		   else
			icon(nout)=-1	!connexion
		   endif
		endif
	endif
1	continue
	RETURN
	end



	integer function IVAL(charmod,i,j)
c Returns ascii value for symbol in charmod(i,j)
c (1)If both elements blank (or contain invalid connection) then returns 32
c (2) If it is a state then returns 1-100=state number
c (3)If it is a connection then returns 196=horiz; 179=vert; 218=top left
c corner; 192=bottom left; 191=top right; 217=bottom right (extended ASCII
c codes- see SETCHAR routine in MODWIND)
cc (ie either or both elements contain
cc a connection symbol) then returns 45=-, 47=/, 92=\, or 124=|
c Returns IVAL=-1 if invalid symbol found
	character*2 charmod(25,40)
	character*1 c1,c2
c
	IVAL=32		!unless changed below
	c1=charmod(i,j)(1:1)
	c2=charmod(i,j)(2:2)
	if(ichar(c1).eq.32.and.ichar(c2).eq.32) RETURN
c
	if(c1.eq.'c'.or.c1.eq.'C'.or.c1.eq.'o'.or.c1.eq.'O') then
	   IVAL=ichar(c2)-48       !state number=0,1,2,...,9
	   if(IVAL.eq.0) IVAL=10
	   RETURN
	endif
c for cells that contain 2 digit numbers
	if((ichar(c1).ge.48.and.ichar(c1).le.57).and.
     &	(ichar(c2).ge.48.and.ichar(c2).le.57)) then
	   call CHTOREAL(charmod(i,j)(1:2),real)
	   IVAL=ifixr(real)
	   RETURN
	endif
c
	IVAL=ichar(c2)
	if(ival.eq.196.or.ival.eq.179.or.ival.eq.218.or.ival.eq.191.or.
     & ival.eq.192.or.ival.eq.217) RETURN
c
	IVAL=-1
	RETURN
	end

