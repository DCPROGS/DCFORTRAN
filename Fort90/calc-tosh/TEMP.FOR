	subroutine GETCYC(iopt)
	integer ICYC(50)
	integer NSC(50),IM(50,100),JM(50,100)
      character*1 ans,UC
c for eqoccd, getqd
	logical allmr	!local
	logical obeymr(50)
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
	COMMON/MPAR/NCYC,NSC,IM,JM
c iopt=0 Prints input values, and asks if change wanted
c iopt=1 prints input values only
c iopt=2 asks for new values only
c iopt=3 asks for rates to be calc by micro rev only
c Modified 01/02/03 04:11pm so that input ncyc value used as default
c when iopt=2
c
	ans='N'
c
	if(iopt.eq.2) goto 5
	if(iopt.eq.3) goto 6
132	if(ncyc.eq.0) then
	   print 1
1	   format(' No cycles in the model',/)
	else
	   do L=1,ncyc
		print 126,L
126		format(' Cycle # ',i3)
		if(obeymr(L)) then
		   print 1311,im(L,1),jm(L,1)
1311		   format(2i3,'  (calc by micro rev)')
		else
		   print 1312,im(L,1),jm(L,1)
1312		   format(2i3,'  (no micro rev constraint)')
		endif
		print 127,(IM(L,M),JM(L,M),m=2,nsc(L))
127		format(2(5(2i3,4x),/))
	   enddo
	endif
	if(iopt.eq.1) RETURN
c
	print 2
2	format(' Are these all O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
5     if(UC(ans).eq.'N') then
	   print 21
21	   format(
     & ' Alter ONLY the rates to be calc by microscopic rev. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
         if(UC(ans).eq.'Y') goto 6
	   print 61,ncyc
61	   FORMAT( ' No of cycles (up to 50) [',i2,'] = ')
	   call INPUTi(L)
	   if(L.lt.0) goto 66
	   ncyc=L
	   IF(NCYC.EQ.0) GOTO 66
	   DO 62 L=1,NCYC
	      print 63,L
63	      FORMAT( ' Number of states in cycle #',I2,' = ')
	      call INPUTi(nsc(L))
		print 67,L,icyc(1),icyc(2)
c67		FORMAT(' Cycle ',i2,': ',/,
c     &'  Define the route, q(i,j), to be calculated by micro rev:',/,
c     &'   Enter i,j [',i2,','i2,'] = ')
	      call INPUT2i(icyc(1),icyc(2))
		print 65,L
65	      FORMAT(
     &  ' List the rest of the states in cycle',i2,' in correct order:')
	      DO M=3,NSC(L)
		   print 651,M
651		   FORMAT( ' state #',I2,' = ')  !EG, 1,3,4,7 IN CORRECT ORDER
		   call INPUTi(icyc(m))
		enddo
C SET IM,JM. IM(L,1)=I,JM(L,1)=J for Q(i,j) to be calc by micro-rev
c in the Lth cycle. The rest of the q(i,j) in the cycle, going round
c in the same direction, have i,j in the other IM,JM
		DO M=1,NSC(L)-1
		   IM(L,M)=ICYC(M)
		   JM(L,M)=ICYC(M+1)
		enddo
C LAST IM,JM
		IM(L,NSC(L))=ICYC(NSC(L))
		JM(L,NSC(L))=ICYC(1)		!COMPLETE THE CYCLE
62	   CONTINUE	!REPEAT FOR NEXT CYCLE
	endif
c
66	continue
	RETURN
c
c Special section for iopt=3, to get only the route to be calc
c by micro rev, for each cycle (when cycles have already been defined
c in CHARQ- values already in im,jm
6	continue
	if(ncyc.eq.0) RETURN
	ans='Y'
	call DCASK('Should ALL cycles be constrained by microscopic rev',
     &	ans,ans)
	allmr=ans.eq.'Y'
	do i=1,ncyc
17	   print 123,i
123	   format(' Cycle #',i3)
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!states for current cycle
	   enddo
	   print 13,(icyc(j),j=1,nsc(i))
13	   format('   states: ',50i3)
	   if(allmr) then
		obeymr(i)=.true.
	   else
		print 661,i
661		format(
     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		obeymr(i)=ans.eq.'Y'
	   endif
	   if(obeymr(i)) then
		print 67,i,i1,j1
67		FORMAT(' Cycle ',i2,': ',/,
     &'  Define the route, q(i,j), to be calculated by micro rev:',/,
     &'   Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
	   else
		goto 9	!go to next cycle
	   endif
c	   print 67
c67	   FORMAT( '  route, q(i,j), to be calc by micro rev: i,j= ')
c	   call INPUT2i(i1,j1)
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	   call IVECREV(icyc,nsc(i),20)
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c if reach here the specified i,j are not found in the cycle
	   goto 17
15	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1-m2,20)
	   goto 18
151	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1,20)
c and redefine im,jm in correct order
18	   continue
	   do m=1,nsc(i)-1
		im(i,m)=icyc(m)
		jm(i,m)=icyc(m+1)
	   enddo
c and the last im,jm
	   im(i,nsc(i))=icyc(nsc(i))
	   jm(i,nsc(i))=icyc(1)		!complete the cycle
c
9	   continue
	enddo		!next cycle
c
	RETURN	!from GETCYC
	end
