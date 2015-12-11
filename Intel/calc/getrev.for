	subroutine GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini)
c To alter, if req, the state in each cycle that is calculated by
c micro ref (see also GETCYC, in QDEF)
c
c Modif 11/22/01 09:43am so ncyc,nsc,im,jm are parameters, rather than
c in common/mpar, so can be called either with values from prog.ini or values
c from qmechs.dat.  Now sets obeymr(i)=true if cycle #i is constrained by
c MR, and moves the constrained rate to first postion in im(),jm(). If no
c MR constraint then obeymr(i)=false and IM(), JM() returned unchanged
c (im1 etc are used only for defaults)
c im() etc are values for QMECHS.DAT; im1() etc are values
c from prog.ini
c
c Modif 11/16/01 09:41am so that if im(L,1)=0 then microscopic reversibility
c check is omitted for cycle #L -so this way can have some cycles that
c do not obey MR and some that do.
c
	integer icyc(50)
	character*1 ans
	logical discprt
	integer nsc(50),im(50,100),jm(50,100)
	integer nsc1(50),im1(50,100),jm1(50,100)
c=	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	common/dp/discprt
	logical obeymr(50),allmr,useini
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c

	ndim=50	!for call to ivecrev
	allmr=.false.	!local
	print 1,ncyc
1	format(/,' Number of cycles in mechanism [',i2,'] = ')
	call INPUTi(ncyc)
	if(ncyc.eq.0) RETURN
	ans='Y'
	call DCASK('Should ALL cycles be constrained by microscopic rev',
     &	ans,ans)
	allmr=ans.eq.'Y'
	do i=1,ncyc
17	   print 123,i,nsc(i)
123	   format(' Cycle #',i3,/,
     &     ' Number of states [',i2,'] = ')
	   call INPUTi(nsc(i))
	   do j=1,nsc(i)
		icyc(j)=im(i,j)	!states for current cycle
	   enddo
	   print 13,(icyc(j),j=1,nsc(i))
13	   format('   states: ',20i3)
	   if(useini) then
		i1=im1(i,1)		!default
		j1=jm1(i,1)
	   else
		i1=im(i,1)		!default
		j1=jm(i,1)
	   endif
	   if(allmr) then
		obeymr(i)=.true.
	   else
		if(obeymr(i)) then	!set default
		   print 65,i
65		   format(
     &	' Should cycle #',i2,' be constrained by micro rev [Y] ? ')
		   ans='Y'
		else
		   print 66,i
66		   format(
     &	' Should cycle #',i2,' be constrained by micro rev [N] ? ')
		   ans='N'
		endif
		call INPUTa(ans)
		obeymr(i)=ans.eq.'Y'
	   endif
	   if(obeymr(i)) then
		print 67,i1,j1
67		FORMAT(
     &'  Define the route, q(i,j), to be calculated by micro rev:',/,
     &'   Enter i,j [',i2,','i2,'] = ')
		call INPUT2i(i1,j1)
	   else
		goto 9	!go to next cycle
	   endif
c must now get i,j to the 1st two places in the cycle, so look through
c current ICYC for states i,j
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c also check if they occur in reverse order
	   call IVECREV(icyc,nsc(i),ndim)
	   do m1=1,nsc(i)-1
		m2=m1		!for skip-out
		if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
	   enddo
	   if(icyc(nsc(i)).eq.i1.and.icyc(1).eq.j1) goto 151
c if reach here the specified i,j are not found in the cycle
	   goto 17
15	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1-m2,ndim)
	   goto 18
151	   continue
c now bring state in element m2 of icyc into element #1
	   call IVECROT(icyc,nsc(i),1,ndim)
c and redefine im,jm in correct order
18	   continue
	   do m=1,nsc(i)-1
		im(i,m)=icyc(m)
		jm(i,m)=icyc(m+1)
	   enddo
c and the last im,jm
	   im(i,nsc(i))=icyc(nsc(i))
	   jm(i,nsc(i))=icyc(1)		!complete the cycle
9	   continue			!skip here if no m.r. constraint for current cycle
	enddo		!next cycle
c
c Print the values
	do L=1,ncyc
	   print 126, L
	   if(discprt) write(7,126)L
126	   format(' Cycle # ',i3)
	   if(obeymr(L)) then
		print 1311,im(L,1),jm(L,1)
		if(discprt) write(7,1311)im(L,1),jm(L,1)
1311		format(2i3,'  (calc by micro rev)')
	   else
		print 1312,im(L,1),jm(L,1)
		if(discprt) write(7,1312)im(L,1),jm(L,1)
1312		format(2i3,'  (no micro rev constraint)')
	   endif
	   print 127,(im(L,m),jm(L,m),m=2,nsc(L))
127	   format(2(5(2i3,4x),/))
	   if(discprt) write(7,127)(im(L,m),jm(L,m),m=2,nsc(L))
	enddo
c
	RETURN
	end

