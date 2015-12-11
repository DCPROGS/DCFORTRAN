c Set of subroutines to add default values into query string used for QDIALOG
c Modif 02/17/94 06:34pm so if DEFOLTa called with no string (length=0) just
c adds ' ? ' 
c
c
c	subroutine DEFOLTi(n1,deftext)
c	subroutine DEFOLT2i(n1,n2,deftext)
c	subroutine DEFOLTr(real1,deftext)
c	subroutine DEFOLT2r(real1,real2,deftext)
c	subroutine DEFOLTa(ch,deftext)
c
	subroutine DEFOLTi(int1,deftext)
	character deftext*(*),cnum*11
	call INTCONV(int1,cnum)
	n=NBLANK(cnum)
	deftext=' ['//cnum(1:n)//'] = '
	RETURN
	end


	subroutine DEFOLT2i(int1,int2,deftext)
	character deftext*(*),cnum1*11,cnum2*11
	call INTCONV(int1,cnum1)
	call INTCONV(int2,cnum2)
	n1=NBLANK(cnum1)
	n2=NBLANK(cnum2)
	deftext=' ['//cnum1(1:n1)//','//cnum2(1:n2)//'] = '
	RETURN
	end


	subroutine DEFOLTr(real1,deftext)
	character deftext*(*),cnum*11
	call REALTOCH(real1,cnum,11)
	n=NBLANK(cnum)
	deftext=' ['//cnum(1:n)//'] = '
	RETURN
	end


	subroutine DEFOLT2r(real1,real2,deftext)
	character deftext*(*),cnum1*11,cnum2*11
	call REALTOCH(real1,cnum1,11)
	call REALTOCH(real2,cnum2,11)
	n1=NBLANK(cnum1)
	n2=NBLANK(cnum2)
	deftext=' ['//cnum1(1:n1)//','//cnum2(1:n2)//'] = '
	RETURN
	end

	subroutine DEFOLTa(ch,deftext)
c eg to insert 'Y','N' as defaults
	character deftext*(*),ch*(*)
	n=NBLANK(ch)
	if(n.ge.1) then
	   deftext=' ['//ch(1:n)//'] ? '
	else
	   deftext=' ? '
	endif
	RETURN
	end

