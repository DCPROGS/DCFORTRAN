C     Last change:  P     7 May 99   12:07 pm
	program testdig
	character    vl*10,str*10
      character*25 str1,digstr(100)
	integer*2    hand,sErr,n1401,sCmd,MaxLongs
	integer*4    lRetValue
	integer*4    lpalBuff(0:4)

      n1401 = 0
	vl='C:\1401\'
	str='DIGTIM'
	call ForOpen1401(hand)
	call ForLd('c:\1401\','KILL,DIGTIM,TIMER2',hand,serr)
	call ForTypeOf1401(itype)
	if(itype.eq.0) then
	   print*,'1401 STANDARD '
	else if(itype.eq.1) then
	   print*,'1401 PLUS '
	else
	   print*,'1401 UNKNOWN '
	endif
	call ForGetUserMemorySize(lpMemorySize,iErr)
	print*,'1401 user memory size = ',lpMemorySize
	str1='DIGTIM,O,1792;'
	nm=nblank1(str1)
      call ForSendString(str1(1:nm),sERR)
	str1='DIGTIM,SI,2048,560;'
	nm=nblank1(str1)
      call ForSendString(str1(1:nm),sERR)

	 digstr(1) = 'DIGTIM,A,4,0,20;'
	 digstr(2) = 'DIGTIM,A,20,4,20;'
	 digstr( 3) = 'DIGTIM,A,16,16,10;'
	 digstr( 4) = 'DIGTIM,A,2,0,40;'
	do i=1,4
	   nm=nblank1(str1)
         call ForSendString(digstr(i),sERR)
	enddo

	str1='DIGTIM,OD;'
	nm=nblank1(str1)
      call ForSendString(str1(1:nm),sERR)
	str1='DIGTIM,C,100,1000;'
	nm=nblank1(str1)
      call ForSendString(str1(1:nm),sERR)

21	str1='DIGTIM,?;'
	nm=nblank1(str1)
      call ForSendString(str1(1:nm),sERR)
	MaxLongs=4
	call ForLongsFrom1401(lpalbuff,4,serr)
	istatus=lPALBUFF(0)
	do k=1,100000		!insert slight pause to make sure istatus read
	   s=123456.789/float(k)
	enddo
	print*,serr,istatus,lpalbuff(1),lpalbuff(2)
	if(istatus.ne.0) goto 21
	CALL ForClose1401()
	END


