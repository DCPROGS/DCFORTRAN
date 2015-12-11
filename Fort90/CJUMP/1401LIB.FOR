
C
C	FORTRAN PROCEDURES EQUIVALENT TO 1401 C FUNCTIONS
C     18.05.1993

C==================================================================
	subroutine ForOpen1401(sErr)
C==================================================================
     	hcexternal Open1401
	integer*2	sErr
	call Open1401(sErr)
	end


C==================================================================
	subroutine ForClose1401()
C==================================================================

	call U14Close1401()

	return
	end

C==================================================================
	subroutine ForStat1401(status)
C==================================================================

	integer*2 status

	call Stat1401(status)

	return
	end

C==================================================================
	subroutine ForTypeOf1401(type)
C==================================================================

	integer*2 	type

	call TypeOf1401(type)

	return
	end



C==================================================================
  	subroutine ForLastErrCode(sErr)
C==================================================================
	integer*2 	sErr
    	call LastErrCode(sErr)
	end

C==================================================================
	subroutine ForCharCount(count)
C==================================================================
     	integer*2	count
	call CharCount(count)
	end

C==================================================================
  	subroutine ForLineCount(count)
C==================================================================
      integer*2	count
	call LineCount(count)
	end


C==================================================================
	subroutine ForOutBufSpace(sErr)
C==================================================================

	integer*2 sErr

	call OutBufSpace(sErr)

	return
	end

C==================================================================
	subroutine ForReset1401(sErr)
C==================================================================

	integer*2 sErr

	call Reset1401(sErr)

	return
	end


C==================================================================
	subroutine ForStateOf1401(sErr)
C==================================================================

	integer*2 sErr

	call StateOf1401(sErr)

	return
	end


C==================================================================
	subroutine ForKillIO1401(sErr)
C==================================================================

	integer*2 sErr

	call KillIO1401(sErr)

	return
	end


C==================================================================
	subroutine ForZeroBlockCount(result)
C==================================================================

	integer*2 result

	call ZeroBlockCount(result)

	return
	end



C==================================================================
	subroutine ForStopCircular(result)
C==================================================================

	integer*2  result

	call StopCircular(result)

	return
	end


C==================================================================
	subroutine ForBlkTransState(sErr)
C==================================================================

	integer*2  sErr

	call BlkTransState(sErr)

	return
	end


C==================================================================
	subroutine ForDriverVersion(lErr)
C==================================================================

	integer*4 lErr

	call DriverVersion(lErr)

	return
	end

C==================================================================
	subroutine ForBaseAddr1401(lErr)
C==================================================================

	integer*4 lErr

	call BaseAddr1401(lErr)

	return
	end

C==================================================================
	subroutine ForSetTimeout(time)
C==================================================================

	integer*4 time

	call SetTimeout(time)

	return
	end

C==================================================================
	subroutine ForGetTimeout(period)
C==================================================================

	integer*4 period

	call GetTimeout(period)

	return
	end


C==================================================================
	subroutine  ForTo1401(Host,dwSize,lAddr1401,eSz,sErr)
C==================================================================


	integer*4   dwSize,lAddr1401
	integer*2 	eSz,sErr
 	integer*2	Host(0:dwSize)
	call To1401(Host,dwSize,lAddr1401,eSz,sErr)
	end

C==================================================================
	subroutine  ForToHost(lpHost,dwSize,lAddr1401,eSz,sErr)
C==================================================================


	integer*4 	dwSize,lAddr1401
	integer*2 	eSz,sErr
	integer*2	lpHost(0:dwSize)

	call ToHost(lpHost,dwSize,lAddr1401,eSz,sErr)
	end

C==================================================================
	subroutine  ForSetTransfer(AreaNum,acBuff,Length,Size,sErr)
C==================================================================


	integer*4 	Length
	integer*2 	AreaNum,Size,sErr
	integer*2	acBuff(0:Length)

	call SetTransfer(AreaNum,acBuff,Length,Size,sErr)
	end

C==================================================================
	subroutine  ForStrToLongs(lpszBuff,lPalNums,MaxLongs,sErr)
C==================================================================


	character*(*)  lpszBuff
	integer*2 	MaxLongs,sErr
	integer*2	lPalNums(0:MaxLongs)

	call StrToLongs(carg(lpszBuff),lPalNums,MaxLongs,sErr)
	end


C==================================================================
	subroutine ForLdCmd(command,sErr)
C==================================================================

	integer*2       sErr
	character*(*)   command

	call LdCmd(carg(command),sErr)

	return
	end




C==================================================================
	subroutine ForSendString(lpString,sErr)
C==================================================================
	allocatable	:: wstring
	integer*2      sErr
	character*(*)  lpString
      character*1	   wString(:)

	MaxLen=nblank1(lpString)
	allocate (wString(0:MaxLen))
	do i=0,MaxLen-1
		wString(i)=lpString(i+1:i+1)
	end do
	call SendStr(wString,sErr)
	end


C==================================================================
	subroutine ForGetString(lpString,MaxLen,sErr)
C==================================================================
	allocatable	:: wstring
	integer*2   	sErr,MaxLen
	character*1	      lpString(0:MaxLen)
      character*1	   	wString(:)
	allocate (wString(0:MaxLen))
	do i=0,MaxLen
		wString(i)=lpString(i)
	end do
	call GetStr(wString,MaxLen,sErr)
	do i=0,MaxLen
		lpString(i)=wString(i)
	end do
	deallocate(wString)
	end

C==================================================================
	subroutine ForGetString1(lpString,MaxLen,sErr)
C==================================================================
	allocatable	:: wstring
	integer*2   	sErr,MaxLen
	character*(*)	lpString(MaxLen)
      character*1	   	wString(:)
	allocate (wString(0:MaxLen))
	do i=0,MaxLen
		wString(i)=' '
	end do
	call GetStr(wString,MaxLen,sErr)
	print*,maxlen
	do i=0,6
		print*,wstring(i)
	enddo
	call sepstr(wString,lpString,MaxLen,m)
	deallocate(wString)
	end


C==================================================================
	subroutine ForLd(vl,str,sCmd,sErr)
C==================================================================
	integer*2 		sErr,sCmd
	character*(*) 	vl
	character*(*)	str

	call Ldc(carg(vl),carg(str),sCmd,sErr)
	end


C==================================================================
	subroutine ForLongsFrom1401(lpalBuff,MaxLongs,sErr)
C==================================================================
	integer*2 sErr,MaxLongs
	integer*4 lpalBuff(0:MaxLongs)

	call LongsFrom1401(lpalBuff,MaxLongs,sErr)
	end

C********************************************************************

C==================================================================
	subroutine ForGetUserMemorySize(lpMemorySize,sErr)
C==================================================================
	integer*2 sErr
	integer*4 lpMemorySize

	call GetUserMemorySize(lpMemorySize,sErr)
	end

C********************************************************************

C==================================================================
	subroutine ForByteCount(lpHowMany,sErr)
C==================================================================
	integer*2 sErr
	integer*4 lpHowMany

	call ByteCount(lpHowMany,sErr)
	end

C********************************************************************

C==================================================================
	subroutine ForUnSetTransfer(AreaNum,sErr)
C==================================================================
	integer*2 sErr
	integer*4 AreaNum

	call UnSetTransfer(AreaNum,sErr)
	end

C==================================================================
	subroutine ForSendChar(cChar,sErr)
C==================================================================
	integer*2 sErr
	integer*1 cChar

	call SendChar(carg(cChar),sErr)
	end

C==================================================================
	subroutine ForGetChar(lpcChar,sErr)
C==================================================================
	integer*2 	    sErr
	character*1	    lpcChar

	call GetChar(lpcChar,sErr)
	end

C********************************************************************
