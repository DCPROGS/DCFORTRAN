
C
C	FORTRAN PROCEDURES EQUIVALENT TO 1401 C FUNCTIONS
C     18.05.1993
c	Modified 18.12.1993

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
	subroutine ForReset1401(sErr)
C==================================================================

	integer*2 sErr

	call Reset1401(sErr)

	return
	end



C==================================================================
  	subroutine ForLastErrCode(sErr)
C==================================================================
	integer*2 	sErr
    	call LastErrCode(sErr)
	end

C==================================================================
	subroutine ForLd(vl,str,sCmd,sErr)
C==================================================================
	integer*2 		sErr,sCmd
	character*(*) 	vl
	character*(*)	str

	call Ld(carg(vl),carg(str),sCmd,sErr)
	end


C==================================================================
	subroutine ForSendString(lpString,sErr)
C==================================================================
	integer*2      sErr
	character*(*)  lpString

	call SendStr(carg(lpString),sErr)
	end


C==================================================================
	subroutine ForGetString(wString,MaxLen,sErr)
C==================================================================
	allocatable	:: lpString
	integer*2   	sErr,MaxLen
	character*1	      wString(0:MaxLen)
      character*1	   	lpString(:)
	allocate (lpString(0:MaxLen))
	do i=0,MaxLen
		lpString(i)=wString(i)
	end do
	call GetStr(lpString,MaxLen,sErr)
	do i=0,MaxLen
		wString(i)=lpString(i)
	end do
	deallocate(lpString)
	end
C==================================================================
	subroutine  ForTo1401(lpHost,Size,Addr1401,eSz,sErr)
C==================================================================


	integer*4   Size,Addr1401
	integer*2 	eSz,sErr
 	integer*2	lpHost(0:Size)
	call To1401(lpHost,Size,Addr1401,eSz,sErr)
	end

C==================================================================
	subroutine  ForToHost(lpHost,Size,Addr1401,eSz,sErr)
C==================================================================


	integer*4 	Size,Addr1401
	integer*2 	eSz,sErr
	integer*2	lpHost(0:Size)

	call ToHost(lpHost,Size,Addr1401,eSz,sErr)
	end

C==================================================================
	subroutine ForBaseAddr1401(addr)
C==================================================================

	integer*4 addr

	call BaseAddr1401(addr)

	return
	end



C==================================================================
	subroutine ForLongsFrom1401(lpalBuff,MaxLongs,sErr)
C==================================================================
	integer*2 sErr,MaxLongs
	integer*4 lpalBuff(0:MaxLongs)

	call LongsFrom1401(lpalBuff,MaxLongs,sErr)
	end

C==================================================================
	subroutine ForTypeOf1401(type)
C==================================================================

	integer*2 	type

	call TypeOf1401(type)

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
	subroutine ForLdCmd(command,sErr)
C==================================================================

	integer*2       sErr
	character*(*)   command

	call LdCmd(carg(command),sErr)

	return
	end


C==================================================================
  	subroutine ForLineCount(count)
C==================================================================
      integer*2	count
	call LineCount(count)
	end



C==================================================================
	subroutine ForCharCount(count)
C==================================================================
     	integer*2	count
	call CharCount(count)
	end


C==================================================================
	subroutine ForSendChar(sChar,sErr)
C==================================================================
	integer*2 sErr
	character*1 sChar
	integer*1   cChar
	cChar=ichar(sChar)
	call SendChar(carg(cChar),sErr)
	end

C==================================================================
	subroutine ForGetChar(gChar,sErr)
C==================================================================
	integer*2 	    sErr
	character*1	    gChar
	integer*1       lpcChar

	call GetChar(lpcChar,sErr)
	gChar=char(lpcChar)
	end


C==================================================================
	subroutine ForSetTimeout(time)
C==================================================================

	integer*4 time

	call SetTimeout(time)

	return
	end

C==================================================================
	subroutine ForGetTimeout(time)
C==================================================================

	integer*4 time

	call GetTimeout(time)

	return
	end



C==================================================================
	subroutine ForDriverVersion(version)
C==================================================================

	integer*4 version

	call DriverVersion(version)

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
	subroutine ForZeroBlockCount(code)
C==================================================================

	integer*2 code

	call ZeroBlockCount(code)

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
	subroutine ForStopCircular(result)
C==================================================================

	integer*2  result

	call StopCircular(result)

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
	subroutine ForGetUserMemorySize(lpMemorySize,sErr)
C==================================================================
	integer*2 sErr
	integer*4 lpMemorySize

	call GetUserMemorySize(lpMemorySize,sErr)
	end


C==================================================================
	subroutine ForByteCount(lpHowMany,sErr)
C==================================================================
	integer*2 sErr
	integer*4 lpHowMany

	call ByteCount(lpHowMany,sErr)
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
	subroutine ForOutBufSpace(nr)
C==================================================================

	integer*2 nr

	call OutBufSpace(nr)

	return
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
	subroutine ForUnSetTransfer(AreaNum,sErr)
C==================================================================
	integer*2 sErr
	integer*4 AreaNum

	call UnSetTransfer(AreaNum,sErr)
	end

C********************************************************************
