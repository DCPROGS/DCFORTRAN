
/*========================================================================

											      C FUNCTIONS
														18.05.1993
									Modified: 18.12.1993
	C module necessarly to interface with Tim B. module USE1401.C


========================================================================*/

#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <string.h>
#include "use1401.h"
#include "iclib.h"

/*-----------------------------------------------------------------*/

void  Open1401(short *sErr)
{
    *sErr = U14Open1401();
}

/*-----------------------------------------------------------------*/
void  Close1401(short *sErr)
{
    *sErr = U14Close1401();
}

/*-----------------------------------------------------------------*/
void Reset1401(short *sErr)
{
	*sErr = U14Reset1401();
}

/*-----------------------------------------------------------------*/


void  LastErrCode(short *sErr)
{
    *sErr = U14LastErrCode();
}
/*-----------------------------------------------------------------*/
void Ld(char *vl, char *str, short *sCmd, short *sErr)
{
		unsigned long lRetValue;

		lRetValue=U14Ld((LPSTR)vl,(LPSTR)str);
		*sErr=(short)(lRetValue & 0xFFFF);
		*sCmd=(short)(lRetValue>>16);
}
/*-----------------------------------------------------------------*/
void SendStr(char *lpString,short *sErr)
{
   	*sErr=U14SendString((LPSTR)lpString);
}

/*-----------------------------------------------------------------*/
void GetStr(char *lpString[], long *MaxLen,short *sErr)
{
		WORD wMaxLen;

		wMaxLen=*MaxLen;
		*sErr=U14GetString(*lpString,wMaxLen);
		/*puts(*lpString); */
}

/*-----------------------------------------------------------------*/
void To1401(short lpHost[],long *Size,long *Addr1401,short *Sz,short *sErr)
{
		DWORD	dwSize;
		long	lAddr1401;
		short	eSz;

    dwSize=*Size;
	 	lAddr1401=*Addr1401;
	 	eSz=*Sz;
		*sErr=U14To1401((LPSTR)lpHost,dwSize,lAddr1401,eSz);
}
/*-----------------------------------------------------------------*/

void ToHost(short lpHost[],long *Size,long *Addr1401,short *Sz,short *sErr)
{
		DWORD	dwSize;
		long	lAddr1401;
		short	eSz;

	 	dwSize=*Size;
	 	lAddr1401=*Addr1401;
	 	eSz=*Sz;
		*sErr=U14ToHost((LPSTR)lpHost,dwSize,lAddr1401,eSz);
}

/*-----------------------------------------------------------------*/
void BaseAddr1401(long *addr)
{
	*addr = U14BaseAddr1401();
}

/*-----------------------------------------------------------------*/

void LongsFrom1401(long lpalBuff[], short *MaxLongs,short *sErr)
{
		short 			sMaxLongs;

		sMaxLongs=*MaxLongs;
		*sErr=U14LongsFrom1401(lpalBuff,sMaxLongs);
}
/*-----------------------------------------------------------------*/

void  TypeOf1401(short *type)
{
	*type=U14TypeOf1401();
}

/*-----------------------------------------------------------------*/

void  Stat1401(short *status)
{
	*status=U14Stat1401();
}
/*-----------------------------------------------------------------*/
void LdCmd(char *command, short *sErr)
{

		*sErr=U14LdCmd((LPSTR)command);
}

/*-----------------------------------------------------------------*/



void  LineCount(short *count)
{
    *count = U14LineCount();
}

/*-----------------------------------------------------------------*/

void  CharCount(short *count)
{
    *count = U14CharCount();
}
/*-----------------------------------------------------------------*/
void SendChar(char cChar,short *sErr)
{
	 *sErr=U14SendChar(cChar);
}
/*-----------------------------------------------------------------*/
void GetChar(char *lpcChar,short *sErr)
{
	 *sErr=U14GetChar((LPSTR)lpcChar);
}
/*-----------------------------------------------------------------*/
void SetTimeout(long *time)
{
	long lTimeout;

	lTimeout=*time;
	U14SetTimeout(lTimeout);
}
/*-----------------------------------------------------------------*/
void GetTimeout(long *time)
{
	*time = U14GetTimeout();
}

/*-----------------------------------------------------------------*/
void DriverVersion(long *version)
{
	*version = U14DriverVersion();
}

/*-----------------------------------------------------------------*/

void KillIO1401(short *sErr)
{
	*sErr = U14KillIO1401();
}

/*-----------------------------------------------------------------*/
void ZeroBlockCount(short *code)
{
	*code = U14ZeroBlockCount();
}

/*-----------------------------------------------------------------*/
void BlkTransState(short *sErr)
{
	*sErr = U14BlkTransState();
}

/*-----------------------------------------------------------------*/
void StopCircular(short *sErr)
{
	*sErr = U14StopCircular();
}

/*-----------------------------------------------------------------*/
void StateOf1401(short *sErr)
{
	*sErr = U14StateOf1401();
}

/*-----------------------------------------------------------------*/

void GetUserMemorySize(long *lpMemorySize,short *sErr)
{
	*sErr=U14GetUserMemorySize(lpMemorySize);

}

/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/

/*void GetUserMemory(short *MemorySize)
{
	*MemorySize=U14GetUserMemorySize(long FAR *lpMemorySize);

}
*/
/*-----------------------------------------------------------------*/

void ByteCount(long *lpHowMany,short *sErr)
{
	*sErr=U14ByteCount(lpHowMany);
}

/*-----------------------------------------------------------------*/
void StrToLongs(char *lpszBuff,long lpalNums[],short *MaxLongs,short *sErr)
{
	short sMaxLongs;

	sMaxLongs=*MaxLongs;
	*sErr=U14StrToLongs((LPSTR)lpszBuff,lpalNums,sMaxLongs);
}

/*-----------------------------------------------------------------*/
void OutBufSpace(short *nr)
{
	*nr = U14OutBufSpace();
}

/*-----------------------------------------------------------------*/
void SetTransfer(int *AreaNum,short acBuff[],long *Length,short *Size,short *sErr)
{
	unsigned short	nAreaNum;
	TRANSFERDESC lpTransDesc;

	nAreaNum=*AreaNum;
	lpTransDesc.wAreaNum=nAreaNum;
	lpTransDesc.lpvBuff=(void FAR *)&acBuff;
	lpTransDesc. dwLength=*Length;
	lpTransDesc.eSize=*Size;

	*sErr=U14SetTransfer((LPTRANSFERDESC)&lpTransDesc);

}
/*-----------------------------------------------------------------*/
 void UnSetTransfer(long *AreaNum, short *sErr)
{
	WORD wAreaNum;
	wAreaNum=*AreaNum;

	*sErr=U14UnSetTransfer(wAreaNum);
}

/*-----------------------------------------------------------------*/
/*end*/


