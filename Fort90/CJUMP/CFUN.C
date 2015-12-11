
/*========================================================================

											      C FUNCTIONS
														18.05.1993
	C module necessarly to interface with Tim B. module USE1401.C


========================================================================*/

#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <string.h>
#include "use1401.h"
#include "cfun.h"

/*-----------------------------------------------------------------*/

short  Open1401(short *sErr)
{
    *sErr = U14Open1401();
		return *sErr;
}

/*-----------------------------------------------------------------*/
void  Close1401(short *sErr)
{
    *sErr = U14Close1401();
}

/*-----------------------------------------------------------------*/


void  LastErrCode(short *sErr)
{
    *sErr = U14LastErrCode();
}
/*-----------------------------------------------------------------*/

void  Stat1401(short *status)
{
	*status=U14Stat1401();
}
/*-----------------------------------------------------------------*/

void  TypeOf1401(short *type)
{
	*type=U14TypeOf1401();
}

/*-----------------------------------------------------------------*/



void  CharCount(short *count)
{
    *count = U14CharCount();
}
/*-----------------------------------------------------------------*/

void  LineCount(short *count)
{
    *count = U14LineCount();
}

/*-----------------------------------------------------------------*/
void Reset1401(short *result)
{
	*result = U14Reset1401();
}

/*-----------------------------------------------------------------*/
void OutBufSpace(short *sErr)
{
	*sErr = U14OutBufSpace();
}

/*-----------------------------------------------------------------*/
void StateOf1401(short *sErr)
{
	*sErr = U14StateOf1401();
}

/*-----------------------------------------------------------------*/
void KillIO1401(short *sErr)
{
	*sErr = U14KillIO1401();
}

/*-----------------------------------------------------------------*/
void ZeroBlockCount(short *result)
{
	*result = U14ZeroBlockCount();
}

/*-----------------------------------------------------------------*/
void BlkTransState(short *sErr)
{
	*sErr = U14BlkTransState();
}

/*-----------------------------------------------------------------*/
void StopCircular(short *result)
{
	*result = U14StopCircular();
}

/*-----------------------------------------------------------------*/
void DriverVersion(long *lErr)
{
	*lErr = U14DriverVersion();
}

/*-----------------------------------------------------------------*/
void GetTimeout(long *period)
{
	*period = U14GetTimeout();
}

/*-----------------------------------------------------------------*/
void BaseAddr1401(long *lErr)
{
	*lErr = U14BaseAddr1401();
}

/*-----------------------------------------------------------------*/

void GetUserMemorySize(long *lpMemorySize,short *sErr)
{
	*sErr=U14GetUserMemorySize(lpMemorySize);
}

/*-----------------------------------------------------------------*/

void ByteCount(long *lpHowMany,short *sErr)
{
	*sErr=U14ByteCount(lpHowMany);
}

/*-----------------------------------------------------------------*/
 void UnSetTransfer(long *AreaNum, short *sErr)
{
	WORD wAreaNum;
	wAreaNum=*AreaNum;

	*sErr=U14UnSetTransfer(wAreaNum);
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
void SetTimeout(long *Timeout)
{
	long lTimeout;
	lTimeout=*Timeout;
	U14SetTimeout(lTimeout);
}
/*-----------------------------------------------------------------*/


void To1401(short Host[],long *Size,long *Addr1401,short *Sz,short *sErr)
{
		DWORD	dwSize;
		long	lAddr1401;
		short	eSz;
		int i;
    dwSize=*Size;
	 	lAddr1401=*Addr1401;
	 	eSz=*Sz;

		/*printf("\n");
	  printf("TEST To1401\n");
	 	printf(" %d %d %d %d\n",lpHost[1],dwSize,lAddr1401,eSz);*/

	  /*for(i=0;i<dwSize;i++)
				{
				 	lpHost[i]=Host[i];
					printf(" %d %d\n ",lpHost[i],Host[i]);
				} */
		*sErr=U14To1401((LPSTR)Host,dwSize,lAddr1401,eSz);

		/*for(i=1;i<5;i++)
	 		printf(" %d %d\n ",lpHost[i],Host[i]); */
}
/*-----------------------------------------------------------------*/
void To14(char *lpHost[],long *Size,long *Addr1401,short *Sz,short *sErr)
{
		DWORD	dwSize;
		long	lAddr1401;
		short	eSz;
  	dwSize=*Size;
	 	lAddr1401=*Addr1401;
	 	eSz=*Sz;
		*sErr=U14To1401(*lpHost,dwSize,lAddr1401,eSz);
}

/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/

void ToHost(short Host[],long *Size,long *Addr1401,short *Sz,short *sErr)
{
		DWORD	dwSize;
		long	lAddr1401;
		short	eSz;
		int i;
	 	dwSize=*Size;
	 	lAddr1401=*Addr1401;
	 	eSz=*Sz;

		/*printf("\n");
		printf("TEST ToHost\n");
	 	printf(" %d %d %d %d\n",lpHost[1],dwSize,lAddr1401,eSz);
		for(i=1;i<5;i++)
	 		printf(" %d %d\n ",lpHost[i],Host[i]); */

		*sErr=U14ToHost((LPSTR)Host,dwSize,lAddr1401,eSz);
		/*sErr=U14ToHost((LPSTR)lpHost,dwSize,lAddr1401,eSz);
	  	for(i=0;i<dwSize;i++)
					Host[i]=lpHost[i];

			for(i=1;i<5;i++)
	 		printf(" %d %d\n ",lpHost[i],Host[i]);
			*/
}

/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/

void SendString(char *wString[],int *ilen,short *sErr)
{
	  /*int i;
		int len=*ilen;
		char	*lpString[35];

		nou = (char *) calloc (len , sizeof(nou));
		nou=*wString;
		char	*nou;
		nou=*wString;
		nou = (char *) malloc(len);
		*strncpy(*lpString,*wString,len);
		for(i=len+1;i<40;i++)
				*lpString[i]=' ';
	  printf("%d %d %d\n",strlen(*wString),strlen(*lpString),strlen(nou));
		puts(*lpString);
	 	printf("\n");
		printf("%d ",len);
		puts(*lpString);

		printf("\n");
		puts(*wString); */
    *sErr=U14SendString(*wString);
}

/*-----------------------------------------------------------------*/
void SendStr(char *wString,int *ilen,short *sErr)
{

		int i;
		char *stcopy;
		F_strcpy(stcopy,wString);
		/*int len=*ilen;
		printf("DRAC\n");
		printf("%s\n",*wString);
    sErr=U14SendString((LPSTR)wString);*/


/*		printf("\n");
		printf("%s\n",wString);
		printf("\n");
		printf("%s\n",stcopy); */
   	*sErr=U14SendString((LPSTR)wString);
		/*ret="what a mess";
		puts(*ret);
		ret[0]='w';
		ret[1]='h';*/
}

/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/

void GetStr1(short String[], long *MaxLen,short *sErr)
{
		WORD wMaxLen;
		int	i;
		wMaxLen=*MaxLen;
		*sErr=U14GetString((LPSTR)String,wMaxLen);
		/*for(i=0;i<wMaxLen;i++)
						 String[i]=lpString[i]; */

		printf("%s\n",String);
}

/*-----------------------------------------------------------------*/

void GetStr(char *String, long *MaxLen,short *sErr)
{
		WORD wMaxLen;
		int	i;
		wMaxLen=*MaxLen;
		*sErr=U14GetString((LPSTR)String,wMaxLen);
		printf("%s\n",String);
}

/*-----------------------------------------------------------------*/

void GetString(char *lpString[], long *MaxLen,short *sErr)
{
		WORD wMaxLen;

		wMaxLen=*MaxLen;
		*sErr=U14GetString(*lpString,wMaxLen);
		puts(*lpString);
}

/*-----------------------------------------------------------------*/
/*-----------------------------------------------------------------*/

void Ld(char *vl[], char* str[], short *sCmd, short *sErr)
{
		unsigned long lRetValue;
		/*printf("\n");
		puts(*vl);
		puts(*str);*/

		lRetValue=U14Ld(*vl,*str);
		*sErr=(short)(lRetValue & 0xFFFF);
		*sCmd=(short)(lRetValue>>16);
}

/*-----------------------------------------------------------------*/
void Ld1(char *vl, char *str, short *sCmd, short *sErr)
{
		unsigned long lRetValue;
		/*printf("\n");
		puts(*vl);
		puts(*str);*/

		lRetValue=U14Ld((LPSTR)vl,(LPSTR)str);
		*sErr=(short)(lRetValue & 0xFFFF);
		*sCmd=(short)(lRetValue>>16);
}

/*-----------------------------------------------------------------*/
void LdCmd(char *command, short *sErr)
{

		*sErr=U14LdCmd((LPSTR)command);
}

/*-----------------------------------------------------------------*/

void LongsFrom1401(long lpalBuff[], short *MaxLongs,short *sErr)
{
		short 			sMaxLongs;

		sMaxLongs=*MaxLongs;
		*sErr=U14LongsFrom1401(lpalBuff,sMaxLongs);
}
/*-----------------------------------------------------------------*/
void StrToLongs(char *lpszBuff,long lpalNums[],short *MaxLongs,short *sErr)
{
	short sMaxLongs;
	sMaxLongs=*MaxLongs;
	*sErr=U14StrToLongs((LPSTR)lpszBuff,lpalNums,sMaxLongs);
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
