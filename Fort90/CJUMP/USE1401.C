/****************************************************************************/
/*                                                                          */
/*  (C) Copyright Cambridge Electronic Design Ltd, 1992                     */
/*                                                                          */
/*  Title:      USE1401.C                                                   */
/*                                                                          */
/*  Version:    1.0                                                         */
/*                                                                          */
/*  Date:       23/6/92                                                     */
/*                                                                          */
/*  Author:     Paul Cox                                                    */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Change log:                                                             */
/*                                                                          */
/*     DATE     REV                 DESCRIPTION                             */
/*  ----------- --- --------------------------------------------------------*/
/*  21/Jul/92  PNC  First version intended to support Mac and DOS/Windows   */
/*                  calls with same interface.                              */
/*                                                                          */
/****************************************************************************/
/*                                                                          */
/* USE1401  Support the 1401 device driver under Windows and on the Mac     */
/*                                                                          */
/****************************************************************************/

#ifdef MSDOS
#ifndef __HIGHC__
#include <windows.h>
#endif
#endif

#ifdef __HIGHC__
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <types.h>
#include <string.h>
#include <stat.h>
#include <memory.h>
#include <fcntl.h>
#include <io.h>
#include <dos.h> 
#include <bios.h> 
#pragma pack(1)
#include <pharlap.h>
#pragma pack()
#endif

#ifdef macintosh
#include <types.h>
#include <resources.h>
#include <strings.h>
#include <string.h>
#include <Devices.h>
#include <events.h>
#include <Memory.h>
#include <errors.h>

#if qDebug
#include <stdio.h>
#endif

#pragma segment Use1401
#endif

#include "use1401.h"

#define DRIVRET_STD     4
#define DRIVRET_PLUS    6


short       sDriverState;           /* state of the driver              */
char        commandext[4];          /* file extension for 1401 commands */
WORD        wTranslateMode;         /* text translation mode            */
long        lTimeOutPeriod=1;       /* timeout time in 1/60th of a sec  */
short       sLastRetCode=U14ERR_NOERROR;     /* last code from a fn call*/
short       sType1401=U14TYPEUNKNOWN;  /* dont know  yet                */
BOOLEAN     bGrabbed=FALSE;         /* set true by grab1401             */

#pragma pack(1)                     /* pack our structures              */

typedef struct CmdHead              /* defines header block on command      */
{                                   /* for PC commands                      */
   char   acBasic[5];               /* BASIC information                    */
   WORD   wBasicSz;                 /* size as seen by BASIC                */
   WORD   wCmdSize;                 /* size of the following info           */
} CMDHEAD;

/* The rest of the header looks like this......                             */
/*  int    iRelPnt;             relocation pointer... actual start          */
/*  char   acName[8];           string holding the command name             */
/*  BYTE   bMonRev;             monitor revision level                      */
/*  BYTE   bCmdRev;             command revision level                      */

typedef CMDHEAD FAR * LPCMDHEAD;    /* long pointer to a command header     */

#define  MAXSTRLEN   255            /* maximum string length we use         */
#define  TOHOST      FALSE            
#define  TO1401      TRUE


#ifndef __HIGHC__
#ifdef MSDOS
/****************************************************************************/
/*                     Windows Specific Variables                           */
/****************************************************************************/

HANDLE      hModule;                /* handle (instance?) of .DLL       */
HWND        hCallBackWnd = NULL;    /* window to call back to           */
DWORD       V1401D_API = NULL;      /* addr of V1401D API entry point   */
WORD        wCEDMessage;            /* our message identifier           */

#endif
#endif

#ifdef __HIGHC__
USHORT      V1401D_API_S = 0;         /* Selector of V1401D API entry point */
ULONG       V1401D_API_O = 0;           /* offset of V1401D API entry point */
#endif


#ifdef macintosh
/****************************************************************************/
/*                       Mac Specific Variables                             */
/****************************************************************************/

short ref1401;  /* reference number for 1401 driver, for status and control */
short appResFile;           /* reference number for the application */
short cmdResFile;       /* reference number for 1401 command resource files*/

#endif

#ifdef MSDOS
/****************************************************************************/
/*            Windows Specific Variables Private Functions                  */
/****************************************************************************/

typedef struct VXTransferDesc    /* Structure for VXD comms                 */
{
   WORD        wAreaNum;         /* number of transfer area to set up       */
   WORD        wAddrSel;         /* 16 bit selector for area                */
   DWORD       dwAddrOfs;        /* 32 bit offset for area start            */
   DWORD       dwLength;         /* length of area to set up                */
} VXTRANSFERDESC;


U14API(short) U14CallDriver(BYTE bMainFn,BYTE bSubFn,U14PARAM lpParams);
#ifndef __HIGHC__
U14API(BOOL) LibMain(HANDLE hMod,WORD wDSeg,WORD wHeapSz,LPSTR lpszCmdLine);
U14API(BOOL) WEP(int nArgument);
#endif
#endif

#ifdef macintosh
/****************************************************************************/
/*           Macintosh Specific Variables Private Functions                 */
/****************************************************************************/
U14API(OSErr) GetCommand(short sResFile,char * szName,Handle *hReturn);
#endif

#ifdef __HIGHC__
U14API(long) GetTickCount(void)
{
    long    lval;
    _bios_timeofday(_TIME_GETCLOCK, &lval);             /* Get system ticks */
    return (long)((float)(lval*1000)/18.2F);
}
#endif

/****************************************************************************/
/*                                                                          */
/*   WhenToTimeOut                                                          */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Returns the time to time out in time units suitable for the machine*/
/*       we are running on  ie millsecs for pc, 1/60th for Mac              */
/*                                                                          */
/****************************************************************************/
U14API(long) WhenToTimeOut(void)
{
   #ifdef MSDOS
   return(GetTickCount()+lTimeOutPeriod);
   #endif

   #ifdef macintosh
   return(TickCount()+lTimeOutPeriod);
   #endif
}


/****************************************************************************/
/*                                                                          */
/*   PassedTime                                                             */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Returns none zero if the timed passed in has been passed 0 if not  */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) PassedTime(long lCheckTime)
{
   #ifdef MSDOS
   return(GetTickCount()>(DWORD)lCheckTime);
   #endif

   #ifdef macintosh
   return(TickCount()>(DWORD)lCheckTime);
   #endif
}



/****************************************************************************/
/*                                                                          */
/*   TranslateString                                                        */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Converts all the commas in a string to spaces.                     */
/*       May do more in future                                              */
/*                                                                          */
/****************************************************************************/
U14API(void) TranslateString(LPSTR szWork)
{
   while (*szWork)                        /* not at end */
   {
      if (*szWork==',')
         *szWork=' ';
      szWork++;
   }
   
}

/****************************************************************************/
/*                                                                          */
/*   TranslateChar                                                          */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Converts any commas to spaces                                      */
/*       May do more in future                                              */
/*                                                                          */
/****************************************************************************/
U14API(char) TranslateChar(char cTransMe)
{
   return((cTransMe==',')?' ':cTransMe);
}

/****************************************************************************/
/*                                                                          */
/*   U14StrToLongs                                                          */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*   Converts a string to an array of longs and returns the number of       */
/*   values found.                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14StrToLongs(LPSTR lpszBuff,long FAR *lpalNums,short sMaxLongs)
{
   long      lValue;      /* build each value in here */
   WORD      wChInd;      /* index into source        */
   short     sLgInd;      /* index into result longs  */

   wChInd=0;
   sLgInd=0;

   while ((lpszBuff[wChInd])            /* until we get to end of string    */
          && (sLgInd<sMaxLongs))        /* or filled the buffer             */
   {
      switch (lpszBuff[wChInd])
      {
      case '-':
      case '0': case '1':   case '2': case '3':   case '4':
      case '5': case '6':   case '7': case '8':   case '9':
         {
            BOOLEAN  bDone=FALSE;      /* true at end of number */
            int      iSign=1;        /*sign of number*/

            lValue=0;

            while (!bDone)
            {
               switch (lpszBuff[wChInd])
               {
               case '-':
                  iSign=-1;         /*swap sign*/
                  break;

               case '0': case '1':   case '2': case '3':   case '4':
               case '5': case '6':   case '7': case '8':   case '9':
                  lValue=lValue*10;   /*move to next digit base 10*/
                  lValue+=((int)lpszBuff[wChInd]-(int)'0');
                  break;

               default:            /* end of number */
                  bDone=TRUE;
                  break;
               }
            wChInd++;               /* move onto next character */
            }
            lpalNums[sLgInd]=lValue*iSign;
            sLgInd++;
         }
         break;

      default:
         wChInd++;                  /* look at next char */
         break;
      }

   }

   return (sLgInd);
}

/****************************************************************************/
/*                                                                          */
/*    U14LongsFrom1401                                                      */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*    Gets the next waiting line from the 1401 and converts it longs        */
/*                                                                          */
/*    Returns the number of numbers read or an error.                       */
/*                                                                          */
/****************************************************************************/
U14API(short) U14LongsFrom1401(long FAR * lpalBuff,short sMaxLongs)
{
   char        szWork[MAXSTRLEN];
   short       sResult;

   sResult=U14GetString((LPSTR)szWork,MAXSTRLEN);  /* get reply from 1401   */
   if (sResult==U14ERR_NOERROR)                    /* if no error convert   */
      sResult=U14StrToLongs((LPSTR)szWork,lpalBuff,sMaxLongs);

   return(sResult);
}

/****************************************************************************/
/*    U14LastErrCode                                                        */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Returns the last code from the driver                              */
/*                                                                          */
/****************************************************************************/
U14API(short) U14LastErrCode(void)
{
   return(sLastRetCode);                           /* nothing to it */
}

/****************************************************************************/
/*                                                                          */
/*    U14SetTimeout                                                         */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Set the timeout period for 1401 comms in 1/60ths of a second       */
/*                                                                          */
/****************************************************************************/
U14API(void)  U14SetTimeout(long lTimeOut)
{
   #ifdef MSDOS
   lTimeOut=lTimeOut*17;               /* actually deal in 1000ths on pc */
   #endif
   lTimeOutPeriod=lTimeOut;            /* timeout time in 1/60th of a sec   */
}

/****************************************************************************/
/*                                                                          */
/*    U14GetTimeout                                                         */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Get the timeout period for 1401 comms in 1/60ths of a second       */
/*                                                                          */
/****************************************************************************/
U14API(long)  U14GetTimeout(void)
{
   return(lTimeOutPeriod);
}

/****************************************************************************/
/*                                                                          */
/*    U14OutBufSpace                                                        */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14OutBufSpace(void)
{
      TCSBLOCK    csBlock;
      short       sError;

      sError=U14Status1401(U14_GETOUTBUFSPACE,&csBlock);

      if (sError==U14ERR_NOERROR)
         sError=csBlock.ints[0];
      else
         sError=sLastRetCode;

      return(sError);
}

/****************************************************************************/
/*                                                                          */
/*    U14BaseAddr1401                                                       */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Returns the 1401 base address or an error code                     */
/*                                                                          */
/****************************************************************************/
U14API(long)  U14BaseAddr1401(void)
{
      TCSBLOCK    csBlock;
      long        lError;

      lError=U14Status1401(U14_GETBASEADDRESS,&csBlock);

      if (lError==U14ERR_NOERROR)
         lError=csBlock.longs[0];
      else
         lError=sLastRetCode;

      return(lError);
}

/****************************************************************************/
/*                                                                          */
/*    U14StateOf1401                                                        */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14StateOf1401(void)
{
      TCSBLOCK csBlock;
      short    sError;

      sError=U14Status1401(U14_STATEOF1401,&csBlock);

      if (sError==U14ERR_NOERROR)      /* if there was no error, return the */
      {
         sError=csBlock.ints[0];       /* returned 1401 state               */
         
#ifdef MSDOS
         switch(sError)
         {
            case DRIVRET_STD : 
            {
                sError = U14ERR_NOERROR;
                break;
            }
            case DRIVRET_PLUS :
            {
                sError = U14ERR_NOERROR;
                break;
            }
            default : break;
         }
#endif
         
#ifdef macintosh
         switch(sError)
         {
            case 100: sError=U14ERR_OFF; break;
            case 101: sError=U14ERR_NC; break;
            case 102: sError=U14ERR_ILL; break;
            case 103: sError=U14ERR_NOIF; break;
            case 104: sError=U14ERR_TIME; break;
         }
#endif
      }
      else 
         sError=sLastRetCode;

      return(sError);
}

/****************************************************************************/
/*                                                                          */
/*    U14DriverVersion                                                      */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Returns the driver version. Hi word is major revision, low word is */
/*       minor revision                                                     */
/*                                                                          */
/****************************************************************************/
U14API(long) U14DriverVersion(void)
{
   TCSBLOCK    csBlock;
   long        lError;
   long        lRevLev;

   lError=U14Status1401(U14_GETDRIVERREVISION,&csBlock);

   #ifdef MSDOS
   lRevLev=csBlock.longs[0];
   #endif

   #ifdef macintosh
   lRevLev=csBlock.ints[0];
   #endif

   if (lError==U14ERR_NOERROR)
      lError=lRevLev;
   else
      lError=sLastRetCode;

   return(lError);
}

/****************************************************************************/
/*                                                                          */
/*    U14ZeroBlockCount                                                     */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14ZeroBlockCount(void)
{
   TCSBLOCK    csBlock;

   return( U14Control1401(U14_ZEROBLOCKCOUNT,&csBlock) );
}

/****************************************************************************/
/*                                                                          */
/*    U14ByteCount                                                          */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short)  U14ByteCount(long FAR * lpHowMany)
{
   return( U14Status1401(U14_BYTECOUNT,(LPTCSBLOCK)lpHowMany) );
}

/****************************************************************************/
/*                                                                          */
/*    U14BlkTransState                                                      */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Returns 0 for no transfer in progress.                             */
/*       1 for transfer in progress                                         */
/*       or an error code                                                   */
/*                                                                          */
/****************************************************************************/
U14API(short) U14BlkTransState(void)
{
   TCSBLOCK    csBlock;
   short       sErr;

   sErr=U14Status1401(U14_BLKTRANSSTATE,&csBlock);

   if (sErr==U14ERR_NOERROR)
      sErr=csBlock.ints[0];
   else
      sErr=sLastRetCode;

   return(sErr);                    /* return any error */
}

/****************************************************************************/
/*                                                                          */
/*    U14StopCircular                                                       */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Stop any current circular transfer                                 */
/*                                                                          */
/****************************************************************************/
U14API(short) U14StopCircular(void)
{
   TCSBLOCK csBlock;

   return( U14Control1401(U14_STOPCIRCULAR,&csBlock) );
}

/****************************************************************************/
/*                                                                          */
/*    U14Grab1401                                                           */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14Grab1401(void)
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet     */

   if (sType1401==U14TYPE1401)
   {
      #ifdef macintosh
      TCSBLOCK csBlock;

      sErr=U14Control1401(U14_GRAB1401,&csBlock);
      if (sErr==U14ERR_NOERROR)
         bGrabbed=TRUE;
      #endif
   }

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14Free1401                                                           */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short)  U14Free1401( void )
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet     */

   if (sType1401==U14TYPE1401)
   {
      #ifdef macintosh
      TCSBLOCK csBlock;

      sErr=U14Control1401(U14_FREE1401,&csBlock);
      if (sErr==U14ERR_NOERROR)
         bGrabbed=FALSE;
      #endif
   }

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14Step1401                                                           */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short)  U14Step1401( void )
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet     */

   if (sType1401==U14TYPE1401)
   {
      if (!bGrabbed)                   /* try and grab */
         sErr=U14Grab1401();

      if (bGrabbed)                    /* if we got it */
      {
         #ifdef macintosh
         TCSBLOCK csBlock;

         sErr=U14Control1401(U14_STEP1401,&csBlock);
         #endif
      }
   }

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14StepTill1401                                                       */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short)  U14StepTill1401 (long steps, long targetAddress)
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet     */

   if (sType1401==U14TYPE1401)
   {
      if (!bGrabbed)                   /* try and grab */
         sErr=U14Grab1401();

      if (bGrabbed)                    /* if we got it */
      {
         #ifdef macintosh
         TCSBLOCK csBlock;

         csBlock.ints[0]=targetAddress;
         csBlock.ints[1]=steps;
         sErr=U14Control1401(U14_STEPTILL1401,&csBlock);
         #endif
      }
   }

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14Registers1401                                                      */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short)  U14Registers1401(LPTCSBLOCK lpBlock)
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet     */

   if (sType1401==U14TYPE1401)
   {
      #ifdef macintosh
      sErr=U14Status1401(U14_REGISTERS1401,lpBlock);
      #endif
   }
   
   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14Set1401Registers                                                   */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short)  U14Set1401Registers(LPTCSBLOCK lpBlock)
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet     */

   if (sType1401==U14TYPE1401)
   {
      #ifdef macintosh
      sErr=U14Control1401(U14_SET1401REGISTERS,lpBlock);
      #endif
   }

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14SetOrin                                                            */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14SetOrin(short sOrIn)
{
   short    sErr=U14ERR_NO_SUCH_FN;    /* assume no such function yet       */

   if (sType1401==U14TYPE1401)
   {
      #ifdef macintosh
      TCSBLOCK  csBlock;
      csBlock.ints[0]=sOrIn;           /* value set is the lo byte of sOrIn */
      sErr=U14Control1401(U14_SETORIN,&csBlock);
      #endif
   }

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*    U14GetUserMemorySize                                                  */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14GetUserMemorySize (long FAR * lpMemorySize)
{
   short       sErr;
   long        alLimits[4];

   *lpMemorySize=0;            /* if we get error then leave size set at 0  */

   if (sType1401==U14TYPE1401)
      sErr=U14SendString((LPSTR)"MEMTOP;");
   else
      sErr=U14SendString((LPSTR)"MEMTOP,?;");

   if (sErr==U14ERR_NOERROR)
   {
      sErr=U14LongsFrom1401((long FAR *)alLimits,4);
      if (sErr>0)
      {
         sErr=U14ERR_NOERROR;
         *lpMemorySize=alLimits[0];          /* result for a 1401 plus      */
         if (sType1401==U14TYPE1401)         /* adjust for a 1401           */
            *lpMemorySize= *lpMemorySize-alLimits[1];    /* memtop - membot */
      }
   }
   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/* U14TypeOf1401                                                            */
/*                                                                          */
/*    Returns the type of the 1401, maybe unknown                           */
/*                                                                          */
/****************************************************************************/
U14API(short) U14TypeOf1401(void)
{
   return(sType1401);
}

/****************************************************************************/
/*                                                                          */
/* U14Open1401                                                              */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*    Tries to get the 1401 for use by this application                     */
/*                                                                          */
/* Entry: No Parameters                                                     */
/*                                                                          */
/*                                                                          */
/* Exit: Returns error code                                                 */
/*                                                                          */
/****************************************************************************/

U14API(short) U14Open1401(void)
{
   long     lRetVal;
   short    sErr;
   long     lDriverRev;

#ifdef __HIGHC__
   if (!GetDriverReady())                /* Get comms with driver opened up */
       return sDriverState;
#endif

   sLastRetCode=U14ERR_NOERROR;
   bGrabbed=FALSE;               /* initially we are not in single step mode*/
   lTimeOutPeriod=3*60;          /* 3 second timeout as default             */

   #ifdef MSDOS
   lTimeOutPeriod=lTimeOutPeriod*17;      /* convert to millisecs           */
   sErr=U14CallDriver(U14_OPEN1401, U14_NOSUBFN,(U14PARAM)&lRetVal);
   #endif

   #ifdef macintosh
   appResFile=CurResFile();         /* keep reference for the app resources */
   cmdResFile=OpenResFile(k1401CommandFile);  
   sErr=OpenDriver(k1401DriverName,&ref1401);
   if (sErr==U14ERR_NOERROR)           /* no error, go and find 1401 type   */
   {
      TCSBLOCK csBlock;

      sErr=U14Status1401(U14_TYPEOF1401, &csBlock);
      if (sErr==U14ERR_NOERROR)
         lRetVal=csBlock.ints[1];
   }
   else if (sErr==fnfErr)
      sErr=U14ERR_NO1401DRIV;
   #endif

   if (sErr==U14ERR_NOERROR)
   {
      if (lRetVal==DRIVRET_PLUS)
      {
         #ifdef MSDOS
         F_strcpy((LPSTR)commandext,(LPSTR)".GXC");   /* 1401+ commands     */
         #endif
         sType1401=U14TYPEPLUS;
      }
      else
         if (lRetVal==DRIVRET_STD)
         {
            #ifdef MSDOS
            F_strcpy((LPSTR)commandext,(LPSTR)".CMD");/* standard commands  */
            #endif
            sType1401=U14TYPE1401;
         }
         else
            sType1401=U14TYPEUNKNOWN;

      sErr=U14StateOf1401();
    
      lDriverRev=U14DriverVersion();            /* get driver revision      */

      #ifdef MSDOS
      lDriverRev=((lDriverRev>>16) & 0xFFFF);   /* use hi word for msdos    */
      #endif
   
      if (lDriverRev<MINDRIVERMAJREV)           /* late enough version?     */
         sErr=U14ERR_DRIVTOOOLD;                /* too old                  */

   }         
   return(sErr);
}



/****************************************************************************/
/*                                                                          */
/* U14Close1401                                                             */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*    Closes the 1401 so someone else can use it.                           */
/*                                                                          */
/* Entry: No Parameters                                                     */
/*                                                                          */
/* Exit:  Any Driver error code is returned                                 */
/*                                                                          */
/****************************************************************************/

U14API(short) U14Close1401(void)
{
   U14StopCircular();         /* in case a circular transfer is in progress */

   #ifdef MSDOS
   return( U14CallDriver(U14_CLOSE1401, U14_NOSUBFN, NULL) );
   #ifdef __HIGHC__
   /*  Code here to terminate access to the driver */
   #endif
   #endif

   #ifdef macintosh
   return( CloseDriver(ref1401) );
   #endif
}



/****************************************************************************/
/*                                                                          */
/* U14Reset1401                                                             */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*    Resets the 1401                                                       */
/*                                                                          */
/* Entry: No Parameters                                                     */
/*                                                                          */
/* Exit:  Any Driver error code is returned                                 */
/*                                                                          */
/****************************************************************************/

U14API(short) U14Reset1401(void)
{
   TCSBLOCK    csBlock;

   return( U14Control1401(U14_RESET1401,&csBlock) );
}

/****************************************************************************/
/*                                                                          */
/* U14KillIO1401                                                            */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*    Removes any pending IO from the buffers                               */
/*                                                                          */
/* Entry: No Parameters                                                     */
/*                                                                          */
/* Exit:  Any Driver error code is returned                                 */
/*                                                                          */
/****************************************************************************/

U14API(short) U14KillIO1401(void)
{
   TCSBLOCK    csBlock;
   short       sErr=U14ERR_NOERROR;

   #ifdef macintosh
   sLastRetCode=KillIO(ref1401);       /* stop pending IO */
   #endif

   if (sErr==U14ERR_NOERROR)
      U14Control1401(U14_KILLIO1401,&csBlock);

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*   U14SendString                                                          */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Send character to the 1401                                         */
/*                                                                          */
/****************************************************************************/
U14API(short) U14SendString(LPSTR lpString)
{
   long     lStrLength;                /* length we are sending             */
   long     lTimeOutTicks;             /* when to time out                  */
   BOOLEAN  bSpaceToSend;              /* space to send yet                 */
   BOOLEAN  bTimedOut=FALSE;           /* have we timed out                 */

   lTimeOutTicks=WhenToTimeOut();
   lStrLength=F_strlen(lpString);      /* get string length we want to send */

   do                      /*check sLastRetCode if returns 0 ?*/
      bSpaceToSend=(U14OutBufSpace()>=lStrLength); 
   while ( !bSpaceToSend && !PassedTime(lTimeOutTicks) );

   if (sLastRetCode==U14ERR_NOERROR)          /* no errors? */
   {
      if (bSpaceToSend)
      {
         #ifdef MSDOS
         return( U14CallDriver(U14_SENDSTRING, U14_NOSUBFN,
                                          (U14PARAM)lpString) );
         #endif
         #ifdef macintosh
         return( FSWrite(ref1401,&lStrLength,lpString) );
         #endif
      }
      else
         return(U14ERR_TIMEOUT);
   }
   else
      return(sLastRetCode);
}


/****************************************************************************/
/*                                                                          */
/*   U14SendChar                                                            */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Send character to the 1401                                         */
/*                                                                          */
/****************************************************************************/
U14API(short) U14SendChar(char cChar)
{
   #ifdef MSDOS
   return( U14CallDriver(U14_SENDCHAR, U14_NOSUBFN,(U14PARAM)&cChar) );
   #endif

   #ifdef macintosh
   char  sz[2]=" ";              /* convert to a string and send */
   sz[0]=cChar;
   return(U14SendString(sz));
   #endif

}

/****************************************************************************/
/*                                                                          */
/*   U14GetString                                                           */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Get a string from the 1401. Returns a null terminated string.      */
/*       The string is all the characters up to the next CR in the buffer   */
/*       or the end of the buffer if that comes first.                      */
/*                                                                          */
/****************************************************************************/
U14API(short) U14GetString(LPSTR lpBuffer, WORD wMaxLen)
{
   short    sRetVal=U14ERR_NOERROR;    /* no errors yet                     */
   BOOLEAN  bLineToGet;                /* true when a line to get           */
   long     lTimeOutTicks;             /* when to time out                  */
   BOOLEAN  bTimedOut=FALSE;           /* have we timed out                 */

   /* can only call get string if two or more because we have to place the  */
   /* length in the buffer when we send it                                  */
   /* also a 1 char buffer would only have room for the null terminator so  */
   /* it is a bit pointless                                                 */

   if (wMaxLen>1)    
   {  
      lTimeOutTicks=WhenToTimeOut();

      do     
         bLineToGet=(U14LineCount()!=0);
      while ( !bLineToGet && !PassedTime(lTimeOutTicks) );

      if (bLineToGet)
      {
         if (sLastRetCode==U14ERR_NOERROR)      /* all ok so far */
         {
            #ifdef MSDOS
            *((WORD FAR *)lpBuffer)=wMaxLen;          /* set up length */
            sRetVal=U14CallDriver(U14_GETSTRING, U14_NOSUBFN,
                                  (U14PARAM)lpBuffer);
            #endif

            #ifdef macintosh
            {
               long     lCount;        /* used for mac FSREAD               */
               lCount=wMaxLen-1;       /* so we can null terminate it       */
               sRetVal=FSRead(ref1401,&lCount,lpBuffer);  /* reads one line */
               lpBuffer[(short)lCount]=(char)0; /* null terminate           */
            }
            #endif

            TranslateString(lpBuffer);       /* convert any commas to spaces*/
         }
         else
            sRetVal=sLastRetCode;
      }
      else
         sRetVal=U14ERR_TIMEOUT;
   }
   else
      sRetVal=U14ERR_BUFF_SMALL;

   return(sRetVal);
}

/****************************************************************************/
/*                                                                          */
/*   U14GetChar                                                             */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Get a character from the 1401                                      */
/*                                                                          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14GetChar(LPSTR lpcChar)
{
   short    sErr;
   
   #ifdef MSDOS
   sErr=U14CallDriver(U14_GETCHAR, U14_NOSUBFN,(U14PARAM)lpcChar);
   #endif

   #ifdef macintosh
   char     sz[2];               /* read a very short string                */
   sErr=U14GetString(sz,2);      /* will read one char and nul terminate it */
   *lpcChar=sz[0];               /* copy to result                          */
   #endif

   if (sErr==U14ERR_NOERROR)
      *lpcChar=TranslateChar(*lpcChar);   /* convert any comma to a space   */

   return(sErr);
}

/****************************************************************************/
/*                                                                          */
/*   U14Stat1401                                                            */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Returns 0 for no lines or error                                    */
/*       or non zero for something waiting                                  */
/*                                                                          */
/****************************************************************************/
U14API(short) U14Stat1401(void)
{
   return (U14LineCount()>0);
}


/****************************************************************************/
/*                                                                          */
/*   U14CharCount                                                           */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Returns the number of characters in the input buffer               */
/*                                                                          */
/****************************************************************************/
U14API(short) U14CharCount(void)
{
   TCSBLOCK    csBlock;
   short       sRetVal;

   sRetVal=U14Status1401(U14_STAT1401, &csBlock);

   if (sRetVal==U14ERR_NOERROR)
      sRetVal=csBlock.ints[0];
   else
      sRetVal=sLastRetCode;

   return(sRetVal);
}


/****************************************************************************/
/*                                                                          */
/*   U14LineCount                                                           */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Returns the number of full lines in the input buffer               */
/*                                                                          */
/****************************************************************************/
U14API(short) U14LineCount(void)
{
   TCSBLOCK csBlock;
   short    sRetVal;

   sRetVal=U14Status1401(U14_LINECOUNT,&csBlock);

   if (sRetVal==U14ERR_NOERROR)
      sRetVal=csBlock.ints[0];
   else
      sRetVal=sLastRetCode;

   return(sRetVal);
}

/****************************************************************************/
/*                                                                          */
/*    U14GetTransfer                                                        */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*       Caller fills in AreaNum part of TRANSFERDESC record, we fill       */
/*       in the rest.                                                       */
/*                                                                          */
/*       NOTE: This function will return the current settings for an area   */
/*             THESE VALUES MAY BE RUBBISH IF THE AREA WAS NOT SET UP       */
/*                                                                          */
/****************************************************************************/
U14API(short) U14GetTransfer(LPTRANSFERDESC lpTransDesc)
{
   #ifdef macintosh
   TCSBLOCK    csBlock;
   #endif
   short       sError;

   #ifdef MSDOS
   sError=U14CallDriver(U14_GETTRANSFER,U14_NOSUBFN,(U14PARAM)lpTransDesc);
   #endif

   #ifdef macintosh
   sError=U14Status1401(U14_GETTRANSFER + 256*lpTransDesc->wAreaNum,&csBlock);

   if (sError==U14ERR_NOERROR)
   {
      switch (csBlock.ints[1])
      {
         case 0:
            lpTransDesc->eSize=tszBytes;
         break;

         case 1:
            lpTransDesc->eSize=tszWords;
         break;

         case 3:
            lpTransDesc->eSize=tszLongs;
         break;
      }
      
      lpTransDesc->lpvBuff=(void *)csBlock.longs[1];
   }
   #endif

   return(sError);
}

/****************************************************************************/
/*                                                                          */
/* Function:                                                                */
/*    U14UnSetTransfer  Cancels a transfer area                             */
/*                                                                          */
/* Inputs:                                                                  */
/*    wAreaNum WORD The index of a block previously used in by SetTransfer  */
/*                                                                          */
/****************************************************************************/
U14API(short) U14UnSetTransfer(WORD wAreaNum)
{
   TCSBLOCK    csBlock;

   csBlock.ints[0]=(short)wAreaNum;

   return( U14Control1401(U14_UNSETTRANSFER,&csBlock) );
}

/****************************************************************************/
/*                                                                          */
/* Function:                                                                */
/*    U14SetTransfer    Sets an area up to be used for transfers            */
/*                                                                          */
/* NOTE:                                                                    */
/*    Caller must use GlobalAlloc and GlobalWire on the data area before its*/
/*    address is passed into this routine                                   */
/*                                                                          */
/* Inputs:                                                                  */
/*    lpTransDesc LPTRANSDESC    The address of a block filled in to specify*/
/*                               what a transfer area looks like            */
/*                                                                          */
/****************************************************************************/
U14API(short) U14SetTransfer(LPTRANSFERDESC lpTransDesc)
{
   #ifdef MSDOS

   VXTRANSFERDESC   vxDesc;                     /* Structure to pass to VXD */

   vxDesc.wAreaNum = lpTransDesc->wAreaNum;    /* Copy across simple params */
   vxDesc.wAddrSel = _FP_SEG(lpTransDesc->lpvBuff);   /* 16 bit selector */
   vxDesc.dwAddrOfs = _FP_OFF(lpTransDesc->lpvBuff);    /* 32 bit offset */
   vxDesc.dwLength = lpTransDesc->dwLength;

   U14CallDriver(U14_SETTRANSFER,U14_NOSUBFN,(U14PARAM)&vxDesc);
   #endif

   #ifdef macintosh

   TCSBLOCK    csBlock;

   csBlock.ints[0]=(short)lpTransDesc->wAreaNum;
   csBlock.ints[1]=1;                              /*set cont flag always*/
   csBlock.longs[1]=(long)lpTransDesc->lpvBuff;    /*set the host address*/

   switch (lpTransDesc->eSize)                     /*set the data type*/
   {
      case tszBytes: 
         csBlock.ints[4]=0;
      break;

      case tszWords:
         csBlock.ints[4]=1;
      break;

      case tszLongs:
         csBlock.ints[4]=3;
      break;
   }

   csBlock.longs[3]=lpTransDesc->dwLength;         /*data buffer size*/
   U14Control1401(U14_SETTRANSFER,&csBlock);
   #endif

   return(sLastRetCode);
}


/****************************************************************************/
/*                                                                          */
/*    Transfer                                                              */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*    Transfer moves data to 1401 or to host                                */
/*                                                                          */
/*    Assumes memory is allocated and locked,                               */
/*    which it should be to get a pointer                                   */
/*                                                                          */
/****************************************************************************/
U14API(short) Transfer(BOOLEAN bTo1401,LPSTR lpData,
                          DWORD dwSize,long lAddr1401,short eSz)
{
   char           strcopy[MAXSTRLEN+1];/* to hold copy of work string       */
   long           er[5];               /* for decoding 1401 replies         */
   char           szDirStr[5];         /* direction string 'HOST' or '1401' */
   TRANSFERDESC   rTD;                 /* description of area to set up     */
   short          sResult;             /* fn return value,                  */

   sResult=U14ERR_NOERROR;             /* assume successs */

   rTD.wAreaNum=0;                     /* always uses zero */
   rTD.lpvBuff=(void FAR *)lpData;
   rTD.dwLength=dwSize;
   rTD.eSize=eSz;                      /* for future/mac compatibility      */

   #ifdef MSDOS
   #ifndef __HIGHC__
   GlobalPageLock(HIWORD(lpData));
   #endif
   #endif

   sResult=U14SetTransfer((LPTRANSFERDESC)&rTD);
   if (sResult == U14ERR_NOERROR)                     /* no error */
   {
      if (bTo1401)
         F_strcpy((LPSTR)szDirStr,(LPSTR)"1401");
      else
         F_strcpy((LPSTR)szDirStr,(LPSTR)"HOST");

      #ifdef MSDOS
      #ifndef __HIGHC__
      wsprintf((LPSTR)strcopy,                  /* data offset is always 0  */
               (LPSTR)"TO%s,$%lX,$%lX,0;ERR;",
               (LPSTR)szDirStr,lAddr1401,dwSize);
      #else
      sprintf(strcopy,                  /* data offset is always 0  */
               "TO%s,$%lX,$%lX,0;ERR;",
               szDirStr,lAddr1401,dwSize);
      #endif
      #endif

      #ifdef macintosh
      sprintf((LPSTR)strcopy,                /* data offset is always 0     */
              (LPSTR)"TO%s,$%lX,$%lX,0;ERR;",
              (LPSTR)szDirStr,lAddr1401,dwSize);
      #endif

      U14SendString((LPSTR)strcopy);         /* send transfer string        */

      sResult=U14LongsFrom1401((long FAR *)er,5);
      if (sResult>0)
      {
         sResult=U14ERR_NOERROR;
         if (er[0]!=0L)
            sResult=U14ERR_TOXXXERR;
      }

      U14UnSetTransfer(rTD.wAreaNum);
   }
      
   #ifdef MSDOS
   #ifndef __HIGHC__
   GlobalPageUnlock(HIWORD(lpData));
   #endif
   #endif

   return(sResult);
}


/****************************************************************************/
/*                                                                          */
/*     Function  ToHost transfers data into the host from the 1401          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14ToHost(LPSTR lpAddrHost,DWORD dwSize,long lAddr1401,
                                                           short eSz)
{
   short    sResult=U14ERR_NOERROR;      /* function return value assume ok */

   if (dwSize)                            /* TOHOST is a const*/
      sResult=Transfer(TOHOST,lpAddrHost,dwSize,lAddr1401,eSz);

   return(sResult);
}


/****************************************************************************/
/*                                                                          */
/*     Function  To1401 transfers data into the 1401 from the host          */
/*                                                                          */
/****************************************************************************/
U14API(short) U14To1401(LPSTR lpAddrHost,DWORD dwSize,long lAddr1401,
                                                            short eSz)
{
   short  sResult=U14ERR_NOERROR;      /* function return value assume ok   */

   if (dwSize)                         /* TO1401 is a const */
      sResult=Transfer(TO1401,lpAddrHost,dwSize,lAddr1401,eSz);

   return(sResult);
}



/****************************************************************************/
/*                                                                          */
/*   Function  LdCmd    Loads a command from a full path or just a file     */
/*                                                                          */
/****************************************************************************/
U14API(short) U14LdCmd(LPSTR command)
{
   LPSTR          lpMem=NULL;    /* pointer to the space after locking      */
   long           er[5];         /* for decoding 1401 replies               */
   char           strcopy[MAXSTRLEN+1];   /* to hold copy of work string    */
   TRANSFERDESC   rTD;           /* description of area to set up           */
   short          sResult;       /* function return value                   */
   WORD           wComSize;      /* size of the command we want to load in  */
   BOOLEAN        bDataLocked=FALSE;   /* so we know to unlock              */

   #ifdef MSDOS
   BOOLEAN        bDotIn=FALSE;  /* is the dot part already there ?         */
   CMDHEAD        rCmdHead;      /* block is our pointer                    */
   int            iFHandle;      /* file handle of command                  */
   char           filnam[80];    /* space to build name in                  */
   WORD           wLoop;         /* loop counter                            */
   HANDLE         hMemHand=0;    /* handle to memory                        */
   #endif

   #ifdef macintosh
   Handle         hResource;     /* resource handle                         */
   short          saveResFile;   /* save current resource                   */
   #endif


   sResult=U14ERR_NOERROR;       /* ok so far                               */

   #ifdef MSDOS
   for(wLoop=0;command[wLoop];wLoop++)             /* search through string */
      if (command[wLoop]=='.')
         bDotIn=TRUE;

   if (bDotIn)                                     /* see if needs .CMD     */
      F_strcpy((LPSTR)filnam,(LPSTR)command);
   else
   { 
      F_strcpy((LPSTR)filnam,(LPSTR)"\\1401\\");   /* form full path name   */
      F_strcat((LPSTR)filnam,(LPSTR)command);
      F_strcat((LPSTR)filnam,(LPSTR)commandext);   
   }

   if ((iFHandle=_open((LPSTR)filnam,_O_RDONLY | _O_BINARY)) == -1)  /* HIGHC */
      sResult=U14ERR_NOFILE;
   else
   {                                      /* first read in the header block */
      if (_read(iFHandle,(LPSTR)&rCmdHead,sizeof(CMDHEAD)) != sizeof(CMDHEAD))
         sResult=U14ERR_READERR;
      else
      {
         wComSize=rCmdHead.wCmdSize;
#ifdef __HIGHC__
         if ((lpMem = malloc(wComSize)) != NULL)
         {
            _lseek(iFHandle, (long)sizeof(CMDHEAD), SEEK_SET);
            if (_read(iFHandle,lpMem,wComSize) != wComSize)
                sResult=U14ERR_READERR;
         }
         else
            sResult=U14ERR_HOSTSPACE;     /* not enough space to load       */
#else
         if (hMemHand = GlobalAlloc(GMEM_MOVEABLE,wComSize))
         {
            if (lpMem = GlobalLock(hMemHand))
            {
               _lseek(iFHandle, (long)sizeof(CMDHEAD), SEEK_SET);
               if (_read(iFHandle,lpMem,wComSize) != wComSize)
                  sResult=U14ERR_READERR;
               GlobalPageLock((WORD)(((DWORD)lpMem>>16) & 0xFFFF));
               bDataLocked=TRUE;          /* remember its locked            */
            }
            else
               sResult=U14ERR_LOCKERR;    /* not enough space to load       */
         }
         else
            sResult=U14ERR_HOSTSPACE;     /* not enough space to load       */
#endif
      }
      _close(iFHandle);             /* HIGHC close the file we used         */
   }
   #endif


   #ifdef macintosh
   saveResFile=CurResFile();              /* remember current resource file */
   sResult=GetCommand(appResFile,command,&hResource); /* try app resource   */
   if (sResult)                                       /* if no good try the */
      sResult=GetCommand(cmdResFile,command,&hResource); /* general commands*/

   UseResFile(saveResFile);               /* restore previous resource file */
   HLock(hResource);                      /* lock the relocatable block     */
   bDataLocked=TRUE;                      /* say we locked it               */
   lpMem=(LPSTR)*hResource;               /* pointer to the command         */
   wComSize=(unsigned char)lpMem[0]+256*(unsigned char)lpMem[1];/* cmd size */
   lpMem=lpMem+2;     /* +2 as mac buffer has length for cload at the start */
   #endif

   /* the command is now in the buffer pointed at by lpMem                  */

   if (sResult==U14ERR_NOERROR)
   {
      rTD.wAreaNum=0;                  /* always uses zero */
      rTD.lpvBuff=(void FAR *)lpMem;
      rTD.dwLength=wComSize;
      rTD.eSize=tszBytes;
      sResult=U14SetTransfer((LPTRANSFERDESC)&rTD);
      if(sResult == U14ERR_NOERROR)
      {
         #ifdef MSDOS
         wsprintf((LPSTR)strcopy,(LPSTR)"CLOAD,0,$%lX;ERR;",rTD.dwLength);
         #endif

         #ifdef macintosh
         sprintf(strcopy,"CLOAD,0,$%lX;ERR;",rTD.dwLength);
         #endif

         sResult=U14SendString((LPSTR)strcopy);

         if (sResult==U14ERR_NOERROR)
         {
            sResult=U14LongsFrom1401((long FAR *)er,5);
            if (sResult>0)
            {
               sResult=U14ERR_NOERROR;
               if (er[0] != 0)
                  sResult=U14ERR_CLOADERR;
            }
         }
         U14UnSetTransfer(rTD.wAreaNum);
       }
   }


   /* now clean up the mess of locked and allocated bits                    */
   #ifdef MSDOS
   #ifdef __HIGHC__
   if (lpMem != NULL)
       free(lpMem);
   #else
   if (bDataLocked)                             /* if locked                */
   {                    
      GlobalPageUnlock((WORD)(((DWORD)lpMem>>16) & 0xFFFF));/* undo locking */
      GlobalUnlock(hMemHand);                   /* unlock it                */
   }

   if (hMemHand)                                /* if allocated             */
      GlobalFree(hMemHand);                     /* free it up               */
   #endif
   #endif

   #ifdef macintosh
   if (bDataLocked)
      ReleaseResource(hResource);   /* we have finished with this resource  */
   #endif

   return(sResult);
}

/****************************************************************************/
/*                                                                          */
/*                                                                          */
/*    Ld                                                                    */
/*                                                                          */
/*    DESCRIPTION                                                           */
/*                                                                          */
/*    Loads a command into the 1401                                         */
/*                                                                          */
/*    Returns NOERROR code or a long with error in lo word and index of     */
/*    command that failed in high word                                      */
/*                                                                          */
/****************************************************************************/
U14API(unsigned long) U14Ld(LPSTR vl,LPSTR str)
{
   WORD      wIndex=1;              /* index to current command             */
   short     sErr=U14ERR_NOERROR;   /* what the error was that went wrong   */
   WORD      wLoop1;                /* loop counter                         */
   WORD      wLoop2;                /* loop counter                         */
   BOOLEAN   bDone=FALSE;           /* true when finished all commands      */
   char      filnam[MAXSTRLEN+1];   /* filename to use                      */
   char      szFName[MAXSTRLEN+1];  /* filename work string                 */
   char      strcopy[MAXSTRLEN+1];  /* general work string                  */
   long      er[5];                 /* for decoding 1401 replies            */

   #ifdef macintosh
   vl=NULL;                         /* not needed on mac so set to good val */
   #endif

   F_strcpy((LPSTR)strcopy,(LPSTR)str);   /* to avoid changing original     */

   /* now break out one command at a time and see if loaded                 */

   if (*str)                              /* if anything there              */
   {
      wLoop1=0; wLoop2=0;
      do                                  /* repeat until end of str        */
      {
         if (!strcopy[wLoop1])            /* at the end of the string?      */
            bDone=TRUE;                   /* set the finish flag            */

         if ((strcopy[wLoop1] == ',') || (!strcopy[wLoop1])) /* end of cmd? */
         {
            szFName[wLoop2]=(char)0;      /* null terminate name of command */
            U14SendString((LPSTR)szFName);               /* ask if loaded   */
            U14SendString((LPSTR)";ERR;");               /* add err return  */

            sErr=U14LongsFrom1401((long FAR *)er,5);     /* await reply     */

            if (sErr>0)
            {
               sErr=U14ERR_NOERROR;
               if (er[0] == 255L)         /* if command not loaded at all   */
               {
                  if ((vl) && (*vl))      /* if we have a path name         */
                  {  
                     F_strcpy((LPSTR)filnam,(LPSTR)vl);
                     F_strcat((LPSTR)filnam,(LPSTR)szFName);
                     F_strcat((LPSTR)filnam,(LPSTR)commandext);
                  }
                  else
                     F_strcpy((LPSTR)filnam,(LPSTR)szFName); /*simple name*/

                  wIndex++;
                  sErr=U14LdCmd((LPSTR)filnam);    /* load command          */
                  if (sErr != U14ERR_NOERROR)      /* spot any errors       */
                     bDone=TRUE;                   /* give up if an error   */
               }
            }
            else
               bDone=TRUE;                         /* give up if an error   */

            wLoop2=0;
            wLoop1++;
         }
         else
            szFName[wLoop2++]=strcopy[wLoop1++];
      }
      while (!bDone);
   }
   return( (sErr!=U14ERR_NOERROR) ? ((wIndex<<16)+sErr) : U14ERR_NOERROR);
}                                       

#ifdef MSDOS
#ifndef __HIGHC__
/****************************************************************************/
/*                       Windows Specifics                                  */
/****************************************************************************/

/****************************************************************************/
/*                                                                          */
/*  DriverCallBack                                                          */
/*                                                                          */
/*  Function called by driver when someting interesting happens             */
/*                                                                          */
/*  Sends a message to the registered window if there is one                */
/*                                                                          */
/****************************************************************************/

void FAR PASCAL DriverCallBack(long lParam)
{
   if (hCallBackWnd !=NULL)
   {
      PostMessage(hCallBackWnd,wCEDMessage,0,lParam);
   }
}



/****************************************************************************/
/*                                                                          */
/*   U14RegCallBackWnd                                                      */
/*                                                                          */
/*   Registers a window for messages to be sent to by callback              */
/*   routine and enables call back                                          */
/*                                                                          */
/****************************************************************************/

U14API(short) U14RegCallBackWnd(HWND hWnd, LPWORD lpwMessCode)
{
   short    sRetVal;                /* return value */
   long     lFuncAddr;              /* address of callback function */

   if (hCallBackWnd==NULL)          /* not already assigned */
   {
      lFuncAddr=(long)DriverCallBack;  /* address of call back function */
      sRetVal=U14CallDriver(U14_REGCALLBACK, U14_NOSUBFN,(U14PARAM)lFuncAddr);
      hCallBackWnd=hWnd;            /* remember wher to send messages */

      *lpwMessCode=wCEDMessage;     /* our message identifier           */
   }
   else
      sRetVal=U14ERR_CBALREADY;     /* there is already a callback    */


   return(sRetVal);
}

/****************************************************************************/
/*                                                                          */
/*   U14DeRegCallBackWnd                                                    */
/*                                                                          */
/*   Deregisters the window for messages to be sent to by callback          */
/*   routine and disables call back                                         */
/*                                                                          */
/****************************************************************************/
U14API(short) U14DeRegCallBackWnd(HWND hWnd)
{
   short    sRetVal;                /* return value */
   long     lRemove=0;              /* code to remove call back */

   if (hWnd==hCallBackWnd)
   {
      sRetVal=U14CallDriver(U14_REGCALLBACK, U14_NOSUBFN,(U14PARAM)lRemove);
      hCallBackWnd=NULL;
   }
   else
      sRetVal=U14ERR_BADDEREG;      /* not same handle as registered */

   return(sRetVal);
}

/****************************************************************************/
/*                                                                          */
/*   U14GetMonBuff                                                          */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Retrivies the data in the monitor buffer into the buffer saupplied */
/*                                                                          */
/****************************************************************************/
U14API(short) U14GetMonBuff(LPSTR lpBuffer, WORD wMaxLen)
{
   short    sRetVal;

   /* can only call get buffer if two or more because we have to place the  */
   /* length in the buffer when we send it                                  */
   /* also a 1 char buffer would only have room for the null terminator so  */
   /* it is a bit pointless                                                 */

   if (wMaxLen>1)    
   {                             
      *((WORD far *)lpBuffer)=wMaxLen;          /* set up length */
      sRetVal=U14CallDriver(U14_GETMONITORBUF,U14_NOSUBFN,(U14PARAM)lpBuffer);
   }
   else
      sRetVal=U14ERR_BUFF_SMALL;

   return(sRetVal);
}


/****************************************************************************/
/* U14CallDriver                                                            */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*    Calls the V1401D device if it has been located                        */
/*                                                                          */
/* bMainFn and bSubFn are passed to the driver in AH and AL to indicate     */
/* which driver function is required. The pointer lpParams points at a      */
/* block of data which is function dependent.                               */
/*                                                                          */
/****************************************************************************/

U14API(short) U14CallDriver(BYTE bMainFn,BYTE bSubFn,U14PARAM lpParams)
{
   short    sRetVal;                      /* function return */
   WORD     wWholeFn;

   wWholeFn=(bMainFn<<8)+bSubFn;

   if (V1401D_API != 0)                                   /* is it defined? */
   {
      {
         mov   ax,wWholeFn                ;set function number
         les   di,lpParams                ;parameter block address in es:di
         call  dword ptr [V1401D_API]     ;call V1401D device
         mov   sRetVal,ax                 ;retrieve driver result
      }
   }
   else
      sRetVal=sDriverState;               /* return driver installed state */
      
   sLastRetCode=sRetVal;                  /* remember last code returned   */
   return(sRetVal);

}


/****************************************************************************/
/*                                                                          */
/*   LibMain                                                                */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       DLL Entry point, looks to see if we are in enhanced windows mode   */
/*       and if V1401D is installed.                                        */
/*                                                                          */
/*    Sets the global var sDriverState to indicate what we found out        */
/*                                                                          */
/****************************************************************************/

U14API(BOOL) LibMain(HANDLE hMod,WORD wDSeg,WORD wHeapSz,LPSTR lpszCmdLine)
{
   hModule=hMod;                       /* make instance handle global */

   wCEDMessage=RegisterWindowMessage(WM_CEDCALLBACK_STR);

   /* get API entry to V1401D--if it is installed */
   _asm
   {
      mov     ax,1600h                 ;enhanced mode?
      int     2Fh                      ;api call
      test    al,7Fh                   ;enhance mode running?
      jz      Not_Running_Enhanced     ;no

      mov     ax,1684h                 ;get device API call
      mov     bx, CED_1401_Device_ID     ;for the V1401D VxD
      int     2Fh                      ;get api entry point
      mov     word ptr V1401D_API,di   ;save the callback address
      mov     word ptr V1401D_API+2,es

      mov     ax,es                    ;is V1401D installed?
      or      ax,di
      jz      V1401D_Not_Installed     ;if not, split

      mov     sDriverState,U14ERR_NOERROR    ;show success
      jmp     get_out

Not_Running_Enhanced:
      mov     sDriverState,U14ERR_NO386ENH   ;no enhanced windows!
      jmp     Get_Out                        ;return our error code

V1401D_Not_Installed:
      mov     sDriverState,U14ERR_NO1401DRIV ;no v1401d installed

Get_Out:
   }
   return (TRUE);    /* return success--even though we may have failed */

} /* LibMain() */


/****************************************************************************/
/*                                                                          */
/*  WEP                                                                     */
/*                                                                          */
/*  DESCRIPTION:                                                            */
/*      Performs cleanup tasks when the .DLL is unloaded.  The WEP() is     */
/*      called automatically by Windows when the .DLL is unloaded (no       */
/*      remaining tasks still have the .DLL loaded).  It is strongly        */
/*      recommended that a .DLL have a WEP() function, even if it does      */
/*      nothing but return, as in this example.                             */
/*                                                                          */
/*      Make sure that the WEP() is @1 RESIDENTNAME in the EXPORTS          */
/*      section of the .DEF file.  This ensures that the WEP() can          */
/*      be called as quickly as possible.  Incidently, this is why          */
/*      the WEP() is called the WEP() instead of WindowsExitProcedure().    */
/*      It takes up the minimum amount of space and is quickly located.     */
/*                                                                          */
/*                                                                          */
/****************************************************************************/

U14API(BOOL) WEP(int nArgument)
{
   return ( TRUE );                   /* always return TRUE (1) */
} /* WEP() */


/****************************************************************************/
/*                       End of Windows Specifics                           */
/****************************************************************************/

#endif

/****************************************************************************/
/*                                                                          */
/*   U14GetCircSelector                                                     */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Returns the selector to the circular mode buffer set up by the     */
/*       driver or 0 for not set up, or an error code.                      */
/*                                                                          */
/****************************************************************************/
U14API(short) U14GetCircSelector(void)
{
   WORD     wSelector;
   short    sError;

   sError=U14CallDriver(U14_GETCIRCSELECTOR,U14_NOSUBFN,
                                    (U14PARAM)&wSelector);

   if (sError==U14ERR_NOERROR)
      sError=(short)wSelector;

   return(sError);
}        


#endif

#ifdef __HIGHC__

/****************************************************************************/
/*                                                                          */
/*   GetDriverReady                                                         */
/*                                                                          */
/*   DESCRIPTION                                                            */
/*                                                                          */
/*       Called to set up access to driver, looks to see if we are in       */
/*       enhanced windows mode and if V1401D is installed.                  */
/*                                                                          */
/*    Sets the global var sDriverState to indicate what we found out        */
/*                                                                          */
/****************************************************************************/

U14API(BOOLEAN) GetDriverReady()
{ 
    SWI_REGS     r;
    union  REGS  rg;
    struct SREGS s;
     
    memset(&r, 0, sizeof(r));
    r.eax = 0x1600; 
    _dx_real_int(0x2f, &r); 
    if (!(((r.eax & 0xff) > 1) && ((r.eax & 0xff) != 0x80)))
    {
        sDriverState = U14ERR_NO386ENH;   /* no enhanced windows! */
        return FALSE;
    }
    memset(&rg, 0, sizeof(rg));      /* Now for the second call - get address */
    memset(&s, 0, sizeof(s));      /* Initialise our reg structures */
    rg.w.eax = 0x1684;              /* Code for get address */
    rg.w.ebx = CED_1401_Device_ID;    /* Device driver ID */
    _int86x(0x2f, &rg, &rg, &s);

    if (((rg.w.edi & 0xFFFF) | s.es) == 0)
    {
        sDriverState = U14ERR_NO1401DRIV;   /* no driver found ! */
        return FALSE;
    }
    V1401D_API_S = (USHORT)s.es;
    V1401D_API_O = (ULONG)rg.w.edi;
    return TRUE;
} 


/****************************************************************************/
/* U14CallDriver                                                            */
/*                                                                          */
/* DESCRIPTION                                                              */
/*                                                                          */
/*    Calls the V1401D device if it has been located                        */
/*                                                                          */
/* he driver in AH and AL to indicate     */
/* which driver function is required. The pointer lpParams points at a      */
/* block of data which is function dependent.                               */
/*                                                                          */
/****************************************************************************/
extern SHORT CallDrv(SHORT sel, DWORD addr, LONG func, U14PARAM lpParam);

U14API(short) U14CallDriver(BYTE bMainFn,BYTE bSubFn,U14PARAM lpParams)
{
   short    sRetVal;                      /* function return */
   LONG     wWholeFn;

   wWholeFn=(bMainFn<<8)+bSubFn;

   if (V1401D_API_S != 0)                                 /* is it defined? */
   {
/*     printf("About to call driver %d %d %d %d\n",V1401D_API_S, V1401D_API_O, wWholeFn, lpParams); */
       sRetVal = CallDrv(V1401D_API_S, V1401D_API_O, wWholeFn, lpParams);
   }
   else
      sRetVal=sDriverState;               /* return driver installed state */
      
   sLastRetCode=sRetVal;                  /* remember last code returned   */
   return(sRetVal);

}


#endif



#ifdef macintosh
/****************************************************************************/
/*                       Macintosh Specifics                                */
/****************************************************************************/

/******************** S t a t u s 1 4 0 1 ( ) **************************
**
** Invokes the 1401 drivers status routine with the code 'csCode'.  The 
** parameters are returned in 'pBlock'. The result is false if the 'csCode' 
** given is unsupported.
*/
U14API(short) U14Status1401(short csCode, TCSBLOCK *pBlock)
{
    sLastRetCode = Status(ref1401, csCode, pBlock);
    return (sLastRetCode);
}

/******************** C o n t r o l 1 4 0 1 ( ) *************************
**
** Invoke the 1401 drivers control routine with the code 'csCode'.  The 
** parameters sent should be in 'pBlock'.  The result is returned false
** if the 'csCode' given is unsupported.
*/
U14API(short) U14Control1401(short csCode, TCSBLOCK *pBlock)
{
    sLastRetCode = Control(ref1401, csCode, pBlock);
    return (sLastRetCode);
}

/***************** G e t C o m m a n d ( ) *****************************
**
** Try to get the resource containing the command name and return a HANDLE 
** to it. resFile is the resource file to look in. We return ResErr if we 
** couldn't find it.
*/
U14API(OSErr) GetCommand(short sResFile,char * szName,Handle *hReturn)
{
   char     szTemp[MAXSTRLEN];
   ResType  theResType;
   OSErr    sErr;

   UseResFile(sResFile);
   sErr=ResError();
   if (!sErr)
   {
      theResType=
         (sType1401==U14TYPE1401) ? U14_RES1401COMMAND : U14_RESPLUSCOMMAND;
      strncpy(szTemp,szName,MAXSTRLEN);
      *hReturn=GetNamedResource(theResType,c2pstr(szTemp)); 
      if (ResError())
        sErr=U14ERR_UNKNOWN;                          /* unknown command */
   }
   else
      sErr=U14ERR_NOFILE;
        
   return(sErr);
}

/****************************************************************************/
/*                       End of Macintosh Specifics                         */
/****************************************************************************/
#endif
