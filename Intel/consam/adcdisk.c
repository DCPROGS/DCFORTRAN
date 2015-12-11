#include "windows.h"
#include "use1401.h"
#include "adcdisk.h"
#include "conio.h"
#include "stdio.h"


#define DMASIZ 50000                   /* The size of a DMA transfer block */

char asBuff[DMASIZ]; /* Buffer to hold DMA data */

// this is Fortran function :

extern void  _stdcall RETURN_COUNT(int nDone,int counter, int button, float sec, long nBytes, int* iCancel);

//	using Windows API functions to get control of keyboard status

/****************************************************************************
**
** This is the function that implements the actual sampling loop. By the time
**  it is called, the 1401 and the output file are both set up and ready,
**  including loading the 1401 command, setting the file size and setting up
**  the DMA transfer buffer
*/
int doit(HANDLE hFile, short sHand, long nBytes, const char* szChans,
                       const char* szClk, int div1, int div2,
                       TCancelFunc* pfCancel, int nBuff, int counter, int* ibutton, float sec)
{
    int     iErr = 0;
    int     nDone = 0;
    int     n1401 = 0;
	int		iCancel= 0;
    int     nThis, nPos, nBehind;
	int		kcancel = 0;
    int     nRepeat = (nBytes + nBuff-1) / nBuff;
    char    szBuff[120];
    long    lVals[4];
	short	istat;
	
    BOOL    bDone = FALSE;
    DWORD   dwWrit;
    int button=0;
	int dmasiz=DMASIZ;
	if(dmasiz > nBytes)
		dmasiz=nBytes;
    wsprintf(szBuff, "ADCBST,I,2,0,%d,%s,%d,%s,%d,%d;ERR;", nBuff, szChans,
                     nRepeat, szClk, div1, div2);
	button=0;
    iErr = U14SendString(sHand, szBuff);              /* Kick off the 1401 */
    if (iErr != U14ERR_NOERROR)
        return iErr;

    iErr = U14LongsFrom1401(sHand, lVals, 2);       /* Check we started OK */
    if (iErr != 2)                            /* and give up if we did not */
    {
        U14Reset1401(sHand);
        if (iErr >= 0)
            iErr = U14ERR_DRIVCOMMS;
        return iErr;
    }
    if (lVals[0] != 0)        /* If we got an error, presunmably bad param */
    {
        U14Reset1401(sHand);
        return ADE_BAD_PARAM;
    }

    while (!bDone)                       /* This is the main sampling loop */
    {                                /* Keep going until done or cancelled */
        U14SendString(sHand, "ADCBST,P;");  /* First find out where we are */
        nPos =U14LongsFrom1401(sHand, lVals, 2);
        if (nPos != 1)
        {
            nPos = n1401;              /* Get a reasonable exit sorted out */
            nBehind = 0;
            bDone = TRUE;                     /* if 1401 comms have failed */
            iErr = U14ERR_DRIVCOMMS;
        }
        else
        {
            nPos = lVals[0];     /* This is where the sampling has reached */
            nBehind = nPos - n1401; /* First we need to check for problems */
            if (nBehind < 0)     /* if we are not keeping up with the 1401 */
                nBehind += nBuff;
            if (nBehind > 0.75*nBuff)             /* Failure point is 3/4 full */
            {
                nBehind = 0;           /* Get a reasonable exit sorted out */
                bDone = TRUE;        /* if the 1401 sampling has got ahead */
                iErr = ADE_TOO_FAST;
            }
        }
        if (nBehind >= dmasiz)                    /* Enough data to write? */
        {
            nThis = nBytes - nDone;         /* Bytes to transfer this time */
            if (nThis > dmasiz)                        /* but not too much */
                nThis = dmasiz;

            wsprintf(szBuff, "TOHOST,%d,%d,0;ERR;", n1401, nThis);
            U14SendString(sHand, szBuff);     /* Transfer data to the host */
            iErr = U14LongsFrom1401(sHand, lVals, 2);
            if (iErr != 2)
            {
                if (iErr >= 0)              /* Check for transfer problems */
                    iErr = U14ERR_DRIVCOMMS;
                bDone = TRUE;
            }
            else if (lVals[0] != 0)                /* and for a 1401 error */
            {
                iErr = U14ERR_DRIVCOMMS;
                bDone = TRUE;
            }                            /* Before falling into disk write */
            else if (!WriteFile(hFile, asBuff, nThis, &dwWrit, NULL))
            {
                iErr = ADE_BAD_WRITE;                /* Write the new data */
                bDone = TRUE;
            }
            else
            {
             
				nDone += nThis;              /* Adjust count of bytes read */
                n1401 += nThis;    /* and where we have got to in the 1401 */
                if (n1401 >= nBuff)
                    n1401 -= nBuff;      /* Wrap round in 1401 if we must */
                if (nDone >= nBytes)
                    bDone = TRUE;                /* Have we finished then? */
				if (nDone < nBytes)
				{
					RETURN_COUNT(nDone,counter, button, sec, nBytes, &iCancel);
					button=nDone;
            		if (iCancel != 0)
					{
					//	U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
					//	bDone = TRUE;
					//	iErr=nDone;
					}
					else if(kcancel != 0)
					{
					//	U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
					//	bDone = TRUE;
					//	iErr=nDone;
					}
					else if(GetAsyncKeyState(27) != 0)
					{
				   			kcancel=1;
							U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
							bDone = TRUE;
							iErr=nDone;
					}
					
				}
				#ifdef _DEBUG
					wsprintf(szBuff, "Done %d of %d, 1401 posn %d, sample pos %d, %d behind\n", nDone, nBytes, n1401, nPos, nBehind - nThis);
					OutputDebugString(szBuff);
				#endif
			}
        }
        else
        {
			 
			 if (nDone < nBytes)
             {
				RETURN_COUNT(nDone,-1, button, sec, nBytes, &iCancel);
				button=nDone;
				if (iCancel != 0) 
				{
                    U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
                    bDone = TRUE;
					iErr=nDone;
                    if (iCancel < 0)
                        iErr = ADE_ABORTED;
                }
				else if (kcancel != 0)
					{
                    U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
                    bDone = TRUE;
					iErr=nDone;
                    if (kcancel < 0)
                        iErr = ADE_ABORTED;
               }
			   else if(GetAsyncKeyState(27) != 0)
			   {
				   		kcancel=1;
						U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
						bDone = TRUE;
						iErr=nDone;
			   }
					
			 }
 
        }
    }
    
   
    if (iErr = ADE_TOO_FAST)
	{   
		U14SendString(sHand, "ADCBST,K;"); /* Shut up the 1401 */
		iErr = nDone;
	}
    else  if (iErr >= 0)
        iErr = nDone; /* Return bytes done if all OK */
	else	
        U14Reset1401(sHand);

		*ibutton=nDone;
    return iErr;
}

/****************************************************************************
**
** This is the AdcDisk function itself. It takes care of opening & setting up
**  the 1401 and creating and initialising the output file before calling
**  the doit function to do the work.
*/
int __stdcall AdcDisk(const char* szFile, int nHead, long nBytes,
                      const char* szChans, const char* szClk, int div1,
                      int div2, TCancelFunc* pfCancel, const char* szDir, 
					  int nBuff, int counter, int *button, float sec)
{
    short   sHand;
    int     iErr = 0;
	int ibutton;
    HANDLE  hFile;
    char    szBuff[120];
	LPCSTR  vl;
	vl=szDir;
    sHand = U14Open1401(0);                    /* Now try to open the 1401 */
    if (sHand >= 0)                            /* and check that it worked */
    {
        long    lSize;
        int     nType;
        int*    pHead;

        U14GetUserMemorySize(sHand, &lSize);      /* 1401 memory available */
        if (lSize < nBuff + 50000)           /* We actually only use nBuff bytes */
        {
         if(lSize > 100000)
		 {
			nBuff=100000;
         }
		 else
		 {
			U14Close1401(sHand);
            return U14ERR_OUTOFMEMORY;
         }
		}
	//	if(nBuff > nBytes)
	//	{
	//		if(nBytes > 1000 && nBytes < DMASIZ)
	//			nBuff=nBytes;
	//	}
        nType = U14TypeOf1401(sHand);    /* We need to check the 1401 type */

        if (nType <= U14TYPEPLUS)
            iErr = U14Ld(sHand, vl, "KILL,ADCBST");  /* Get command loaded */
        else
        {
            U14SendString(sHand, "EVENT,T,16;");   /* Trigger to E4 please */
            iErr = U14Ld(sHand, vl, "KILL,ADCMEM");  /* ADCBST with ADCMEM */
        }
        if (iErr != U14ERR_NOERROR)          /* and check that this worked */
        {
            U14Close1401(sHand);
            return (int)((short)(iErr & 0xffff));   /* The main error code */
        }

        if (nHead < 64)                 /* Make sure the header size is OK */
        {
            U14Close1401(sHand);
            return ADE_BAD_PARAM;
        }
        pHead = (int*)malloc(nHead);           /* Get space for the header */
        if (pHead == NULL)
        {
            U14Close1401(sHand);
            return U14ERR_OUTOFMEMORY;
        }

        iErr = U14SetTransArea(sHand, 0, asBuff, DMASIZ, 0);
        if (iErr == U14ERR_NOERROR)      /* OK, now the 1401 is sorted out */
        {
            hFile = CreateFile(szFile, GENERIC_READ | GENERIC_WRITE, 0, NULL,
                                CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
            if (hFile != INVALID_HANDLE_VALUE)
            {
                DWORD   dwWrit;
                char*   pTok;
                int     nChan = 0;

                strcpy(szBuff, szChans);     /* We want to know the number */
                pTok = strtok(szBuff, " ");
                while (pTok != NULL)          /* of channels to be sampled */
                {                                /* So we count the tokens */
                    nChan++;
                    pTok = strtok(NULL, " ");
                }

                memset(pHead, 0, nHead);             /* Initialise  header */
                pHead[0] = 0x0cedadcd;         /* Signature (CED ADC Disk) */
                pHead[1] = nHead;                       /* the header size */
                pHead[2] = nChan;
                pHead[3] = div1 * div2; /* Interval (to be adjusted below) */

                switch (szClk[0])        /* Get the sample interval sorted */
                {
                    case 'T' : pHead[3] *= 10; break;      /* 10 MHz clock */
                    case 'H' : pHead[3] *= 25; break;       /* 4 MHz clock */
                    case 'C' : pHead[3] *= 100; break;      /* 1 MHz clock */
                }

                /* Write the file header to the start of the file */
                if (!WriteFile(hFile, pHead, nHead, &dwWrit, NULL))
                    iErr = ADE_BAD_WRITE;
                else                       /* Now get the file size set OK */
                {
                    long    offset = nBytes + nHead;

                    if (SetFilePointer(hFile, offset, NULL, FILE_BEGIN) != (DWORD)offset)
                        iErr = ADE_BAD_SEEK;
                    else if (!SetEndOfFile(hFile))
                        iErr = ADE_BAD_WRITE;
                    else if (SetFilePointer(hFile, nHead, NULL, FILE_BEGIN) != (DWORD)nHead)
                        iErr = ADE_BAD_SEEK;
                    else                     /* If that went OK, off we go */
                        iErr = doit(hFile, sHand, nBytes, szChans,
                                    szClk, div1, div2, pfCancel, nBuff, counter, 
									&ibutton, sec);

                    if ((iErr > 0) && (iErr < nBytes))
                    {
                        offset = iErr  + nHead;/* The size wanted */
                        if (SetFilePointer(hFile, offset, NULL, FILE_BEGIN) != (DWORD)offset)
                            iErr = ADE_BAD_SEEK;
                        else if (!SetEndOfFile(hFile))
                            iErr = ADE_BAD_WRITE;
                    }
                }
                CloseHandle(hFile);       /* Tidy up, first close the file */
             //   if (iErr <= 0)
               //    DeleteFile(szFile);        /* and delete after failure */
            }
            else
                iErr = ADE_CREATE_FAILED;

            U14UnSetTransfer(sHand, 0);      /* Free the DMA transfer area */
        }

        free(pHead);                              /* Free allocated memory */
        U14Close1401(sHand);                         /* and close the 1401 */
    }
    else
        iErr = sHand;

#ifdef _DEBUG
    wsprintf(szBuff, "Done, result is %d\n", iErr);
    OutputDebugString(szBuff);
#endif
    *button=ibutton;
    return iErr;
}
