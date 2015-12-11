/*-----------------------------------------------------------------------

	 															CLIB.H

 -----------------------------------------------------------------------*/

void  Open1401(short *);
void  Close1401(short *);
void  Reset1401(short *);
void  LastErrCode(short *);
void  Ld(char *,char *, short *,short *);
void  SendStr(char *,short *);
void  GetStr(char *[] , long *,short *);
void  To1401(short [],long *,long *,short *,short *);
void  ToHost(short [],long *,long *,short *,short *);
void  BaseAddr1401(long *);
void  LongsFrom1401(long [], short *,short *);
void  TypeOf1401(short *);
void  Stat1401(short *);
void  LdCmd(char *, short *);
void  LineCount(short *);
void  CharCount(short *);
void	SendChar(char , short *);
void  GetChar(char *,short *);
void  SetTimeout(long *);
void  GetTimeout(long *);
void  DriverVersion(long *);
void  KillIO1401(short *);
void  ZeroBlockCount(short *);
void  BlkTransState(short *);
void  StopCircular(short *);
void  StateOf1401(short *);
void  GetUserMemorySize(long *,long *);
void  ByteCount(long *,short *);
void  StrToLongs(char *,long [],short *,short *);
void  OutBufSpace(short *);
void  SetTransfer(int *,short [],long *,short *,short *);
void  UnSetTransfer(long *,short *);

/*------------------------------------------------------------------------*/
