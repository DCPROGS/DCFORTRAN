subroutine HJCDATw(idatyp,nfile,kfile,pfile,nval,irecs,calfacs2,&
     nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
     name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
     qfile2,adcfil,nfileb,pfileb,npatch,defname,samexp,&
     cjump,nsweep,tzero,tsamp)




	integer*4 jstrec(200)
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	character ndev*2,name*12,ans*1,UC*1,expdate*11,title*70
	character tapeID*24,expnum*9

	real*4 conc(10,10)
	character*20 ligname(10)
	logical contig,onedisk
	integer kfile(20,10),nfile(10),nfileb(10),nintt(10)
	character*33 pfile(20,10),pfileb(20,10)	!path names for .SCN files
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	character*14 ptype(5)
	LOGICAL debug,caplock,present
	logical sbin,shist,sres,sexp,samexp
	logical discprt,readini,cjump

	real*4 avamp(10),rms(10),fc(10),ffilt(10),trise(10)

	real*8 dfinter,tlast
	real*8 tzerod,tsample
	logical invert,opendown,newpar,sdone
	logical disp,usepots,simulat
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	character ADCFIL*30,cdate*11,cdate1*11,filtfile*20
	character*1 qfile1*35	!as in scan.dat
	character*1 qfile*40	!as in vplot and vhist
	character*1 qfile2*40	!use input -ignore value from disk!
	character adctime*8
	character defname*6
	character cnum1*11,cnum2*11

	character path*30,path1*30,pname*8,suffix*3	!for parsname
	logical nopath

	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM

	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'


	