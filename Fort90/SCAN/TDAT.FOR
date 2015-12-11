      program tdat
c To test read/write of jumps/kjumps to scandat.dat
	integer*2 itest(5000)
	character name*12,name1*12,expdate*11,title*70
	character tapeID*24,expnum*9,ch*1
	character*14 ptype(5),expdate1*11,title1*70,tape1*24
      logical cjump
c
      cjump=.true.
c
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'

      print 3
3     format('(1) Read integer*2 array; (2) Read header.  i = ')
	call INPUTi(i)
	OPEN(unit=15,file='a:\scandat.dat',status='unknown',
     & access='TRANSPARENT')
      if(i.eq.2) goto 10
c
	nread=10
      print 1
1     format(' Read n integers: n [10] = ')
	call INPUTi(nread)
	nbyte=2*nread
      print 11
11    format(' Start read at byte # n1 = ')
	n1=1
	call INPUTi(n1)
      read(15,rec=n1) (itest(i),i=1,nread)
      do i=1,nread
         print 2,i,itest(i)
2        format(' itest(',i4,') = ',i6)
      end do
      goto 99
c
10    continue
      print 4
4     format('Start at byte # n1 = ')
	call INPUTi(n1)
	read(15,rec=n1)expnum,ifile1,kt,iscan,expdate,title,
     &    tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     &    imin,imax,cjump,nsweep,tzero,tsamp

         print 38,ifile,kt,'consam.dat   ',expdate,title,
     & tapeID,ptype(ipatch),Emem,temp,ffilt,avamp,rms,calfac2
38		format(
     &  ' Experiment file ',i4,': ',i6,' transitions from ',a33,
     & '; ',a11,/,
     & ' Title: ',a70,/,
     & ' Tape details: ',a24,4x,
     & ' Patch type: ',a14,4x,/,
     & ' Membrane potential (mV) = ',f9.2,4x,
     & ' Temperature = ',f9.1,/,
     & ' Filter (kHz, -3 dB) = ',f12.5,4x,
     & ' Full amplitude (pA) (approx) = ',f9.2,/,
     & ' RMS noise (pA) = ',f10.4,4x,
     & ' Calibration (amplitude to pA) = ',g13.6)
	   if(cjump) then
	      print 20,nsweep,tsamp,tzero
20		format(' CJUMP data:',/,
     &   1x,i6,' jumps of ',f10.1,' ms (t=0 at ',f7.2,' ms)')
	   endif
99    continue
	CLOSE(unit=15)
c
      end

