subroutine read_other(testfil,title,ilen,inchan,calfac,pAV,srate,ffilt,iver,ioff)

integer nFileVersion
integer*2   inchan,iver
character*20 sAllHardwareSettings
real*8 fTimePerPoint
logical bDataAvailable
character*60 testfil,title
logical discprt
ALLOCATABLE::data8

real*8 data8(:)
common/dp/discprt

inchan=0
calfac=0.
pav=0.
srate=0.
ffilt=0.
iver=0
ioff=0
OPEN(unit=14,file=testfil,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1		)
read(14,rec=1) nfileversion,nNumberOfActiveInstruments,sAllHardwareSettings,nCurveLength,&
    nSweepsToAverage,nReadoutsPerStep,fTimePerPoint,nTPPAutomatic,nFileHasData
allocate(data8(100))
read(14,rec=8001) (data8(i), i=1,100)     
close(unit=14)
deallocate(data8)      
end