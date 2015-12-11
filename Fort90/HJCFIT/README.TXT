USE OF QGEN, SCSIM and HJCFIT
===============================

(1) .CML files
  I have sent some .CML files to aid with compiling all the necessary
 subroutines; e.g. \F77L3\BIN\F77L3 &HJCFIT.CML compiles all the files
 listed in HJCFIT.CML.  I use the batch file COMF.BAT so I can just
 type COMF HJCFIT.  Remember to compile with /Q1 as in earlier email.

rem COMF.BAT.
\F77L3\BIN\F77L3 @%1.CML

(2) LIBRARIES
	I have sent all the current libraries that I am using.  I
haven't sent all the sources at the moment, but will do so if necessary.
I have made a directory structure on your disk which corresponds with
the paths specified in the link (.LNK) files

(3) SETUP
	The file WINPRINT.INI is now obsolete -it is now a bit 
different, and is called DCPROGS.INI (winprint sounded like a
Windows program!).  The first time that you run any of these programs
it will create C:\DCPROGS.INI for you, and then ask you to leave and
delete WINPRINT.INI before proceeding.  The initial blue menu now has
a fourth option, 'Set up this computer' -good idea to check your 
set up with this option.

(4) PROGRAMS

FORTRAN\CALC\QGEN
FORTRAN\CALC\SCSIM
FORTRAN\SCAN\SCDIR
	As before, QGEN specifies a model and generates the appropriate
Q matrix for it.  The nature of the models that have been defined are
stored in QMODEL.DAT, and sets of rate constants for them (possibly
more than one set for a given model) are stored in QDAT.DAT.  Certain
defaults for QGEN are stored in QGEN.INI. All three files (QGEN.INI,
QDAT.DAT, QMODEL.DAT) are now kept in the root segment of the drive
where the .EXE reside (they need to be available to several different
progs).  It will be best to always use the 'single channel' option at
the beginning of QGEN -other options will be removed soon as the client
programs (those that read the Q matrix defined in QGEN -e.g. SCSIM) are
gradually being altered to allow concentration, jump parameters etc to
be set in the client program, rather than having to set them in QGEN.

	SCSIM generates simulated data, using a model defined in QGEN,
and puts it into SCANDAT.DAT.  The latter is the new style data file,
which replaces SCDAT.DAT + SCDIR.DAT (there are no longer separate
directory and data files).  The SCANDAT.DAT that I have sent has the
same data in it as the old simulated data that I sent (e.g. HJCFIT can
be tested with files 1-10 = simulated CH82 data with 50 mus resolution
already imposed).  But with the new SCANDAT.DAT you can have any number
of transitions in a file (not limited to 1024 as in old data files) so
new simulated data sets will not need to be split up into many data
files.

	SCDIR is updated version of the old directory-reading program
which reads the directory of data in the new (SCANDAT.DAT) data files.

FORTRAN\HJCFIT\HJCFIT
	The new HJCFIT reads data (real or simulated) from SCANDAT.DAT
The files that are read (and other defaults) are recorded in HJCFIT.INI
(in same directory).  The model and rates are read from QDAT.DAT,
QMODEL.DAT, which are assumed to be in root segment of current drive.
The defaults in the HJCFIT.INI I sent are (1) data, files 1-10 in
L:\SCANDAT.DAT (you can specify any name/path for the data file now),
and (2) model, model 1 with rates from file 3 (this is CH82).
HJCFIT.ICO is an icon I made for running the program from Windows.


