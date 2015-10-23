Welcome to ALS Prolog!
======================

Contents:  
--------
   - LICENSE  
   - WARRANTY
   - DISTRIBUTION ORGANIZATION  
   - MANUAL  
   - HELP SYSTEM  
   - COPYING ALS

LICENSE
=======
See the file LICENSE.txt.

WARRANTY
========
This program is provided "as is " without warranty of any kind, 
either expressed or implied, including, but not limited to, 
implied warranties of merchantability and fitness for a 
particular purpose.  You assume the entire risk as to the 
selection of this program to achieve your intended results and 
for the installation, use, performance, and results obtained 
through the use of this program.

DISTRIBUTION ORGANIZATION
=========================
The ALS Prolog distribution contains the following files:

     ????ALS Prolog 3.1 Installer????
	 LICENSE.txt
	 README.md - this file 

On Macintosh and Windows, ALS Prolog 3.1 Installer is a standard
system installer (on Windows, it is ALS Prolog 3.1 Installer.EXE).
Simply double-click on the icon for

	ALS Prolog 3.1 Installer, 

and follow the simple directions to install ALS Prolog on your 
hard drive.

On Unix, ALS Prolog 3.1 Installer is a gzipped tar file whose name
is of the form

	als-prolog-<ARCH>.tgz

where <ARCH> is your target OS (e.g., solaris, hpux, irix, linux).
Choose the directory in which you want to install ALS Prolog, and
copy als-prolog-<ARCH>.tgz to that directory.  Then perform:

	gzip -d als-prolog-<ARCH>.tgz
	tar -xf ./als-prolog-<ARCH>.tar

If you do not have gzip on your system, a copy is included on the
CD for your convenience.

After installation on Unix, set your search paths appropriately.

MANUAL
======
The User Manual for the standard edition of ALS Prolog is provided
by the file

    ALS Prolog Manual.pdf

in the installation directory for ALS Prolog.  This is an Adobe 
Acrobat(r) file.  

To read the ALS Prolog manual:  
On Windows and Macintosh, simply double-click on the icon for

    	ALS Prolog Manual.pdf

On Linux, execute

		acroread

and then open als-prolog-manual.pdf.  If <MP> is a path to the directory containing the manual, then executing

		acroread <MP>/als-prolog-manual.pdf

will start the Acrobat Reader and automatically load the manual.

HELP SYSTEM
===========

The Reference Manual for ALS Prolog is implemented as an HTML-based HELP system.
You will need fairly recent versions of the Netscape or Internet Explorer browsers
to read the help files, as follows:

	Macintosh:  Netscape Navigator 4.0+
	---------   { IE exibits difficulties on Macintosh }

	Windows:    Netscape Navigator 4.06+
	-------     Internet Explorer 4.0+

	Unix:       Netscape Navigator 4.07+
	----

After the installation is complete, the ALS Prolog HELP system is contained in the

	help

directory inside the installation directory.  It consists of a large number of
*.htm and *.gif files, together with a number of additonal files.  In addition,
there is one file

	als_help.htm 

in the installation directory.  To view the ALS Prolog Help system, simply open the 
file

	als_help.htm

in your browser.  PLEASE NOTE that the start-up of the help system is rather
slow, since the browsers must fire up Java and other components.  However,
once the contents for the system have appeared, it performs quite nicely
on most platforms.

   = = = = = = = = = = = = = = = = = = = = = = = = = = = =

       Applied Logic Systems, Inc.
       PO Box 468, Vanderwagen, NM USA
	   Web: http://www.applied-logic-systems.com  
