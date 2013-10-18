Notes on NEMO v3.4 Compiler Options
===================================

Information came from 
http://www2.units.it/divisioneisi/ci/tartaglia/intel/fce/copts_for.pdf
and mainly from
http://software.intel.com/sites/products/documentation/doclib/stdxe/2013/composerxe/compiler/fortran-win/GUID-AD753AD3-59A4-4E86-98A1-596CFD029814.htm

Note that FCFLAGS are fortran 90 flags and FFLAGS are fortran 77 flags.

* **-c** Compile or assemble the source files, but do not link. The linking stage simply is not done. The ultimate output is in the form of an object file for each source file. 

* **-C** == -check: Checks for certain conditions at run time.
  - with argument **none** Prevents all run-time checking. 

* **-fpp** Runs the Fortran preprocessor on source files before compilation.

* **-r8** == real_size 64 Makes default real and complex variables 8 bytes long. REAL declarations are treated as DOUBLE PRECISION (REAL(KIND=8)) and COMPLEX declarations are treated as DOUBLE COMPLEX (COMPLEX(KIND=8)). 

* **-O** Specifies the code optimization for applications.
  - **O3** is aggressive optimization.  See second webpage above for the details.... long!

* **-assume** Tells the compiler to make certain assumptions.

  -	**[no]byterecl**	Determines whether units for the OPEN statement	RECL specifier (record length) value in unformatted files are in bytes or longwords (four-byte units). 

* **-convert** Specifies the format of unformatted files containing numeric data.

  -	 **big_endian**
	 Specifies that the format will be big endian for integer data and big endian IEEE floating-point for real and complex data. 

* **-heap-arrays** : This option puts automatic arrays and arrays created for temporary computations on the heap instead of the stack.

  - If heap-arrays is specified and size is omitted, all automatic and temporary arrays are put on the heap. If 10 is specified for size, all automatic and temporary arrays larger than 10 KB are put on the heap.

* **-extend-source** : Specifies the length of the statement field in a fixed-form source file.  If you specify extend_source without size, the statement field ends at column 132

* **-traceback:** Tells the compiler to generate extra information in the object file to provide source file traceback information when a severe error occurs at run time

* **-openmp:** Enables the parallelizer to generate multi-threaded code based on the OpenMP* directives. 

