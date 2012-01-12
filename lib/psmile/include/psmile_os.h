#ifndef __PRISM_OS_INCLUDED

#define __PRISM_OS_INCLUDED

#undef __NO_4BYTE_REALS
#undef __NO_4BYTE_CMPLX

#ifdef __SXdbl4

#define __NO_4BYTE_REALS
#define __NO_4BYTE_CMPLX

#endif

#ifdef __crayx1

#define  __NO_4BYTE_REALS
#define  __NO_4BYTE_CMPLX
#define  __no_8byte_integers

#endif 

#endif
