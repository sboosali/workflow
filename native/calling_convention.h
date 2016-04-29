
#ifndef __WINDOWS_CCONV_H
#define __WINDOWS_CCONV_H

#if defined(i386_HOST_ARCH)
# define CALLING_CONVENTION stdcall
#elif defined(x86_64_HOST_ARCH)
# define CALLING_CONVENTION ccall
#else
# define CALLING_CONVENTION ccall
#endif

#endif

