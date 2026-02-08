! This module is mainly taken from the source:
! https://github.com/urbanjost/M_system.
! The Author's name is John S. Urban
!
! The original name of the program has been changed
! from M_SYSTEM to System_Method.
!
! The routine is divided into Modules and Submodules.
!
! This is to confirm to the coding sytles of easifem.
! Original program has been re-organized into module and submodule.
! If you are using easifem for getting methods defined in this
! module, then please use M_System module by using the above link.
! We would like to thank the original author Urban Jost for creating
! This useful module.

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-07
! summary: Fortran interface to C system interface
!
!# System_Method
!
! M_system(3fm) is a collection of Fortran procedures that call C
! or a C wrapper using the ISO_C_BINDING interface to access system calls.
! System calls are a special set of functions used by programs to communicate
! directly with an operating system.
!
! Generally, system calls are slower than normal function calls because
! when you make a call control is relinquished to the operating system
! to perform the system call. In addition, depending on the nature of the
! system call, your program may be blocked by the OS until the system call
! has finished, thus making the execution time of your program even longer.
!
! One rule-of-thumb that should always be followed when calling a system
! call -- Always check the return value.

MODULE System_Method
USE SystemOptions
USE SystemInterface
USE SystemSignal_Method
USE SystemFile_Method
USE SystemEnvironment_Method
USE SystemEnquiry_Method
USE SystemProcess_Method
END MODULE System_Method
