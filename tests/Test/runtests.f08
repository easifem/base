program runtests
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   integer i, argstatus, arglen
   character(len=120) testprogram
   integer total_tests, total_skipped
   integer total_todos, total_unexptodos
   integer total_planned, total_failed
   integer ntests, nskipped, ntodos, nunexptodos, nplanned, nfailed, nfiles
   real t1, t2, time

   ! TODO: (1) Collect test program names, and line them up nicely with dots;
   !       (2) Read test stream from input_unit by default on 0 arguments;
   !       (3) Show tests or other options?

   total_tests = 0; total_todos = 0; total_unexptodos = 0;
   total_skipped = 0; total_planned = 0; total_failed = 0
   time = 0.0
   nfiles = command_argument_count()
   call cpu_time(t1)
   do i = 1, nfiles
      call get_command_argument(i, testprogram, &
                              & length=arglen, status=argstatus)
      if (argstatus /= 0) then
         if (argstatus < 0) then
            write (error_unit, '("runtests: ")', advance="NO")
            write (error_unit, '("Argumment no. ",I0," is too long.")') i
            write (error_unit, '("          ")', advance="NO")
            write (error_unit, '("It must be <= 120 characters long.")')
            error stop
         else
            write (error_unit, '("runtests: ")', advance="NO")
            write (error_unit, '("Cannot retrieve argument no. ",I0)') i
            write (error_unit, '("          ")', advance="NO")
            write (error_unit, '("Argument status is ",I2)') argstatus
            error stop
         end if
      end if
      call runtest(testprogram)
   end do
   call cpu_time(t2)
   time = t2 - t1

   ! Delete all test output
   call execute_command_line("del *.testoutput")   ! DOS
   call execute_command_line("rm -f *.testoutput") ! UNIX
   ! ...

   write (*, '("--------------------------------------------------")')
   write (*, '("      Files:    ",I0)') nfiles
   write (*, '("      Planned:  ",I0," (ran ",I0,")")') &
         & total_planned, total_tests
   write (*, '("      Skipped:  ",I0)') total_skipped
   write (*, '("      Todos:    ",I0," (",I0," unexpectedly passed)")') &
         & total_todos, total_unexptodos
   write (*, '("      Failed:   ",I0)') total_failed
   write (*, '("      OK tests: ",I0," (",I0," - ", I0, " - ", I0, ")")') &
         & total_tests - total_failed - total_skipped, & 
         & total_tests, total_failed, total_skipped
   write (*, '("      Time:     ",G0," seconds")') time
   write (*, '("--------------------------------------------------")')

   if (total_failed > 0) then
      write (*, '("Result: FAIL")')
   else
      write (*, '("Result: PASS")')
   end if
   stop

contains

   subroutine runtest(testprogram)
      character(len=*), intent(in) :: testprogram
      character(len=:), allocatable :: t, testoutput
      integer stat

      allocate(t, source=trim(testprogram))
      allocate(testoutput, source=t//".testoutput")
      call execute_command_line("./" // t // " > " // testoutput, &
                              & wait=.true., exitstat=stat)
      ! '>>' appends output from a command to a specified file.
      ! It works the same in both MS-DOS, PC-DOS, Unix, and Mac OS X.
      ! If the filename does not exist, it is created.
      if (stat /= 0) then
         write (*, '("ERROR! Failed to run: ",A)') t
      else
         call analyze(t, testoutput)
      end if
   end subroutine runtest

   subroutine analyze(testprogram, testoutput)
      character(len=*), intent(in) :: testprogram, testoutput
      character(len=200) testline
      character(len=30) testprogram_dots
      character(len=:), allocatable :: msg
      integer stat

      testprogram_dots = repeat(".", len(testprogram_dots))
      testprogram_dots(1:min(len(testprogram), 30)) = testprogram
      write (*, '(A, ": ")', advance="NO") testprogram_dots

      ntests = 0; ntodos = 0; nunexptodos = 0
      nskipped = 0; nplanned = 0; nfailed = 0
      allocate(msg, source=" ")
      open(10, file=testoutput, status='OLD', iostat=stat, iomsg=msg)
      if (stat /= 0) then
         write (*, *) "ERROR! ", msg
         close(10)
         return
      end if

      do
         read (unit=10, fmt='(A200)', iostat=stat, iomsg=msg) testline
         if (is_iostat_end(stat)) exit
         if (stat > 0) then ! iostat error
            msg = "ERROR READING TESTLINE! " // msg
            exit
         end if
         call analyze_line(testline, msg)
         if (msg /= "") exit ! Early exit, e.g. no plan
      end do 

      close(10)

      if (msg /= "") then
         write (*, *) msg
      else
         if (nfailed > 0) write (*, '(A)', advance="NO") "not "
         write (*, '(A)', advance="NO") "ok"

         if (nplanned /= ntests) then
            write (*, '(A,I0,A,I0)', advance="NO") &
            & "; mismatch of planned and runned tests - planned ", &
            & nplanned, ", but ran ", ntests
         end if
         if (nfailed > 0) then
            write (*, '("; ",I0,"/",I0," failed")', advance="NO") &
                     & nfailed, ntests
         end if
         if (nunexptodos > 0 .or. ntodos > 0) then
            write (*, '("; ",I0,"/",I0," todos")', advance="NO") &
                     & ntodos - nunexptodos, ntodos
         end if
         if (nskipped > 0) then
            write (*, '("; ",I0," skipped")', advance="NO") nskipped
         end if
!         if (nunexptodos > 0) &
!            & write (*, '(A,I0,A)', advance="NO") "; ", nunexptodos, &
!            & " todos unexpectedly passed"
!         if (nskipped > 0) &
!            & write (*, '(A,I0,A)', advance="NO") "; ", nskipped, " skipped"
!         if (ntodos > 0) &
!            & write (*, '(A,I0,A)', advance="NO") "; ", ntodos, " todos"
         write (*, *) ""
      end if

      total_tests = total_tests + ntests
      total_todos = total_todos + ntodos
      total_unexptodos = total_unexptodos + nunexptodos
      total_skipped = total_skipped + nskipped
      total_planned = total_planned + nplanned
      total_failed = total_failed + nfailed
   end subroutine analyze

   subroutine analyze_line(testline, msg)
      character(len=*), intent(in) :: testline
      character(len=:), allocatable, intent(out) :: msg

      if (testline(1:1) == "#") then
         msg = ""
         return
      else if (testline(:9) == "Bail out!") then
         msg = "Bail out!"
         return
      else if (testline(:4) == "1..0") then
         msg = "No plan!"
         if (index(testline, "Skipped") > 0) msg = "Skipped!"
         return
      else if (testline(:3) == "1..") then
         if (nplanned > 0) then
            msg = "Double plan!"
            return
         end if
         read (testline(4:), *) nplanned
      end if

      msg = ""
      if (testline(1:2) == "ok") then
         ntests = ntests + 1
         if (index(testline, "SKIP") > 0) then
            nskipped = nskipped + 1
         else if (index(testline, "TODO") > 0) then
            nunexptodos = nunexptodos + 1
         end if
      else if (testline(1:6) == "not ok") then
         ntests = ntests + 1
         if (index(testline, "SKIP") > 0) then
            nskipped = nskipped + 1
         else if (index(testline, "TODO") > 0) then
            ntodos = ntodos + 1
         else
            nfailed = nfailed + 1
         end if
      end if
   end subroutine analyze_line

end program runtests

