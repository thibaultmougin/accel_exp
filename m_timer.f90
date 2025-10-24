module m_timer
USE OMP_LIB
use,intrinsic :: iso_fortran_env, only : int32,int64,real32,dp=>real64
use,intrinsic :: iso_fortran_env, only : stdout=>OUTPUT_UNIT
implicit none
private

type timer
   real(kind=dp)   :: cpu_start
   real(kind=dp)   :: cpu_end
   integer(kind=int64) :: clock_start
   integer(kind=int64) :: clock_end
   integer             :: wall_start(8)
   integer             :: wall_end(8)
   contains
       procedure  ::  tic        =>  clock_tic
       procedure  ::  toc        =>  clock_toc
       procedure  ::  print      =>  clock_print
       procedure  ::  walltime   =>  clock_walltime
       procedure  ::  cputime    =>  clock_cputime
       procedure  ::  dattime    =>  clock_dattime
end type

interface timer
     procedure :: clock_new
end interface timer

! type for unix epoch time and julian days
integer,parameter,public   :: realtime=dp 

public :: timer
public :: say_hello

character(len=*),parameter :: gen='(*(g0))'
character(len=*),parameter  :: all='(*(g0,1x))'

contains

! initialization constructor
type(timer) function clock_new(this)
type(timer),intent(in),optional :: this

   call cpu_time(clock_new%cpu_start)
   call system_clock(clock_new%clock_start)
   call date_and_time(values=clock_new%wall_start)

   clock_new%cpu_end= clock_new%cpu_start
   clock_new%clock_end= clock_new%clock_start
   clock_new%wall_end= clock_new%wall_start

end function clock_new

subroutine clock_tic(this)
class(timer) :: this

   call cpu_time(this%cpu_start)
   call system_clock(this%clock_start)
   call date_and_time(values=this%wall_start)

   this%cpu_end   = this%cpu_start
   this%clock_end = this%clock_start
   this%wall_end  = this%wall_start

end subroutine clock_tic

subroutine clock_toc(this)
class(timer) :: this

   call cpu_time(this%cpu_end)
   call system_clock(this%clock_end)
   call date_and_time(values=this%wall_end)

end subroutine clock_toc

subroutine clock_print(this,string,lun)
class(timer),intent(in)                  :: this
character(len=*),intent(in),optional     :: string
integer(kind=int32),intent(in),optional  :: lun
integer(kind=int32)                      :: lun_
real(kind=dp)                            :: elapsed_time
real(kind=realtime)                      :: elapsed_date_and_time
real(kind=dp)                            :: cpu_time
character(len=105)                       :: biggest
integer(kind=int64)                      :: count_rate

   if(present(lun))then
      lun_=lun
   else
      lun_=stdout
   endif

   elapsed_time           =  this%walltime()
   elapsed_date_and_time  =  this%dattime()
   cpu_time               =  this%cputime()

   if(present(string)) write( lun_,gen ) string

   if(elapsed_date_and_time >= 0)then
      write( lun_,'(a,f0.3)')    'Elapsed dat  (sec) ::',elapsed_date_and_time
   else
      write( lun_,'(a)')         'Elapsed dat  (sec) :: N/A'
   endif

   ! try to make a reasonable format for the number of digits of precision
   call system_clock(count_rate=count_rate) ! Find the time rate
   write(biggest,'("(a,f0.",i0,")")')ceiling(log10(real(count_rate,kind=dp)))

   write( lun_,biggest)          'Elapsed time (sec) ::',elapsed_time
   write( lun_,gen)              'CPU time     (sec) ::',cpu_time
   write( lun_,'(a,1x,f0.2)')    'Percentage         ::',(cpu_time/elapsed_time)*100

end subroutine clock_print

function clock_walltime(this) result(elapsed_time)
class(timer)        :: this
integer(kind=int64) :: count_rate
real(kind=dp)       :: elapsed_time
real(kind=dp)       :: cpu_time
   call system_clock(count_rate=count_rate) 
   elapsed_time = real(this%clock_end-this%clock_start,kind=dp)/real(count_rate,kind=dp)
end function clock_walltime

function  clock_cputime(this)  result(cpu_time)
class(timer)         :: this
real(kind=dp)        :: cpu_time
   cpu_time = real(this%cpu_end-this%cpu_start,kind=dp)
end function clock_cputime

function  clock_dattime(this)  result(cpu_time)
class(timer)         :: this
real(kind=dp)        :: cpu_time
real(kind=realtime)  :: endit,startit
integer              :: ierr
   call date_to_julian(this%wall_end,endit,ierr)
   call date_to_julian(this%wall_start,startit,ierr)
   if(ierr == 0)then
      cpu_time = real((endit-startit)*86400,kind=dp)
   else
      cpu_time = -huge(cpu_time)
   endif
end function clock_dattime

subroutine date_to_julian(dat,julian,ierr)
! @(#)M_time::date_to_julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
! correction for time zone should or should not be included?
integer,intent(in)               ::  dat(8)! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  ::  julian
integer,intent(out) :: ierr ! 0 =successful, -1=bad year, -4=bad date 29 Feb, non leap-year, -6 negative value -9 bad input
integer                          ::  a , y , m , jdn
integer                          ::  utc
utc=dat(4)*60
julian = -huge(99999)               ! this is the date if an error occurs and IERR is < 0
if(any(dat == -huge(dat)))then
   ierr=-9
   return
endif
associate&
&(year=>dat(1),month=>dat(2),day=>dat(3),utc=>utc,hour=>dat(5),minute=>dat(6),second=>dat(7)-utc+dat(8)/1000.0d0)
   if ( year==0 .or. year<-4713 ) then
      ierr = -1
   else
      ierr=0
   !  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
      a = (14-month)/12    ! A will be 1 for January or February, and 0 for other months, with integer truncation
      y = year + 4800 - a
      m = month + 12*a - 3 ! M will be 0 for March and 11 for February
   !  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
   !  Convert to a negative number, then increment towards zero
   !  Staring from a Gregorian calendar date
      jdn = day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - 32045 !  with integer truncation
   !  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
      julian = jdn + (hour-12)/24.0_realtime + (minute)/1440.0_realtime + second/86400.0_realtime
      ierr=merge(-6,ierr, julian<0.0_realtime ) ! Julian Day must be non-negative
   endif
end associate
end subroutine date_to_julian

subroutine say_hello()
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
character(len=*),parameter :: all='(*(g0,1x))'
character(len=*),parameter :: chs='(*(g0))'
character(len=2)           :: ch, split
integer                      :: argument_length, istat, posix, dos, i
character(len=:),allocatable :: progname, options
   call get_command_argument(number=0,length=argument_length)
   if(allocated(progname))deallocate(progname)
   allocate(character(len=argument_length) :: progname)
   call get_command_argument (0, progname, status=istat)
   print all, 'run date.....:',iso_8601()
   if (istat == 0) then
      print all, "program name.:" // trim (progname)
   else
      print all, "Could not get the program name " // trim (progname)
   endif
   print all, 'compiled by..:', compiler_version()
   options=' '//compiler_options()
   if(options /= '')then
      print all, 'using options:'
      ! guess which one
      posix=0
      dos=0
      do i=2,len(options)
         ch=options(i-1:i)
         select case(ch)
         case(' -');    posix=posix+1
         case(' /');    dos=dos+1
         end select
      enddo
      split=merge(' -',' /',posix > 0)
      do i=2,len(options)
         ch=options(i-1:i)
         if(ch == split)then
            write(*,chs,advance='no')char(10),ch
         else
            write(*,chs,advance='no')ch(2:2)
         endif
      enddo
      print all
   endif
   print all
end subroutine say_hello

function iso_8601()
! return date using ISO   8601 format at a resolution of seconds
character(len=8)  :: dt
character(len=10) :: tm
character(len=5)  :: zone
character(len=25) :: iso_8601
   call date_and_time(dt, tm, zone)
   ISO_8601 = dt(1:4)//'-'//dt(5:6)//'-'//dt(7:8)   &
      & //'T'//                                     &
      & tm(1:2)//':'//tm(3:4)//':'//tm(5:6)         &
      & //zone(1:3)//':'//zone(4:5)
end function iso_8601

end module m_timer