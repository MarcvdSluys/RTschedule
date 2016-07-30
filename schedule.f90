!***********************************************************************************************************************************
program schedule
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, syntax_quit
  use SUFR_dummy, only: dumStr
  use SUFR_text, only: d2s
  
  implicit none
  integer, parameter :: nLines=99
  integer :: status,ip,ln, it,np,pr, ri,ro, time, ti(nLines),ci(nLines),di(nLines),pi(nLines), li(nLines),cc(nLines),tte(nLines)
  integer :: gcd,lcm, optts,majFr
  real(double) :: frac,load
  character :: inFile*(99), name(nLines), ccpr*(9),lipr*(9),ttepr*(9)
  
  call set_SUFR_constants()
  
  if(command_argument_count().ne.1) call syntax_quit('<input file name>', 0, 'Simple scheduling tool for LLF')
  call get_command_argument(1, inFile)
  
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(inFile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(inFile), 1, 1)  ! 1: input file, 1: status: not ok
  
  li = 0
  ro = 1
  cc = 0
  
  ! Read file header:
  do ln=1,1
     read(ip,'(A)') dumStr
  end do
  
  ! Read file body:
  read(ip,*) dumStr, time
  print*,time
  write(*,'(9A5)') 'name', 'ti','ci','di','pi'
  do ln=1,nLines
     read(ip,*,iostat=status) name(ln), ti(ln),ci(ln),di(ln),pi(ln)
     if(status.lt.0) exit
     if(status.gt.0) call file_read_error_quit(trim(inFile), ln, 0)
     write(*,'(A5, 9I5)') name(ln), ti(ln),ci(ln),di(ln),pi(ln)
  end do  ! ln
  close(ip)
  np = ln - 1
  write(*,'(2x,2(I0,A))') np, ' lines (processes) read; scheduling for ', time,' time units.'
  write(*,*)
  
  
  ! Print system load:
  write(*,'(A)', advance='no') '  System load: '
  load = 0.d0
  do pr=1,np
     frac = dble(ci(pr))/dble(pi(pr))
     load = load + frac
     write(*,'(A)', advance='no') d2s(frac,4)
     if(pr.lt.np) write(*,'(A)', advance='no') ' + '
  end do
  write(*,'(A)') ' = '//d2s(load,4)
  if(load.gt.1.d0) then
     write(*,'(A)') '  The system is NOT schedulable indefinately... :-('
  else
     write(*,'(A)') '  The system is SCHEDULABLE! :-)'
  end if
  write(*,*)
  
  optts = gcd(np, ci(1:np))
  majFr = lcm(np, pi(1:np))
  write(*,'(A,I0,A)') '  Optimal timeslice: ', optts, ' time units'
  write(*,'(A,I0,A)') '  Major frame: ', majFr, ' time units'
  write(*,'(A,I0,A)') '  Minor frame: ', gcd(np, pi(1:np)), ' time units'
  write(*,*)
  
  ! Initial computation times and laxities:
  cc = ci
  li = di - ci
  
  
  do it=1,time  ! Note: this is the time unit that ENDS at t=ti
     
     ! Print timestamp:
     write(*,'(2x,I0,A,I0,T9,A)', advance='no') it-1,'-',it, ''
     
     ! Print detailed data:
     do pr=1,np
        !write(*,'(2x,3I3)', advance='no') cc(pr), li(pr), tte(pr)
        
        write(ccpr,'(I0)')  cc(pr)
        if(cc(pr).eq.0) then
           lipr = '-'
           ttepr = '-'
        else
           write(lipr,'(I0)')  li(pr)
           write(ttepr,'(I0)') tte(pr)
        end if
        
        
        !write(*,'(2x,3A3)', advance='no') ccpr, lipr, ttepr
        write(*,'(2x,A3)', advance='no') lipr
     end do
     !li = 1
     
     ri = minval( minloc(li(1:np), cc(1:np).gt.0) )  ! Running task: minimum li and cc>0
     if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. li(ro).le.li(ri)) ri = ro  ! Keep the old task running if laxities are equal
     
     !write(*,'(5x,3I3)', advance='no') minval(li(1:np)), ri,ro
     write(*,'(5x,2(A,I0))', advance='no') 'run: ',ri, ', lax: ', li(ri)
     if(ri.ne.ro) write(*,'(2x,A)', advance='no') 'switch'
     
     ! Current running job is ci, all other laxities decrease:
     do pr=1,np
        if(pr.eq.ri) then
           cc(pr) = cc(pr) - 1
        else
           li(pr) = li(pr) - 1
        end if
     end do
     
     
     
     do pr=1,np
        tte(pr) = mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr))  ! Time to next event
        if(tte(pr).eq.0) then
           cc(pr) = ci(pr)
           li(pr) = di(pr) - ci(pr)
        end if
     end do
     
     if(minval(li(1:np)).lt.0) then  ! Deadline missed
        write(*,'(//,A,I0,A)', advance='no') '  At t=',it,', a deadline has been missed for process'
        do pr=1,np
           if(li(pr).lt.0) write(*,'(A)', advance='no') ' '//name(pr)
        end do
        write(*,'(A,/)') ', while process '//name(ri)//' is running.'
        stop
     end if
     
     ro = ri
     write(*,*)
  end do
  
  write(*,'(/,A,I0,A)') '  The system can be scheduled for ', time, ' time units.'
  write(*,*)
end program schedule
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief Computes the greatest common divisor (GCD) of two positive integers
!!
!! This function uses the Euclid method.  Given a and b, a >= b, the Euclid method goes as follows:
!! - (1) dividing a by b yields a remainder c
!! - (2) if c is zero, b is the GCD
!! - (3) if c is non-zero, b becomes a and c becomes c and go back to step (1).  This process will continue until c is zero.
!!
!! \see http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap04/gcd.html

function gcd2(a,b)
  implicit  none
  integer, intent(in) :: a, b
  integer :: gcd2, la,lb,lc
  
  ! Don't change the input parameters:
  la = a
  lb = b
  
  if(la.lt.lb) then       ! since a >= b must be true, they
     lc = la              ! are swapped if a < b
     la = lb
     lb = lc
  end if
  
  do                      ! now we have a <= b
     lc = mod(la, lb)     !    compute c, the reminder
     if (lc == 0) exit    !    if c is zero, we are done.  gcd = b
     la = lb              !    otherwise, b becomes a
     lb = lc              !    and c becomes b
  end do                  !    go back
  
  gcd2 = lb
  
end function gcd2
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief Computes the greatest common divisor (GCD) for an array of positive integers
!!

function gcd(size, array)
  implicit none
  integer, intent(in) :: size,array(size)
  integer :: gcd, gcd2, it
  
  gcd = maxval(array)
  do it=1,size
     gcd = gcd2(array(it),gcd)
  end do
  
end function gcd
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief Computes the least common multiplier (LCM) of an array of positive integers
!!
!! \see https://en.wikipedia.org/wiki/Least_common_multiple#A_simple_algorithm

function lcm(size, array)
  implicit none
  integer, intent(in) :: size,array(size)
  integer :: lcm, larray(size), in
  
  larray = array
  do
     if(minval(larray).eq.maxval(larray)) exit  ! All values are equal, we have finished!
     
     in = minval(minloc(larray))  ! Index of the (first) smallest value in the array
     larray(in) = larray(in) + array(in)
  end do
  
  lcm = larray(1)
end function lcm
!***********************************************************************************************************************************

