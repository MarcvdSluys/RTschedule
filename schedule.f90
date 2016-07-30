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
  real(double) :: frac,load
  character :: inFile*(99), name(nLines)
  
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
  
  
  ! Initial computation times and laxities:
  cc = ci
  li = di - ci
  
  
  do it=1,time  ! Note: this is the time unit that ENDS at t=ti
     write(*,'(2x,I0,A,I0,T9,A)', advance='no') it-1,'-',it, ''
     
     do pr=1,np
        write(*,'(2x,3I3)', advance='no') cc(pr), li(pr), tte(pr)
     end do
     !li = 1
     
     ri = minval( minloc(li(1:np), cc(1:np).gt.0) )  ! Running task: minimum li and cc>0
     if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. li(ro).le.li(ri)) ri = ro  ! Keep the old task running if laxities are equal
     
     write(*,'(5x,3I3)', advance='no') minval(li(1:np)), ri,ro
     
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
