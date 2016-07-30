!***********************************************************************************************************************************
program schedule
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, syntax_quit
  use SUFR_dummy, only: dumStr
  use SUFR_text, only: d2s
  use SUFR_numerics, only: gcd,lcm
  
  implicit none
  integer, parameter :: nLines=99
  integer :: status,ip,ln, it,np,pr, ri,ro, time, ti(nLines),ci(nLines),di(nLines),pi(nLines), li(nLines),cc(nLines),tte(nLines)
  integer :: optts,majFr, run(nLines)
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
  write(*,*)
  read(ip,*) dumStr, time
  write(*,'(9A5)') 'Name', 'ti','ci','di','pi'
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
  
  optts = gcd(ci(1:np))
  majFr = lcm(pi(1:np))
  write(*,'(A,I0,A)') '  Optimal timeslice: ', optts, ' time units'
  write(*,'(A,I0,A)') '  Major frame: ', majFr, ' time units'
  write(*,'(A,I0,A)') '  Minor frame: ', gcd(pi(1:np)), ' time units'
  write(*,*)
  
  ! Initial computation times and laxities:
  cc = 0
  li = 0
  do pr=1,np
     if(ti(pr).eq.0) then
        cc(pr) = ci(pr)
        li(pr) = di(pr) - ci(pr)
     end if
  end do
  
  do it=1,time  ! Note: this is the time unit that ENDS at t=ti
     
     ! Print timestamp:
     write(*,'(2x,I0,A,I0,T9,A)', advance='no') it-1,'-',it, ''
     
     
     ! Determine running task:
     ri = minval( minloc(li(1:np), cc(1:np).gt.0) )  ! Running task: minimum li and cc>0
     if(ri*ro.ne.0) then
        if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. li(ro).le.li(ri)) ri = ro  ! Keep the old task running if laxities are equal
     end if
     run(it) = ri
     
     ! Print detailed data:
     do pr=1,np
        
        write(ccpr,'(I0)')  cc(pr)
        if(cc(pr).eq.0) then
           lipr = '-'
           ttepr = '-'
        else
           write(lipr,'(I0)')  li(pr)
           write(ttepr,'(I0)') tte(pr)
        end if
        
        
        if(pr.eq.ri) then
           lipr = '_'//trim(lipr)//'_'
        else
           lipr = ' '//trim(lipr)
        end if
        write(*,'(3x,A4)', advance='no') lipr
        if(it.ge.ti(pr) .and. tte(pr).eq.0) then  ! New event
           write(*,'(A)', advance='no') 'e'
        else
           write(*,'(A)', advance='no') ' '
        end if
        
     end do
     
     
     ! Print which task is running + its laxity:
     if(ri.eq.0) then  ! No task is running
        write(*,'(5x,A)', advance='no') 'run: -,  lax: -,  cpu: -'
     else
        write(*,'(5x,3(A,I0))', advance='no') 'run: ',ri, ',  lax: ', li(ri), ',  cpu: ', cc(ri)
     end if
     if(ri.ne.ro) then
        write(*,'(2x,A)', advance='no') 'switch'
        if(ro.ne.0) then
           if(cc(ro).gt.0) write(*,'(A,I0,A)', advance='no') ' (', cc(ro), '>)'
        end if
     end if
     
     ! Current running job is ci, all other laxities decrease:
     do pr=1,np
        if(pr.eq.ri) then
           cc(pr) = cc(pr) - 1
        else
           if(it.ge.ti(pr)) li(pr) = li(pr) - 1
        end if
     end do
     
     
     ! New event:
     do pr=1,np
        tte(pr) = mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr))  ! Time to next deadline
        !tte(pr) = mod( ti(pr)-it + pi(pr)*1000, pi(pr))  ! Time to next event
        if(tte(pr).eq.0) then        ! New event occurs
           cc(pr) = ci(pr)           ! Reset the computation time 
           li(pr) = di(pr) - ci(pr)  ! Reset the laxity
        end if
     end do
     
     ! Deadline missed:
     if(minval(li(1:np)).lt.0) then
        write(*,'(//,A,I0,A)', advance='no') '  At t=',it,', a deadline has been missed for process'
        do pr=1,np
           if(li(pr).lt.0) write(*,'(A)', advance='no') ' '//name(pr)
        end do
        write(*,'(A,/)') ', while process '//name(ri)//' is running.'
        stop
     end if
     
     ro = ri
     write(*,*)
  end do  ! it
  
  
  ! 'Plot' an ascii scheduler:
  write(*,*)
  do pr=1,np
     write(*,'(A4,3x)', advance='no') name(pr)
     do it=1,time
        
        ! Mark runtime:
        if(run(it).eq.pr) then
           write(*,'(A)', advance='no') '#'
        else
           write(*,'(A)', advance='no') ' '
        end if
        
        ! Mark event/deadline:
        if( mod( ti(pr)-it + pi(pr)*1000, pi(pr)).eq.0 ) then  ! Next event
           write(*,'(A)', advance='no') 'e'
        else if( mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  then ! Next deadline != event
           write(*,'(A)', advance='no') 'd'
        else
           write(*,'(A)', advance='no') ' '
        end if

     end do  ! it
     write(*,*)
  end do  ! pr
  
  write(*,'(A4,I3)', advance='no') 't',0
  do it=1,time
     if(mod(it,5).eq.0) write(*,'(I10)', advance='no') it
  end do
  write(*,*)
  
  
  
  write(*,'(/,A,I0,A)') '  The system can be scheduled for ', time, ' time units.'
  write(*,*)
end program schedule
!***********************************************************************************************************************************

