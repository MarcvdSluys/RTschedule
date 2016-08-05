!> \file schedule.f90  Main routines for schedule

!***********************************************************************************************************************************
!> \brief  Module to share plot settings
module plotSettings
  implicit none
  save
  
  integer :: sclType, plSize
  character :: plotType*(19)
  logical :: colour
  
end module plotSettings
!***********************************************************************************************************************************


!***********************************************************************************************************************************
program schedule
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  
  implicit none
  integer, parameter :: nProcMax=19  ! Maximum number of processes to expect
  integer :: np, time, ti(nProcMax),ci(nProcMax),di(nProcMax),pi(nProcMax)
  real(double) :: load
  character :: sched*(9), name(nProcMax)*(9), fileBaseName*(99)
  
  ! Initialise libSUFR:
  call set_SUFR_constants()
  
  ! Read the input file:
  call read_input_file(nProcMax, name, ti,ci,di,pi, np,time, fileBaseName)
  
  ! Print some basic data about the system:
  call print_system_data(np,time, name,ti,ci,di,pi, load)
  
  
  ! Create an RM schedule:
  sched = 'RM'  ! RM algorithm
  !call make_schedule(sched, np,time, name, ti,ci,di,pi, load)
  
  
  ! Create an EDF schedule:
  sched = 'EDF'  ! EDF algorithm
  call make_schedule(sched, np,time, name, ti,ci,di,pi, load, fileBaseName)
  
  
  ! Create a LLF schedule:
  sched = 'LLF'  ! LLF algorithm
  !call make_schedule(sched, np,time, name, ti,ci,di,pi, load)
  
  write(*,*)
end program schedule
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine read_input_file(nProcMax, name, ti,ci,di,pi, np,time, fileBaseName)
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, syntax_quit
  use SUFR_dummy, only: dumStr
  use plotSettings, only: plotType, sclType, plSize, colour
  
  implicit none
  integer, intent(in) :: nProcMax
  integer, intent(out) :: ti(nProcMax),ci(nProcMax),di(nProcMax),pi(nProcMax), np,time
  character, intent(out) :: name(nProcMax)*(9), fileBaseName*(99)
  integer :: ip,ln, status, lastDot
  character :: inFile*(99)
  
  ! See whether a file name was passed on the command line:
  if(command_argument_count().ne.1) call syntax_quit('<input file name>', 0, 'Simple scheduling tool for LLF')
  call get_command_argument(1, inFile)
  
  ! Open the input file:
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(inFile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(inFile), 1, 1)  ! 1: input file, 1: status: not ok
  
  ! Read file header:
  read(ip,'(A)') dumStr
  
  ! Read file body:
  write(*,*)
  read(ip,*) dumStr, plotType
  read(ip,*) dumStr, colour
  read(ip,*) dumStr, sclType
  read(ip,*) dumStr, plSize
  read(ip,*) dumStr, time
  
  ! Read task-list header:
  read(ip,'(A)') dumStr
  read(ip,'(A)') dumStr
  
  ! Read task list:
  do ln=1,nProcMax
     read(ip,*,iostat=status) name(ln), ti(ln),ci(ln),di(ln),pi(ln)
     if(status.lt.0) exit
     if(status.gt.0) call file_read_error_quit(trim(inFile), ln, 0)
  end do  ! ln

  ! Close the file:
  close(ip)
  np = ln - 1  ! Number of processes/tasks found
  
  ! Get the base of the file name:
  fileBaseName = trim(inFile)
  lastDot = index(inFile,'.', back=.true.)  ! Position of the last dot in the file name
  if(lastDot.ne.0) fileBaseName = trim(inFile(1:lastDot-1))  ! Reomve file-name extension
  
end subroutine read_input_file
!***********************************************************************************************************************************
  



!***********************************************************************************************************************************
subroutine print_system_data(np,time, name,ti,ci,di,pi, load)
  use SUFR_kinds, only: double
  use SUFR_text, only: d2s
  use SUFR_numerics, only: gcd,lcm
  
  implicit none
  integer, intent(in) :: np, ti(np),ci(np),di(np),pi(np)
  integer, intent(inout) :: time
  character, intent(in) :: name(np)*(9)
  real(double), intent(out) :: load
  integer :: pr, optts,majFr
  real(double) :: frac
  
  ! Print task list:
  write(*,'(A)') '  **************************************************************************************************************'
  write(*,'(A,T110,A)') '  ***   GENERAL TASK DATA', '***'
  write(*,'(A)') '  **************************************************************************************************************'
  write(*,*)
  write(*,'(2x,A)') 'Task list:'
  write(*,'(2x,9A5)') 'Name', 'ti','ci','di','pi'
  do pr=1,np
     write(*,'(2x,A5, 9I5)') trim(name(pr)), ti(pr),ci(pr),di(pr),pi(pr)
  end do
  write(*,*)
  
  ! Print scheduling time:
  majFr = lcm(pi(1:np))
  if(time.le.0) then  ! Use major frame
     time = majFr
     write(*,'(A)') '  Using a major frame (hyperperiod) as the scheduling time.'
  end if
  write(*,'(2x,2(I0,A))') np, ' lines (processes) read; scheduling for ', time,' time units.'
  write(*,*)
  
  
  ! Print system load:
  write(*,'(/,A)', advance='no') '  System load: '
  load = 0.d0
  do pr=1,np
     frac = dble(ci(pr))/dble(pi(pr))
     load = load + frac
     write(*,'(A)', advance='no') d2s(frac,4)
     if(pr.lt.np) write(*,'(A)', advance='no') ' + '
  end do
  write(*,'(A)') ' = '//d2s(load,4)
  if(load.gt.1.d0) then
     write(*,'(A)') '  The task list is NOT schedulable indefinately... :-('
  else
     write(*,'(A)') '  The task list is SCHEDULABLE in principle (feasible) :-)'
  end if
  write(*,*)
  
  optts = gcd(ci(1:np))
  write(*,*)
  write(*,'(A,I0,A)') '  Optimal timeslice: ', optts, ' time units'
  write(*,'(A,I0,A)') '  Major frame: ', majFr, ' time units'
  write(*,'(A,I0,A)') '  Minor frame: ', gcd(pi(1:np)), ' time units'
  write(*,*)

end subroutine print_system_data
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  'Plot' an ascii scheduler

subroutine plot_ascii_scheduler(sched, np,time, name,ti,pi,di, run, detail)
  implicit none
  integer, intent(in) :: np,time, ti(np),pi(np),di(np), run(time)
  character, intent(in) :: sched*(9), name(np)*(9)
  logical, intent(in) :: detail
  integer :: it, pr
  
  write(*,*)
  write(*,'(A)') '  '//trim(sched)//' ASCII schedule:'
  
  do pr=1,np
     write(*,'(A4,3x)', advance='no') trim(name(pr))
     do it=1,time
        
        ! Mark runtime:
        if(run(it).eq.pr) then
           write(*,'(A)', advance='no') '#'
        else
           write(*,'(A)', advance='no') ' '
        end if
        
        ! Mark event/deadline:
        if(detail) then
           if( mod( ti(pr)-it + pi(pr)*1000, pi(pr)).eq.0 ) then  ! Next event
              write(*,'(A)', advance='no') 'e'
           else if( mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  then ! Next deadline != event
              write(*,'(A)', advance='no') 'd'
           else
              write(*,'(A)', advance='no') ' '
           end if
        end if
        
     end do  ! it
     write(*,*)
  end do  ! pr
  
  write(*,'(A4,I3)', advance='no') 't',0
  do it=1,time
     if(mod(it,5).eq.0) then
        if(detail) write(*,'(5x)', advance='no')
        write(*,'(I5)', advance='no') it
     end if
  end do
  write(*,*)
  
end subroutine plot_ascii_scheduler
!***********************************************************************************************************************************
  

