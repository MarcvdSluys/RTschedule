!***********************************************************************************************************************************
program schedule
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  
  implicit none
  integer, parameter :: nProcMax=19  ! Maximum number of processes to expect
  integer :: np, time, ti(nProcMax),ci(nProcMax),di(nProcMax),pi(nProcMax)
  real(double) :: load
  character :: sched*(9), name(nProcMax)*(9)
  
  ! Initialise libSUFR:
  call set_SUFR_constants()
  
  ! Read the input file:
  call read_input_file(nProcMax, name, ti,ci,di,pi, np,time)
  
  ! Print some basic data about the system:
  call print_system_data(np,time, name,ti,ci,di,pi, load)
  
  
  ! Create an RM schedule:
  sched = 'RM'  ! RM algorithm
  call make_schedule(sched, np,time, name, ti,ci,di,pi, load)
  
  
  ! Create a LLF schedule:
  sched = 'LLF'  ! LLF algorithm
  call make_schedule(sched, np,time, name, ti,ci,di,pi, load)
  
  write(*,*)
end program schedule
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine read_input_file(nProcMax, name, ti,ci,di,pi, np,time)
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, syntax_quit
  use SUFR_dummy, only: dumStr
  
  implicit none
  integer, intent(in) :: nProcMax
  integer, intent(out) :: ti(nProcMax),ci(nProcMax),di(nProcMax),pi(nProcMax), np,time
  character, intent(out) :: name(nProcMax)*(9)
  integer :: ip,ln, status
  character :: inFile*(99)
  
  ! See whether a file name was passed on the command line:
  if(command_argument_count().ne.1) call syntax_quit('<input file name>', 0, 'Simple scheduling tool for LLF')
  call get_command_argument(1, inFile)
  
  ! Open the input file:
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(inFile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(inFile), 1, 1)  ! 1: input file, 1: status: not ok
  
  ! Read file header:
  do ln=1,1
     read(ip,'(A)') dumStr
  end do
  
  ! Read file body:
  write(*,*)
  read(ip,*) dumStr, time
  do ln=1,nProcMax
     read(ip,*,iostat=status) name(ln), ti(ln),ci(ln),di(ln),pi(ln)
     if(status.lt.0) exit
     if(status.gt.0) call file_read_error_quit(trim(inFile), ln, 0)
  end do  ! ln

  ! Close the file:
  close(ip)
  np = ln - 1  ! Number of processes/tasks found
  
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
     write(*,'(A)') '  The task list is NOT schedulable indefinately... :-('
  else
     write(*,'(A)') '  The task list is SCHEDULABLE in principle (feasible) :-)'
  end if
  write(*,*)
  
  optts = gcd(ci(1:np))
  write(*,'(A,I0,A)') '  Optimal timeslice: ', optts, ' time units'
  write(*,'(A,I0,A)') '  Major frame: ', majFr, ' time units'
  write(*,'(A,I0,A)') '  Minor frame: ', gcd(pi(1:np)), ' time units'
  write(*,*)

end subroutine print_system_data
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  'Plot' an ascii scheduler

subroutine plot_ascii_scheduler(np,time, name,ti,pi,di, run, detail)
  implicit none
  integer, intent(in) :: np,time, ti(np),pi(np),di(np), run(time)
  character, intent(in) :: name(np)*(9)
  logical, intent(in) :: detail
  integer :: it, pr
  
  write(*,*)
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
  

!***********************************************************************************************************************************
!> \brief  Plot a graphical scheduler

subroutine plot_scheduler(sched, np,time, name,ti,pi,di, ccs,run)
  use SUFR_kinds, only: double
  use plplot, only: plsdev, plsfnam, plbox, plmtex,plfill,plptex, plpoin
  
  implicit none
  integer, intent(in) :: np,time, ti(np),pi(np),di(np), ccs(np,time),run(time)
  character, intent(in) :: sched*(9), name(np)*(9)
  integer :: it, pr, xsize,ysize
  real(double) :: rat
  character :: tmpStr*(9)
  
  
  call plsfnam('schedule_'//trim(sched)//'.png')  ! Set file name
  call plsdev('pngcairo')                         ! Set plotting device: png
  
  xsize = 1400  ! pixels
  rat = max( dble(np)/dble(time), 0.15d0)
  ysize = nint(dble(xsize) * rat )
  call plspage(0.d0,0.d0, xsize,ysize, 0,0)       ! Set page size: dpi, size, offset (px/mm)
  call plmycolours()                              ! White bg, proper colours
  
  call plinit()                                   ! Initialise environment; Call after plsdev(), plssub(), plspage()
  call plbop()                                    ! Begin a new page
  call plvpor(0.07d0,0.96d0, 1.d0/dble(np),0.96d0)   ! Set view port in plot window
  call plwind(0,dble(time), dble(np),0)           ! Set view port in world coordinates
  
  call plwidth(2.d0)                              ! Thick lines
  call pllsty(1)                                  ! Full lines
  
  do it=1,time
     
     ! Fill a square:
     call plcol0(10)                              ! Grey squares
     if(run(it).ne.0) then
        call plfill( dble([it-1,it-1,it,it]), dble([run(it),run(it)-1,run(it)-1,run(it)]) )
     end if
     
     ! Print remaining cpu time:
     if(it.gt.1) then  ! Can't have a task switch in timeslise 0-1
        if(run(it).ne.run(it-1) .and. run(it-1).ne.0) then  ! There must be a task switch
           if(ccs(run(it-1),it).gt.0) then  ! CPU time left for the old task
              
              call plcol0(1)                              ! Black text
              write(tmpStr,'(I0,A)') ccs(run(it-1),it), '>'
              call plptex(dble(it-1), dble(run(it-1)-0.5d0), 1.d0,0.d0, 1.d0, trim(tmpStr))
              
           end if
        end if
     end if
     
  end do  ! it
  
  
  call plcol0(1)                              ! Black box
  call plbox('BCGHNT',5.d0,5, 'BCGT',1.d0,0)  ! Plot box
  
  
  call plcol0(2)                              ! Red lines
  call plwidth(2.5d0)                          ! Thicker lines
  do it=0,time
     
     do pr=1,np
        
        ! Mark event (up arrow, as in SimSo - len 0.25, angle 25d):
        if( mod( ti(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  call plarrow(2,  dble([it,it]), dble([pr,pr-1]), 0.25d0, 25.d0)
        
        ! Mark deadline (down arrow, as in SimSo):
        if( mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  then
           call plarrow( 2, dble([it,it]), dble([pr-1,pr]), 0.25d0, 25.d0)  ! len = 0.25, angle 25d
           if(it.gt.0) then
              if(ccs(pr,it).gt.1) then  ! Process pr misses a deadline at t=it
                 call plssym(10.d0, 1.d0)  ! Huge symbols
                 call plpoin(dble([it]), dble([pr])-0.5d0, 5)  ! Cross
                 call plssym(5.d0, 1.d0)  ! Default symbol size
              end if
           end if
        end if
        
     end do  ! pr
     
     ! Mark a completed task:
     if(it.gt.1) then  ! Can't have a task switch in timeslise 0-1
        if(run(it).ne.run(it-1) .and. run(it-1).ne.0) then  ! There must be a task switch
           if(ccs(run(it-1),it).le.0) then  ! No CPU time left for the old task
              call plssym(7.d0, 1.d0)  ! Larger symbols
              call plpoin([dble(it-1)], [dble(run(it-1))], 17)
              call plssym(5.d0, 1.d0)  ! Default symbol size
           end if
        end if
     end if
     
  end do  ! it
  
  call plwidth(2.d0)                          ! Thick lines
  
  
  
  
  ! Print axis labels:
  call plcol0(1)                                   ! Black text
  call plmtex('B', 3.5d0, 0.5d0,0.5d0, 'Time')     ! Plot label for horizontal axis
  call plmtex('L', 3.5d0, 0.5d0,0.5d0, 'Process')  ! Plot label for vertical axis
  
  
  ! Print the task names:
  do pr=1,np
     call plmtex('LV', 1.5d0, (dble(np-pr)+0.5d0)/dble(np), 0.5d0, trim(name(pr)))  ! Plot label for vertical axis
  end do
  
  call plend()                                ! Finish plot
  
end subroutine plot_scheduler
!***********************************************************************************************************************************
  

!***********************************************************************************************************************************
!> \brief  Create a white background and define my colours in PLplot

subroutine plmycolours()
  implicit none
  
  call plscol0(0,  255,255,255)  ! Default BG, white
  call plscol0(1,  0,0,0)        ! Default foreground, black
  call plscol0(2,  255,0,0)      ! Red
  call plscol0(3,  0,191,0)      ! Green for white bg
  call plscol0(4,  0,0,255)      ! Blue
  call plscol0(5,  0,191,191)    ! Magenta -> darker for white bg
  call plscol0(6,  191,0,191)    ! Cyan -> darker
  call plscol0(7,  255,127,0)    ! Orange
  call plscol0(8,  63,255,63)    ! Light green
  call plscol0(9,  127,63,0)     ! Brown
  call plscol0(10, 150,150,150)  ! Light grey for white bg (170)
  call plscol0(11, 84,84,84)     ! Dark grey for white bg
  call plscol0(12, 255,127,127)  ! Pink/salmon
  call plscol0(13, 150,0,0)      ! Dark red
  call plscol0(14, 255,0,255)    ! Cyan
  call plscol0(15, 255,255,0)    ! Yellow
  
end subroutine plmycolours
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine plarrow(Narr, Xarr,Yarr, arrowLen, angle)
  use SUFR_constants, only: d2r !,r2d
  use plplot, only: plflt, plline, plfill
  
  implicit none
  integer, intent(in) :: Narr
  real(kind=plflt), intent(in) :: Xarr(Narr),Yarr(Narr), arrowLen,angle
  real(kind=plflt) :: dX,dY, dX1,dX2, dY1,dY2, alpha,alpha1,alpha2
  
  
  call plline(Xarr, Yarr)        ! Plot line
  
  dX = Xarr(Narr) - Xarr(Narr-1)
  dY = Yarr(Narr) - Yarr(Narr-1)
  
  alpha = atan2(dY,dX)
  alpha1 = alpha - angle*d2r
  alpha2 = alpha + angle*d2r
  
  dX1 = -arrowLen * cos(alpha1)
  dY1 = -arrowLen * sin(alpha1)
  dX2 = -arrowLen * cos(alpha2)
  dY2 = -arrowLen * sin(alpha2)
  
  !call plline([Xarr(Narr), Xarr(Narr)+dX1], [Yarr(Narr), Yarr(Narr)+dY1])        ! Plot first head line
  !call plline([Xarr(Narr), Xarr(Narr)+dX2], [Yarr(Narr), Yarr(Narr)+dY2])        ! Plot second head line
  !
  !call plline([Xarr(Narr)+dX1, Xarr(Narr)+dX2], [Yarr(Narr)+dY1, Yarr(Narr)+dY2])        ! Plot third line for triangular head
  
  call plpsty(0)                                                                                           ! Filled arrow head
  call plfill([Xarr(Narr), Xarr(Narr)+dX1, Xarr(Narr)+dX2], [Yarr(Narr), Yarr(Narr)+dY1, Yarr(Narr)+dY2])  ! Plot line
  
  
end subroutine plarrow
!***********************************************************************************************************************************
