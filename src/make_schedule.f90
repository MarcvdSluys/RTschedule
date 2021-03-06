!  Copyright (c) 2016-2019  Marc van der Sluys - han.vandersluys.nl
!   
!  This file is part of the RT Schedule package, 
!  see: http://rtschedule.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.


!***********************************************************************************************************************************
subroutine make_schedule(sched, np,time, name, ti,ci,di,pi, load, fileBaseName, opTex)
  use SUFR_kinds, only: double
  use SUFR_sorting, only: sorted_index_list
  use SUFR_text, only: d2s
  use SUFR_system, only: quit_program_error
  use settings, only: asciiSchedule, printTable, plotSchedule,saveLaTeX
  
  implicit none
  integer, intent(in) :: np,time, ti(np),ci(np),di(np),pi(np), opTex
  real(double), intent(in) :: load
  character, intent(in) :: sched*(9), name(np)*(9), fileBaseName*(99)
  integer :: ccs(np,-1:time), run(-1:time), laxs(np,0:time)
  integer :: it,pr,pr1, ri,ro, indx(np), prio(np),cc(np),tte(np),ttd(np),lax(np), nSwitch,nMiss, Nopts
  real(double) :: maxLoad
  character :: ccpr*(9),priopr*(9)
  
  
  if(printTable.ge.1) then
     write(*,'(/)')
     if(printTable.ge.2) write(*,'(A)') '  **************************************************************************************************************'
     write(*,'(A,T110,A)') '  ***   '//trim(sched)//' SCHEDULER', '***'
     if(printTable.ge.2) write(*,'(A,/)') '  **************************************************************************************************************'
  end if
  
  cc=0; prio=0
  ro=1; tte=0; ttd=0
  ccs=0; laxs=0
  nSwitch=0; nMiss=0
  
  
  if(trim(sched).eq.'RMS') then
     ! Determine RMS priorities:
     call sorted_index_list(dble(pi), indx)
     
     do pr=1,np
        prio(indx(pr)) = pr
     end do
     
     ! Print task list with RMS priorities:
     if(printTable.ge.2) then
        write(*,'(2x,A)') 'RMS priorities:'
        write(*,'(2x,9A5)') 'Name', 'ti','ci','di','pi','prio'
        do pr=1,np
           write(*,'(2x,A5, 9I5)') trim(name(pr)), ti(pr),ci(pr),di(pr),pi(pr), prio(pr)
        end do
       write(*,*)
     end if
     
     ! RMS schedulability test:
     maxLoad = np * (2.d0**(1.d0/dble(np)) - 1.d0)
     if(printTable.ge.2) then
        write(*,'(A,I0,A)') '  RMS schedulability test for n=',np,':  SUM Ci/Pi <= n (2^(1/n) - 1):'
        write(*,'(A)', advance='no') '    '//d2s(load,3)//' <= '//d2s(maxLoad,3)
        if(load.le.maxLoad) then
           write(*,'(A)') ', so task set is GUARANTEED to be schedulable with RMS.'
        else if(load.le.1.d0) then
           write(*,'(A)') ', so task set is NOT guaranteed to be schedulable with RMS.'
        else
           write(*,'(A)') ', so task set is NOT SCHEDULABLE indefinately.'
        end if
        write(*,*)
     end if
  end if
  
  
  ! Start scheduling:
  
  ! Write header lines:
  if(printTable.ge.2) then
     select case(trim(sched))
     case('RMS')
        write(*,'(A11,3x)', advance='no') 'Timeslice'
        do pr=1,np
           write(*,'(A12)', advance='no') name(pr)
        end do
        write(*,'(A)') ' Running  Notes'
        
        
        write(*,'(10x)', advance='no')
        do pr=1,np
           write(*,'(5x,A7)', advance='no') 'cpu  ev'
        end do
        write(*,'(/)')
        
     case('EDF')
        write(*,'(A11,3x)', advance='no') 'Timeslice'
        do pr=1,np
           write(*,'(A15)', advance='no') name(pr)
        end do
        write(*,'(1x,A)') 'Running  Notes'
        
        
        write(*,'(10x)', advance='no')
        do pr=1,np
           write(*,'(A15)', advance='no') 'cpu  dl ev'
        end do
        write(*,'(/)')
        
     case('LST','LLF')
        write(*,'(A11,3x)', advance='no') 'Timeslice'
        do pr=1,np
           write(*,'(A15)', advance='no') name(pr)
        end do
        write(*,'(1x,A)') 'Running Lax  Notes'
        
        
        write(*,'(10x)', advance='no')
        do pr=1,np
           write(*,'(A15)', advance='no') 'lax cpu ev'
        end do
        write(*,'(/)')
        
     case default
        call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched)//'.  Available schedulers: RMS, EDF, LST.', 1)
     end select
  end if
  
  
  ! Initial computation times and laxities/priorities:
  do pr=1,np
     if(ti(pr).eq.0) then
        cc(pr) = ci(pr)
        ! The priority is given by the time to the next deadline in EDF:
        it = 0
        lax(pr) = di(pr) - ci(pr)  ! Initial laxity
        laxs(pr,0) = lax(pr)
        if(trim(sched).eq.'EDF') prio(pr) = mod( ti(pr)+di(pr)-it-1 + pi(pr)*1000000, pi(pr)) + 1  ! Time till deadline
        if(trim(sched).eq.'LST'.or.trim(sched).eq.'LLF') prio(pr) = lax(pr)  ! The priority is given by the laxity in LST/LLF
     end if
  end do
  
  
  do it=1,time  ! Note: this is the time unit that ENDS at t=ti
     
     ! Determine running task:
     ri = minloc(prio(1:np), 1, cc(1:np).gt.0)  ! Running task: minimum prio and cc>0
     if(ri*ro.ne.0) then
        if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. prio(ro).le.prio(ri)) ri = ro  ! Keep old task running if prios are equal
     end if
     
     
     ! Save the current computational times, running tasks and priorities for later use:
     ccs(1:np,it) = cc(1:np)
     run(it) = ri
     
     
     ! Report missed deadlines:
     do pr=1,np
        ttd(pr) = mod( ti(pr) + di(pr) - it + pi(pr)*1000000, pi(pr))  ! Time to next deadline
        
        ! The task's deadline has just passed, and the task is running and has ci>1 or is not running and has ci>0:
        if(ttd(pr).eq.0 .and. ( (pr.eq.ri.and.ccs(pr,it).gt.1) .or. (pr.ne.ri.and.ccs(pr,it).gt.0) ) ) then
           if(printTable.ge.2) write(*,'(//)')
           if(printTable.ge.1) write(*,'(A,I0,A)', advance='no') '  ***   At t=',it,', a DEADLINE IS MISSED for process'
           do pr1=1,np
              if( ttd(pr1).eq.0 .and. ccs(pr1,it).ge.1 ) then
                 if(printTable.ge.1) write(*,'(A)', advance='no') ' '//trim(name(pr1))
                 nMiss = nMiss + 1
              end if
           end do
           if(printTable.ge.1) write(*,'(A)') ', while process '//trim(name(ri))//' is running.   ***'
           if(printTable.ge.2) write(*,'(//)')
           exit
        end if
        
     end do  ! pr
     
     
     ! Print timestamp:
     if(printTable.ge.2) write(*,'(2x,I0,A,I0,T11,A)', advance='no') it-1,'-',it, ''
     
     ! Print detailed data:
     do pr=1,np
        
        write(ccpr,'(I0)') cc(pr)
        write(priopr,'(I0)') prio(pr)
        
        select case(trim(sched))
        case('RMS')  ! Detailed RMS data per process
           if(pr.eq.ri) then
              ccpr = '_'//trim(ccpr)//'_'
           else
              ccpr = ' '//trim(ccpr)
           end if
           
           if(printTable.ge.2) write(*,'(5x,A4,1x)', advance='no') ccpr
           
        case('EDF')  ! Detailed EDF data per process
           if(pr.eq.ri) then
              ccpr = '_'//trim(ccpr)//'_'
           else
              ccpr = ' '//trim(ccpr)
           end if
           if(cc(pr).eq.0) priopr = '-'
           
           if(printTable.ge.2) then
              write(*,'(5x,A4)', advance='no') ccpr
              write(*,'(A3,1x)', advance='no') trim(priopr)
           end if
           
        case('LST','LLF')  ! Detailed LST/LLF data per process
           if(cc(pr).eq.0) then
              priopr = '-'
              ccpr = '-'
           end if
           
           if(pr.eq.ri) then
              priopr = '_'//trim(priopr)//'_'
           else
              priopr = ' '//trim(priopr)
           end if
           
           if(printTable.ge.2) then
              write(*,'(5x,A4)', advance='no') priopr
              write(*,'(1x,A3)', advance='no') ccpr
           end if
           
        case default
           call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched)//'.  Available schedulers: RMS, EDF, LST.', 1)
        end select
        
        
        ! Label new event:
        if(printTable.ge.2) then
           if(it-1.ge.ti(pr) .and. tte(pr).eq.0) then  ! New event
              write(*,'(A)', advance='no') 'e'
           else
              write(*,'(A)', advance='no') ' '
           end if
           
           ! Label deadline:
           if(it-1.ge.ti(pr) .and. ttd(pr).eq.0) then  ! New deadline
              write(*,'(A)', advance='no') 'd'
           else
              write(*,'(A)', advance='no') ' '
           end if
        end if
        
     end do  ! pr
     
     if(printTable.ge.2) then
        select case(trim(sched))
        case('RMS','EDF')  ! Print currently running task:
           if(ri.eq.0) then
              write(*,'(2x,A9)', advance='no') '-'  ! No task is running
           else
              write(*,'(2x,A9)', advance='no') trim(name(ri))  ! Running task
           end if
           
        case('LST','LLF')  ! Print which task is running + its laxity:
           if(ri.eq.0) then  ! No task is running
              write(*,'(3x,A8,A4)', advance='no') '-','-'
           else
              write(*,'(3x,A8,I4)', advance='no') trim(name(ri)), prio(ri)  ! Running task and its laxity
           end if
           
        case default
           call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched)//'.  Available schedulers: RMS, EDF, LST.', 1)
        end select
     end if
     
     ! Label task switch:
     if(it.gt.1 .and. ri.ne.ro) then
        nSwitch = nSwitch + 1
        if(printTable.ge.2) then
           write(*,'(3x,A)', advance='no') 'Switch'
           if(ro.ne.0) then
              if(cc(ro).gt.0) then
                 write(*,'(A,I0,A)', advance='no') ' (task '//trim(name(ro))//': ',cc(ro), '>)'
              else
                 write(*,'(A)', advance='no') ' (task '//trim(name(ro))//' done)'
              end if
           end if
        end if
     end if
     
     ! Choice in LST/LLF:
     if(trim(sched).eq.'LST'.or.trim(sched).eq.'LLF') then
        Nopts = count(prio(1:np).eq.minval(prio(1:np), cc(1:np).gt.0) .and. cc(1:np).gt.0)  ! # tasks with Ci>0 and lowest laxity
        if(printTable.ge.2) then
           if(Nopts.gt.1) then   ! Have a choice
              if(ri.eq.ro) then  ! No choice - keep current task running
                 write(*,'(3x,A)', advance='no') 'Choice: keep same task'
              else               ! True choice
                 write(*,'(3x,A)', advance='no') 'Choice: pick first task'
              end if
           end if
        end if
     end if
     
     
     
     ! Run the current task:
     do pr=1,np
        if(pr.eq.ri) then
           cc(pr) = cc(pr) - 1                       ! The running task's cc decreases
        else
           if(it.ge.ti(pr)) lax(pr) = lax(pr) - 1   ! All other tasks: laxity decreases
        end if
     end do
     
     select case(trim(sched))
     case('RMS')
        
     case('EDF')
        prio = mod( ti+di-it-1 + pi*1000000, pi) + 1    ! Update all priorities
        
     case('LST','LLF')
        prio(1:np) = lax(1:np)                          ! Priority ~ laxity for LST/LLF
        
     case default
        call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched)//'.  Available schedulers: RMS, EDF, LST.', 1)
     end select
     
     
     
     ! New event:
     do pr=1,np
        tte(pr) = mod( ti(pr) - it + pi(pr)*1000000, pi(pr))           ! Time to next event
        if(tte(pr).eq.0) then         ! New event occurs
           cc(pr) = ci(pr)            ! Reset the computation time
           lax(pr) = di(pr) - ci(pr)  ! Reset the laxity
           if(trim(sched).eq.'LST'.or.trim(sched).eq.'LLF') prio(pr) = lax(pr)  ! Priority ~ laxity for LST/LLF
        end if
     end do

     laxs(1:np,it) = lax(1:np)  ! Needed for annotation in LST/LLF
     
     ro = ri
     if(printTable.ge.2) write(*,*)
  end do  ! it=1,time
  
  
  ! Report on missed deadlines:
  if(printTable.ge.1) then
     if(printTable.ge.2) write(*,*)
     select case(nMiss)
     case(0)
        write(*,'(A,I0,A)') '  No deadlines were missed: the system can be scheduled for ', time, ' time units.'
     case(1)
        write(*,'(2x,A,I0,A)') 'ONE DEADLINE HAS BEEN MISSED in ', time, ' time units.'
     case default
        write(*,'(2x,I0,A,I0,A)') nMiss, ' DEADLINES HAVE BEEN MISSED in ', time, ' time units.'
     end select
     write(*,'(2x,I0,A)') nSwitch, ' task switches occurred ('//d2s(dble(time)/dble(nSwitch+1),2)//' time units per run).'
     
     if(saveLaTeX.ge.1) then
        write(opTex,'(A)') ''
        write(opTex,'(A)') '\subsection{Results for the '//trim(sched)//' scheduler}'
        select case(nMiss)
        case(0)
           write(opTex,'(A,I0,A)') 'No deadlines were missed; the system can be scheduled for ', time, ' time units.'
        case(1)
           write(opTex,'(A,I0,A)') '\textbf{One deadline has been missed} in ', time, ' time units.'
        case default
           write(opTex,'(A,I0,A,I0,A)') '\textbf{',nMiss, ' deadlines have been missed} in ', time, ' time units.'
        end select
        write(opTex,'(I0,A)') nSwitch, ' task switches occurred (\emph{i.e.}\ '//d2s(dble(time)/dble(nSwitch+1),2)//' time units per run).'
        
        if(plotSchedule.ge.1) write(opTex,'(A)') 'The '//trim(sched)//' schedule can be found in Figure~\ref{fig:'//trim(fileBaseName)//'_'//trim(sched)//'}.'
     end if
     
     if(printTable.ge.2) write(*,*)
  end if
  
  
  ! 'Plot' an ascii schedule:
  if(asciiSchedule.eq.1) call plot_ascii_schedule(sched, np,time, name,ti,pi,di, run,ccs, .false.)  ! Detail: .false.
  if(asciiSchedule.eq.2) call plot_ascii_schedule(sched, np,time, name,ti,pi,di, run,ccs, .true.)   ! Detail: .true.
  
  
  ! Graphical plot:
  if(printTable.ge.2) write(*,*)
  if(plotSchedule.ge.1) call plot_schedule(sched, np,time, name,ti,pi,di, ccs,run,laxs, fileBaseName,opTex)
  
  
end subroutine make_schedule
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  'Plot' an ascii schedule

subroutine plot_ascii_schedule(sched, np,time, name,ti,pi,di, run,ccs, detail)
  use settings, only: optTS
  implicit none
  integer, intent(in) :: np,time, ti(np),pi(np),di(np), run(-1:time), ccs(np,-1:time)
  character, intent(in) :: sched*(9), name(np)*(9)
  logical, intent(in) :: detail
  integer :: it, pr
  
  write(*,*)
  if(detail) then
     write(*,'(A)') '  '//trim(sched)//' ASCII schedule with events and deadlines:'
  else
     write(*,'(A)') '  '//trim(sched)//' ASCII schedule:'
  end if
  
  
  ! Print schedule for each task:
  do pr=1,np
     write(*,'(A4,1x)', advance='no') trim(name(pr))
     do it=0,time
        
        ! Mark runtime:
        if(run(it).eq.pr) then
           write(*,'(A)', advance='no') '#'
        else
           write(*,'(A)', advance='no') ' '
        end if
        
        ! Mark event/deadline:  priority: missed deadline, event, non-missed deadline, nothing (space):
        if(detail) then
           if( mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  then  ! Deadline
              if(it.ne.0 .and. ccs(pr,it).gt.0) then
                 write(*,'(A)', advance='no') 'D'                         ! Missed deadline
              else if( mod( ti(pr)-it + pi(pr)*1000, pi(pr)).eq.0 ) then  ! Event == deadline
                 write(*,'(A)', advance='no') 'e'
              else
                 write(*,'(A)', advance='no') 'd'                         ! Deadline != event
              end if
           else if( mod( ti(pr)-it + pi(pr)*1000, pi(pr)).eq.0 ) then     ! Event != deadline
              write(*,'(A)', advance='no') 'e'
           else
              write(*,'(A)', advance='no') ' '
           end if
        end if
        
     end do  ! it
     write(*,*)
  end do  ! pr
  
  
  ! Print time labels:
  write(*,'(A4,I3)', advance='no') 't',0
  do it=1,time
     if(mod(it,5).eq.0) then
        if(detail) write(*,'(5x)', advance='no')
        write(*,'(I5)', advance='no') it*optTS
     end if
  end do
  
  if(optTS.gt.1) then
     write(*,*)
     write(*,'(2x,A,I0,A)') 'Each timeslice equals ', optTS, ' time units.'
  end if
  write(*,*)
  
end subroutine plot_ascii_schedule
!***********************************************************************************************************************************
  

