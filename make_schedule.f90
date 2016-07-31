!***********************************************************************************************************************************
subroutine make_schedule(sched, np,time, name, ti,ci,di,pi, load)
  use SUFR_kinds, only: double
  use SUFR_sorting, only: sorted_index_list
  use SUFR_text, only: d2s
  use SUFR_system, only: quit_program_error
  
  implicit none
  integer, intent(in) :: np,time, ti(np),ci(np),di(np),pi(np)
  real(double), intent(in) :: load
  character, intent(in) :: sched*(9), name(np)*(9)
  integer, allocatable :: run(:), ccs(:,:)
  integer :: it,pr, ri,ro, indx(np), prio(np),cc(np),tte(np), nSwitch,nMiss, Nopts
  real(double) :: maxLoad
  character :: ccpr*(9),priopr*(9),ttepr*(9)
  
  cc=0; prio=0
  ro=1; tte=0
  allocate(ccs(np,time), run(time));  ccs = 0
  nSwitch=0; nMiss=0
  
  if(trim(sched).eq.'RM') then
     ! Determine RM priorities:
     call sorted_index_list(dble(pi), indx)
     
     do pr=1,np
        prio(indx(pr)) = pr
     end do
     
     ! Print task list with RM priorities:
     write(*,'(2x,A)') 'RM priorities:'
     write(*,'(2x,9A5)') 'Name', 'ti','ci','di','pi','prio'
     do pr=1,np
        write(*,'(2x,A5, 9I5)') trim(name(pr)), ti(pr),ci(pr),di(pr),pi(pr), prio(pr)
     end do
     write(*,*)
     
     ! RM schedulability test:
     maxLoad = np * (2.d0**(1.d0/dble(np)) - 1.d0)
     write(*,'(A,I0,A)') '  RM schedulability test for n=',np,':  SUM Ci/Pi <= n (2^(1/n) - 1):'
     write(*,'(A)', advance='no') '    '//d2s(load,3)//' <= '//d2s(maxLoad,3)
     if(load.le.maxLoad) then
        write(*,'(A)') ', so task set is GUARANTEED to be schedulable with RM.'
     else if(load.le.1.d0) then
        write(*,'(A)') ', so task set is NOT guaranteed to be schedulable with RM.'
     else
        write(*,'(A)') ', so task set is NOT SCHEDULABLE indefinately.'
     end if
  end if
  
  
  ! Start scheduling:
  
  ! Initial computation times and laxities:
  do pr=1,np
     if(ti(pr).eq.0) then
        cc(pr) = ci(pr)
        if(trim(sched).eq.'LLF') prio(pr) = di(pr) - ci(pr)  ! The priority is given by the laxity in LLF
     end if
  end do
  
  do it=1,time  ! Note: this is the time unit that ENDS at t=ti
     
     ! Save cc for later use:
     ccs(1:np,it) = cc(1:np)
     

     ! Determine running task:
     ri = minloc(prio(1:np), 1, cc(1:np).gt.0)  ! Running task: minimum prio and cc>0
     if(ri*ro.ne.0) then
        if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. prio(ro).le.prio(ri)) ri = ro  ! Keep old task running if prios are equal
     end if
     run(it) = ri  ! Save for later use
     
     
     ! Report missed deadlines:
     if( maxval(ccs(:, it), mod( ti+di-it + pi*1000, pi).eq.0).gt.1 ) then  ! The maximum Ci of tasks with Di=0 is >= 2
        write(*,'(//,A,I0,A)', advance='no') '   ***   At t=',it,', a deadline is missed for process'
        do pr=1,np
           if( mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr)).eq.0 .and. ccs(pr,it).gt.1 ) then
              write(*,'(A)', advance='no') ' '//trim(name(pr))
              nMiss = nMiss + 1
           end if
        end do
        write(*,'(A,//)') ', while process '//trim(name(ri))//' is running.   ***'
     end if
     
     
     ! Print timestamp:
     write(*,'(2x,I0,A,I0,T11,A)', advance='no') it-1,'-',it, ''
     
     ! Print detailed data:
     do pr=1,np
        
        write(ccpr,'(I0)') cc(pr)
        write(priopr,'(I0)') prio(pr)
        write(ttepr,'(I0)') tte(pr)
        
        select case(trim(sched))
        case('RM')
           if(pr.eq.ri) then
              ccpr = '_'//trim(ccpr)//'_'
           else
              ccpr = ' '//trim(ccpr)
           end if
           
           write(*,'(2x,A4)', advance='no') ccpr
           write(*,'(2x,I4)', advance='no') tte(pr)
        case('LLF')
           if(cc(pr).eq.0) then
              priopr = '-'
              ttepr = '-'
           end if
           
           if(pr.eq.ri) then
              priopr = '_'//trim(priopr)//'_'
           else
              priopr = ' '//trim(priopr)
           end if
           
           write(*,'(5x,A4)', advance='no') priopr
        case default
           call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched), 1)
        end select
        
        
        ! Label new event:
        if(it.ge.ti(pr) .and. tte(pr).eq.0) then  ! New event
           write(*,'(1x,A)', advance='no') 'e'
        else
           write(*,'(1x,A)', advance='no') ' '
        end if
        
     end do  ! pr
     
     
     select case(trim(sched))
     case('RM')
        ! Print currently running task:
        write(*,'(5x,2I4)', advance='no') ri,ro
     case('LLF')
        ! Print which task is running + its laxity:
        if(ri.eq.0) then  ! No task is running
           write(*,'(5x,A)', advance='no') 'run: -,  lax: -,  cpu: -'
        else
           write(*,'(5x,3(A,I0))', advance='no') 'run: ',ri, ',  lax: ', prio(ri), ',  cpu: ', cc(ri)
        end if
     case default
        call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched), 1)
     end select
     
     
     ! Label task switch:
     if(it.gt.1 .and. ri.ne.ro) then
        write(*,'(2x,A)', advance='no') 'switch'
        nSwitch = nSwitch + 1
        if(ro.ne.0) then
           if(cc(ro).gt.0) then
              write(*,'(A,I0,A)', advance='no') ' (task '//trim(name(ro))//': ',cc(ro), '>)'
           else
              write(*,'(A)', advance='no') ' (task '//trim(name(ro))//' done)'
           end if
        end if
     end if
     
     ! Choice in LLF:
     if(trim(sched).eq.'LLF') then
        Nopts = count(prio(1:np).eq.minval(prio(1:np), cc(1:np).gt.0) .and. cc(1:np).gt.0)  ! # tasks with Ci>0 and lowest laxity
        if(Nopts.gt.1) then   ! Have a choice
           if(ri.eq.ro) then  ! No choice - keep current task running
              write(*,'(2x,A)', advance='no') 'Choice: keep same task'
           else               ! True choice
              write(*,'(2x,A)', advance='no') 'Choice: pick first task'
           end if
        end if
     end if
     
     
     select case(trim(sched))
     case('RM')
        ! Run the task; decrease its cc:
        if(ri.gt.0) cc(ri) = cc(ri) - 1
     case('LLF')
        ! Run job: current job is ri, so its cc decreases; all other laxities decrease:
        do pr=1,np
           if(pr.eq.ri) then
              cc(pr) = cc(pr) - 1
           else
              if(it.ge.ti(pr)) prio(pr) = prio(pr) - 1
           end if
        end do
     case default
        call quit_program_error('make_schedule():  unknown scheduler: '//trim(sched), 1)
     end select
     
     
     ! New event:
     do pr=1,np
        tte(pr) = mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr))  ! Time to next deadline
        if(tte(pr).eq.0) then        ! New event occurs
           cc(pr) = ci(pr)           ! Reset the computation time 
           if(trim(sched).eq.'LLF') prio(pr) = di(pr) - ci(pr)  ! Reset the laxity for LLF
        end if
     end do
     
     ro = ri
     write(*,*)
  end do  ! it
  
  
  ! 'Plot' an ascii scheduler:
  call plot_ascii_scheduler(np,time, name,ti,pi,di, run, .false.)  ! Detail: .true./.false.
  call plot_ascii_scheduler(np,time, name,ti,pi,di, run, .true.)  ! Detail: .true./.false.
  
  
  ! Graphical plot:
  call plot_scheduler(sched, np,time, name,ti,pi,di, ccs,run)
  
  
  ! Report on missed deadlines:
  select case(nMiss)
  case(0)
     write(*,'(/,A,I0,A)') '  No deadlines were missed: the system can be scheduled for ', time, ' time units.'
  case(1)
     write(*,'(/,2x,I0,A,I0,A)') nMiss, ' ONE DEADLINE HAS BEEN MISSED in ', time, ' time units.'
  case default
     write(*,'(/,2x,I0,A,I0,A)') nMiss, ' DEADLINES HAVE BEEN MISSED in ', time, ' time units.'
  end select
  write(*,'(2x,I0,A)') nSwitch, ' task switches ('//d2s(dble(time)/dble(nSwitch+1),2)//' time units per run).'
  
  write(*,*)
  
end subroutine make_schedule
!***********************************************************************************************************************************
