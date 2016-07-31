!***********************************************************************************************************************************
subroutine schedule_LLF(np,time, name, ti,ci,di,pi)
  use SUFR_text, only: d2s
  
  implicit none
  integer, intent(in) :: np,time, ti(np),ci(np),di(np),pi(np)
  character, intent(in) :: name(np)*(9)
  integer :: it,pr, ri,ro, li(np),cc(np),tte(np), Nopts, nSwitch,nMiss
  integer, allocatable :: run(:), ccs(:,:)
  character :: ccpr*(9),lipr*(9),ttepr*(9)
  
  ro=1; tte=0
  allocate(ccs(np,time), run(time));  ccs = 0
  nSwitch=0; nMiss=0
  
  ! Initial computation times and laxities:
  cc=0; li=0
  do pr=1,np
     if(ti(pr).eq.0) then
        cc(pr) = ci(pr)
        li(pr) = di(pr) - ci(pr)
     end if
  end do
  
  do it=1,time  ! Note: this is the time unit that ENDS at t=ti
     
     ! Save cc for later use:
     ccs(1:np,it) = cc(1:np)
     

     ! Determine running task:
     ri = minloc(li(1:np), 1, cc(1:np).gt.0)  ! Running task: minimum li and cc>0
     if(ri*ro.ne.0) then
        if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. li(ro).le.li(ri)) ri = ro  ! Keep old task running if laxities are equal
     end if
     run(it) = ri  ! Save for later use
     
     
     ! Deadline missed:
     if(minval(li(1:np)).lt.0) then
        write(*,'(//,A,I0,A)', advance='no') '   ***   At t=',it,', a deadline is missed for process'
        do pr=1,np
           if(li(pr).lt.0) then
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
        write(lipr,'(I0)')  li(pr)
        write(ttepr,'(I0)') tte(pr)
        if(cc(pr).eq.0) then
           lipr = '-'
           ttepr = '-'
        end if
        
        
        if(pr.eq.ri) then
           lipr = '_'//trim(lipr)//'_'
        else
           lipr = ' '//trim(lipr)
        end if
        write(*,'(5x,A4)', advance='no') lipr
        !write(*,'(A2)', advance='no') trim(ccpr)
        !write(*,'(A2)', advance='no') trim(ttepr)
        
        ! Label new event:
        if(it.ge.ti(pr) .and. tte(pr).eq.0) then  ! New event
           write(*,'(1x,A)', advance='no') 'e'
        else
           write(*,'(1x,A)', advance='no') ' '
        end if
        
     end do  ! pr
     
     
     ! Print which task is running + its laxity:
     if(ri.eq.0) then  ! No task is running
        write(*,'(5x,A)', advance='no') 'run: -,  lax: -,  cpu: -'
     else
        write(*,'(5x,3(A,I0))', advance='no') 'run: ',ri, ',  lax: ', li(ri), ',  cpu: ', cc(ri)
     end if
     
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
     
     Nopts = count(li(1:np).eq.minval(li(1:np), cc(1:np).gt.0) .and. cc(1:np).gt.0)  ! Number of tasks with Ci>0 and lowest laxity
     if(Nopts.gt.1) then   ! Have a choice
        if(ri.eq.ro) then  ! No choice - keep current task running
           write(*,'(2x,A)', advance='no') 'Choice: keep same task'
        else               ! True choice
           write(*,'(2x,A)', advance='no') 'Choice: pick first task'
        end if
     end if
     
     
     
     ! Run job: current job is ri, so its cc decreases; all other laxities decrease:
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
     
     ro = ri
     write(*,*)
  end do  ! it
  
  
  ! 'Plot' an ascii scheduler:
  call plot_ascii_scheduler(np,time, name,ti,pi,di, run, .false.)  ! Detail: .true./.false.
  call plot_ascii_scheduler(np,time, name,ti,pi,di, run, .true.)  ! Detail: .true./.false.
  
  
  ! Graphical plot:
  call plot_scheduler(np,time, name,ti,pi,di, ccs,run)
  
  
  ! Report on missed deadlines:
  if(nMiss.eq.0) then
     write(*,'(/,A,I0,A)') '  No deadlines were missed: the system can be scheduled for ', time, ' time units.'
  else
     write(*,'(/,2x,I0,A,I0,A)') nMiss, ' DEADLINES HAVE BEEN MISSED in ', time, ' time units.'
  end if
  write(*,'(2x,I0,A)') nSwitch, ' task switches ('//d2s(dble(time)/dble(nSwitch+1),2)//' time units per run).'
  
end subroutine schedule_LLF
!***********************************************************************************************************************************
