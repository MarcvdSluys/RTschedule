!***********************************************************************************************************************************
subroutine schedule_RM(np,time, name, ti,ci,di,pi, load)
  use SUFR_kinds, only: double
  use SUFR_sorting, only: sorted_index_list
  use SUFR_text, only: d2s
  
  implicit none
  integer, intent(in) :: np,time, ti(np),ci(np),di(np),pi(np)
  real(double), intent(in) :: load
  character, intent(in) :: name(np)*(9)
  integer, allocatable :: run(:), ccs(:,:)
  integer :: indx(np), it,pr, prio(np),cc(np),tte(np), ri,ro, nSwitch,nMiss !Nopts
  real(double) :: maxLoad
  character :: ccpr*(9),priopr*(9)
  
  ! Determine priorities:
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
  
  
  ! Start scheduling
  ro=1; tte=0
  allocate(ccs(np,time), run(time));  ccs = 0
  nSwitch=0; nMiss=0
  
  ! Initial computation times:
  cc=0
  do pr=1,np
     if(ti(pr).eq.0) then
        cc(pr) = ci(pr)
     end if
  end do
  
  do it=1,time
     
     ! Save cc for later use:
     ccs(1:np,it) = cc(1:np)
     

     ! Determine running task:
     ri = minloc(prio(1:np), 1, cc(1:np).gt.0)  ! Running task: minimum prio and cc>0
     if(ri*ro.ne.0) then
        if(it.ne.1 .and. ri.ne.ro .and. cc(ro).gt.0 .and. prio(ro).le.prio(ri)) ri = ro  ! Keep old task running if prios are equal
     end if
     run(it) = ri  ! Save for later use
     
     ! Print timestamp:
     write(*,'(2x,I0,A,I0,T11,A)', advance='no') it-1,'-',it, ''
     
     ! Print detailed data:
     do pr=1,np
        
        write(ccpr,'(I0)') cc(pr)
        write(priopr,'(I0)') prio(pr)
        if(pr.eq.ri) then
           ccpr = '_'//trim(ccpr)//'_'
        else
           ccpr = ' '//trim(ccpr)
        end if
        
        !write(*,'(5x,A4)', advance='no') priopr
        write(*,'(2x,A4)', advance='no') ccpr
        write(*,'(2x,I4)', advance='no') tte(pr)
        

        ! Label new event:
        if(it.ge.ti(pr) .and. tte(pr).eq.0) then  ! New event
           write(*,'(1x,A)', advance='no') 'e'
        else
           write(*,'(1x,A)', advance='no') ' '
        end if
        

     end do  ! pr
     
     write(*,'(5x,2I4)', advance='no') ri,ro
     
     
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
     

     
     ! Run the task; decrease its cc:
     if(ri.gt.0) cc(ri) = cc(ri) - 1
     
     
     ! New event:
     do pr=1,np
        tte(pr) = mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr))  ! Time to next deadline
        if(tte(pr).eq.0) then        ! New event occurs
           cc(pr) = ci(pr)           ! Reset the computation time 
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
  
end subroutine schedule_RM
!***********************************************************************************************************************************
