!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.23
!
!  ストップウォッチ
!
!  History
!    2013.06.05: This comes from my old timer.f90.
!    2013.08.04: Removed timer__start and __end.
!    2013.08.04: TM_STT & _END be public.
!    2017.07.03: use mpi. print by only rank 0.
!    2017.07.09: Merged the bugfix by T. Furuzono.
!    2017.07.09: Use efpp.sh macros.
!    2017.07.17: This is kutimer.e03. Renamed from timer.e03.
!    2020.01.23: mv kutimer.e03 watch.f90
!
!
module watch_m
  implicit none
  private
  public :: & !< routines >!
            watch__,            &
            watch__count,       &
            watch__end,         &
            watch__print,       &
            watch__start

  integer, parameter :: WATCH_STT = 1  ! Any numbers are fine;
  integer, parameter :: WATCH_END = 2  ! just be different.


  integer :: clock_counter
  integer, parameter :: MAX_KEY_N    = 50
  integer, parameter :: MAX_SUBKEY_N = 100
  integer, parameter :: KEYLEN       = 6
  integer, save :: job_start_time,  &
                   job_end_time,    &
                   loop_counter = 0
  character(len=19), save :: dateandtime_start, dateandtime_end
  ! ttb_t = Time table struct. When you change this,  take care of
  ! the initialization of the array 'ttb' (see below).

  type ttb_t
    integer :: stt_tim, total_tim, call_cntr, sub_last_tim
    character(len=KEYLEN) :: keyname
    integer, dimension(MAX_KEY_N) :: sub_total_tim
    character(len=KEYLEN), dimension(MAX_SUBKEY_N) :: sub_keyname
    integer :: sub_key_n            ! Number of break points
    logical :: pair_check           ! (stt,end) must be a pair.
  end type ttb_t

  ! Decreration & initialization of array.
  ! This is automatically saved anyway.
  type(ttb_t), dimension(MAX_KEY_N),             &
               save :: ttb = ttb_t( 0,           &  ! stt_tim
                                    0,           &  ! total_tim
                                    0,           &  ! call_cntr
                                    0,           &  ! call_cntsub_last_tim
                                    '123456',    &  ! keyname
                                    0,           &  ! sub_total_tim
                                    '******',    &  ! sub_key_name
                                    0,           &  ! sub_key_n
                                    .true. )        ! pair_check

  integer :: key_n ! Used key number

  interface watch__
    module procedure start_or_end, break_point
  end interface

  logical :: jump_flag = .false.   ! Jump all timer routines if this is set.

contains

!---------------
!--- Private --
!---------------

  function get_dateandtime()
    character(len=19) :: get_dateandtime
    character(len= 8) :: date
    character(len=10) :: time
    call date_and_time(date=date, time=time)
    get_dateandtime = date(1:4)//'.'//date(5:6)//'.'//date(7:8)//'-'&
                   &//time(1:2)//':'//time(3:4)//':'//time(5:6)
  end function get_dateandtime


  function get_index(key)
    character(len=KEYLEN), intent(in) :: key
    ![ char string name (key)-->index integer, Returns 0 when it's new.]
    integer :: get_index
    integer :: i
    get_index = 0
    do i = 1 , key_n
      if (ttb(i)%keyname == key) then
        get_index = i
        return
      end if
    end do
  end function get_index


  function get_subindex(i, subkey)
    integer, intent(in) :: i
    ! [ char string name (subkey)-->index integer, Returns 0 when new.]
    character(len=KEYLEN), intent(in) :: subkey
    integer :: get_subindex
    integer :: j
    get_subindex = 0
    do j = 1 , ttb(i)%sub_key_n
      if(ttb(i)%sub_keyname(j) == subkey) then
        get_subindex = j
        return
      end if
    end do
  end function get_subindex

  subroutine printer
    !  [ Print out the timer results. List them in formatted. ]
    integer :: i, j, k, dummy, tick
    integer :: ttl       ! total clock count
    integer :: sbttl     ! subtotal clock count
    real :: ts           ! total seconds
    integer :: h,m,s     ! hour, minute, second

    integer :: ttmp

    if (jump_flag) return                           ! Error happend before.

    call system_clock(dummy,tick)
    dateandtime_end = get_dateandtime()
    print '(50a1)', ('*',i=1,50)                    ! Horizontal line
    print '(7x,a30,i2)',"CPU Time Table.    total key =",key_n
    print '(2x,a19," --> ",a19)', dateandtime_start, dateandtime_end
    print '(50a1)', ('*',i=1,50)                    ! Horizontal line
    do i = 1 , key_n
! debug s
! print *,'[watch] i = ', i, ttb(i)%keyname
! debug e
      ttl = ttb(i)%total_tim
      print '(/8x,a5,a6,a5,1x,i9,a8)',                      & ! Title
                        "///  ",ttb(i)%keyname,"  ///",     &
                                ttb(i)%call_cntr," call"
      if(.not. ttb(i)%pair_check) then
        print *," <watch> 'E' isn't called."    ! Call error.
        cycle                                ! Ignore this.
      end if
      print '(2x,40a1)', ('-',k=1,40)        ! Horizontal line
      do j = 1 , ttb(i)%sub_key_n            ! Sub procedure loop
        sbttl = ttb(i)%sub_total_tim(j)      ! Subtotal tick number
        print '(3x,a6,a3,f10.3,a4,3x,a1,f7.3,a3)',           &
                       ttb(i)%sub_keyname(j)," : ",          &
                       real(sbttl)/real(tick)," sec",        &
                    "(",real(sbttl)/real(ttl)*100," %)"
      end do
      print '(2x,40a1)', ('-',k=1,40)        ! Horizontal line
      ts = real(ttl)/real(tick)              ! Make it second
      h = int(ts) / 3600                     ! Hour
      m = mod(int(ts),3600) / 60             ! Minute
      s = int(ts)-h*3600-m*60                ! Second
      print '(3x,a9,f10.3,a4,3x,a1,3(i2.2,a1),a1)',           &
                       "TOTAL  : ",ts," sec",                 &
                       "[",h,"h",m,"m",s,"s","]"
      print '(2x,40a1)', ('-',k=1,40)        ! Horizontal line
    end do
    call system_clock(job_end_time)
    print '(/50a1)', ('*',i=1,50)            ! Horizontal line
    print '(3x,"         TOTAL LOOP = ",i12)',loop_counter
    ttmp = job_end_time - job_start_time
    if(ttmp<0) then
      ttmp = ttmp + clock_counter
    end if
    print '(3x,"             1 STEP : ",f12.5," sec")', &
                   real(ttmp)/real(tick)/loop_counter
    print '(50a1)', ('*',i=1,50)            ! Horizontal line
  end subroutine printer


  subroutine start_or_end(key_, stt_or_end)
    character(len=KEYLEN), intent(in) :: key_
    integer, intent(in) :: stt_or_end
    ! [ Called in the first and the last as a pair in a program unit. ]

    integer :: ctmp
    logical :: just_once = .true.
    integer :: i,c                            ! i=work, c=clock ticks
    character(len=KEYLEN):: key
    if (just_once) then 
      ! Get the compiler's default count_max
      call system_clock(job_start_time,  &
                        count_max=clock_counter) ! Get the clock counter.
      dateandtime_start = get_dateandtime()
    just_once = .false. ; end if 
    if(jump_flag) return                      ! Error happend before.
    call system_clock(c)                      ! Get the clock counter.
    key = adjustl(key_)
    i = get_index(key)

    if (i == 0) then
      if (stt_or_end == WATCH_END) then           ! Call error.
        print *,"<watch> *** watch__end called before watch_start for key = ",key
        jump_flag = .true.                    ! Error flag.
        return
      end if
      i = key_n + 1                           ! Call is OK, but too much.
      if (i > MAX_KEY_N) then                 ! Too many keys?
        print *,"<watch>  *** over max key number"  ! Must increase MAX_KEY_N.
        jump_flag = .true.                    ! Error flag.
        return
      end if
      key_n = i
      ttb(i)%keyname = key
    end if
    select case (stt_or_end)
    case (WATCH_STT)
      ttb(i)%call_cntr    = ttb(i)%call_cntr + 1
      ttb(i)%stt_tim      = c
      ttb(i)%sub_last_tim = c
      ttb(i)%pair_check   = .false.
    case (WATCH_END)
      ctmp = c-ttb(i)%stt_tim
      if(ctmp<0) then
        ctmp = ctmp + clock_counter
      end if
      ttb(i)%total_tim = ttb(i)%total_tim + ctmp
      ttb(i)%pair_check = .true.
    end select
  end subroutine start_or_end


  subroutine break_point(key_, subkey_)
    character(len=KEYLEN), intent(in) :: key_, subkey_
    !  [ Called at break points in a program unit. ]
    integer :: i,j,c                    ! (i,j)=work, c=clock ticks
    integer :: ctmp
    character(len=KEYLEN) :: key, subkey
    if (jump_flag) return               ! Error happend before.
    call system_clock(c)                ! Get the clock counter.
    key = adjustl(key_)
    subkey = adjustl(subkey_)
    i = get_index(key)
    if(i == 0) then                     ! Forgot to call start_or_end before.
      print *, '<watch> *** No key for this subkey: key=',key,' subkey=',subkey
      jump_flag = .true.                ! Error flag on.
      return
    end if
    j = get_subindex(i,subkey)
    if (j == 0) then
      j = ttb(i)%sub_key_n + 1
      if (j > MAX_SUBKEY_N) then
        print *,"<watch> *** over max sub key number." ! Increase MAX_SUBKEY_N.
        print *,"                     at key = ", key
        jump_flag = .true.              ! Error flag on.
        return
      end if
      ttb(i)%sub_key_n = j
      ttb(i)%sub_keyname(j) = subkey
    end if
    ctmp = c-ttb(i)%sub_last_tim
    if(ctmp<0) then
      ctmp = ctmp + clock_counter 
    end if
    ttb(i)%sub_total_tim(j) = ttb(i)%sub_total_tim(j) + ctmp
    call system_clock(c)
    ttb(i)%sub_last_tim = c
  end subroutine break_point


!--------------
!--- Public --
!--------------

  subroutine watch__count
    loop_counter = loop_counter + 1
  end subroutine watch__count

  subroutine watch__end(key_)
    character(len=KEYLEN), intent(in) :: key_
    call start_or_end(key_, WATCH_END)
  end subroutine watch__end


  subroutine watch__print
    call printer
  end subroutine watch__print

  subroutine watch__start(key_)
    character(len=KEYLEN), intent(in) :: key_
    ! [ Called first and last in a pair in a program unit. ]
    call start_or_end(key_, WATCH_STT)
  end subroutine watch__start

end module watch_m
