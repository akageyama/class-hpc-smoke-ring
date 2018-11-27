!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    Copyright 2018 Akira Kageyama
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    slice_grapher/gnuplot_data_generator.f90
!-------------------------------------------------------------------

program main
  use target_m
  use constants_m
  use ut_m
  use grid_m
  use turtle_m
  implicit none

  integer, parameter :: FILE_FOR_TURTLE = 70
  integer, parameter :: FILE_SLICEDATA = 20

  ! - 2-D single precision real arrays.
  real(SR), dimension(:,:), allocatable :: Slice_vx  ! x-comp. of velocity
  real(SR), dimension(:,:), allocatable :: Slice_vy  ! y-comp.
  real(SR), dimension(:,:), allocatable :: Slice_vz  ! z-comp.
  real(SR), dimension(:,:), allocatable :: Slice_ps  ! Pressure
  real(SR), dimension(:,:), allocatable :: Slice_en  ! Enstrophy

  integer :: draw_loop

  type(turtle__pos_) :: window_lower_left, window_upper_right
  type(turtle__pos_) :: shift

  type contour_info_
     integer  :: nlevels
     real(SR) :: vmin
     real(SR) :: vmax
  end type contour_info_

  call grid%initialize

  allocate(Slice_vx(NX,NZ),   &
           Slice_vy(NX,NZ),   &
           Slice_vz(NX,NZ))
  allocate(Slice_ps(NX,NZ),   &
           Slice_en(NX,NZ))

  window_lower_left%x  = -1.7
  window_lower_left%y  = -0.7
  window_upper_right%x =  1.7
  window_upper_right%y =  0.7

  call turtle__initialize(window_lower_left, window_upper_right)

!  shift%x = 0.0 
!  shift%y = 0.0
!  call turtle__coords_shift(shift)

  read(5,*) draw_loop                 ! Get from stdin

  call read_slice_data(draw_loop)
  call draw_zxplane
  call draw_boundary_box


contains


!________________________________________________________________________
!                                                                        !
  subroutine draw_boundary_box                                           !
!________________________________________________________________________!
!
    type(turtle__pos_) :: corner_southwest, corner_northeast

    corner_southwest%x = grid%pos%x(1)
    corner_southwest%y = grid%pos%y(1)

    corner_northeast%x = grid%pos%x(NX)
    corner_northeast%y = grid%pos%y(NY)

    open(FILE_FOR_TURTLE,                                               &
         file=trim(turtle__filename_for_lines("boundary_box")))

    call turtle__rectangle(corner_southwest,corner_northeast)

    close(FILE_FOR_TURTLE)

  end subroutine draw_boundary_box


!________________________________________________________________________
!                                                                        !
  subroutine draw_zxplane                                                !
!________________________________________________________________________!
!

    open(FILE_FOR_TURTLE,                              &
         file=trim(turtle__filename_for_lines("zxplane_contour_en")))
    call draw_zxplane_contour(Slice_en)
    close(FILE_FOR_TURTLE)

    open(FILE_FOR_TURTLE,                              &
         file=trim(turtle__filename_for_lines("zxplane_contour_ps")))
    call draw_zxplane_contour(Slice_ps)
    close(FILE_FOR_TURTLE)

    open(FILE_FOR_TURTLE,                              &
         file=trim(turtle__filename_for_lines("zxplane_vector_vel")))
    call draw_zxplane_vector(Slice_vx,                 &
                             Slice_vz)
    close(FILE_FOR_TURTLE)

  end subroutine draw_zxplane


!________________________________________________________________________
!                                                                        !
  subroutine draw_zxplane_contour(field)                                 !
    real(SR), intent(in), dimension(:,:) :: field                        !
!________________________________________________________________________!
!
    type(turtle__scalar2d_cartesian_) :: work
    integer :: contour_levels

    contour_levels = 20

    allocate(work%f(NX,NZ),work%xpos(NX),work%ypos(NZ))

    work%nx = NX
    work%ny = NZ

    work%xpos(:) = grid%pos%x(:)
    work%ypos(:) = grid%pos%z(:)
    work%f(:,:)  = field(:,:)

    call turtle__contour_cartesian(work,contour_levels,         &
                                   maxval(work%f),              &
                                   minval(work%f))

    deallocate(work%f,work%xpos,work%ypos)

  end subroutine draw_zxplane_contour


!_______________________________________________________________private__
!                                                                        !
  subroutine draw_zxplane_vector(vector_x,vector_z)                      !
    real(SR), dimension(:,:), intent(in) :: vector_x, vector_z           !
!________________________________________________________________________!
!
    type(turtle__vector2d_cartesian_) :: vec

    allocate(vec%x(NX,NZ))
    allocate(vec%y(NX,NZ))
    allocate(vec%xpos(NX))
    allocate(vec%ypos(NZ))

    vec%nx = NX
    vec%ny = NZ

    vec%xpos(:) = grid%pos%x(:)
    vec%ypos(:) = grid%pos%z(:)

    vec%x(:,:)  = vector_x(:,:)
    vec%y(:,:)  = vector_z(:,:)
    
!    call turtle__vector_cartesian(vec,norm=arrow_norm)
    call turtle__vector_cartesian(vec)

    deallocate(vec%x,vec%y,vec%xpos,vec%ypos)

  end subroutine draw_zxplane_vector


!________________________________________________________________________
!                                                                        !
  subroutine read_slice_data(target_nloop)                               !
    integer, intent(in) :: target_nloop                                  !
!________________________________________________________________________!
!
    integer  :: dummy_nloop
    real(SR) :: dummy_time

    print *,'Opening ', trim(TARGET__FILENAME)

    open(FILE_SLICEDATA,                &
         file=trim(TARGET__FILENAME),   &
         form='unformatted',            &
         status='old')

    do
       read(FILE_SLICEDATA) dummy_nloop, dummy_time,            &
                            Slice_vx, Slice_vy, Slice_vz,       &
                            Slice_ps, Slice_en
       print *, ' dummy_nloop = ', dummy_nloop
       if (dummy_nloop==target_nloop) exit
    end do

    if (dummy_nloop/=target_nloop)                              &
         call ut__fatal('Could not find the target data.')

    close(FILE_SLICEDATA)

  end subroutine read_slice_data

end program main




