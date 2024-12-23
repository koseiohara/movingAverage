module globals

    implicit none

    integer, parameter :: kp = 4

    integer :: nx
    integer :: ny
    integer :: nz

    integer :: filterlen
    integer :: filtype_specifier
    character(32) :: filtername
    logical :: odd
    character(16) :: even_center

    integer :: varnum
    integer :: irec_init
    integer :: tnum

    character(128) :: input_fname
    character(128) :: output_fname

    character(16)  :: variable
    character(16)  :: datetime_init
    character(256) :: options

    integer, parameter :: nzmax = 100
    real(4) :: xmin
    real(4) :: ymin
    real(4) :: xstep
    real(4) :: ystep
    real(4) :: zlevels(nzmax)
    integer :: tstep


end module globals

