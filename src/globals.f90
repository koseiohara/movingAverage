module globals

    implicit none

    integer, parameter :: kp = 4

    integer :: nx
    integer :: ny
    integer :: nz

    integer :: filterlen
    integer :: filtype_specifier
    character(32) :: filtername

    integer :: varnum
    integer :: irec_init
    integer :: tnum

    character(128) :: input_fname
    character(128) :: output_fname

    character(16) :: variable
    character(16) :: datetime_init


end module globals

