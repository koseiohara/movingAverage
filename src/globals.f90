module globals

    implicit none

    integer, parameter :: kp = 4

    integer, save :: nx
    integer, save :: ny
    integer, save :: nz

    integer, save :: filterlen
    character(32), save :: filtername

    integer, save :: varnum
    integer, save :: irec_init
    integer, save :: tnum

    character(128), save :: input_fname
    character(128), save :: output_fname

end module globals

