module namelist

    use globals, only : nx, ny, nz             , &
                      & filterlen, filtername  , &
                      & varnum, irec_init, tnum, &
                      & input_fname, output_fname

    implicit none

    private
    public :: read_nml, minuteTaker

    contains


    subroutine read_nml()

        integer, parameter :: nml_unit = 5

        namelist / grid / nx, ny, nz
        namelist / filter / filterlen, filtername
        namelist / recinfo / varnum, irec_init, tnum
        namelist / files / input_fname, output_fname

        nx = 0
        ny = 0
        nz = 0

        filterlen  = 0
        filtername = ''

        varnum    = 0
        irec_init = 0
        tnum      = 0

        input_fname  = ''
        output_fname = ''


        read(nml_unit, nml=  files)
        read(nml_unit, nml=recinfo)
        read(nml_unit, nml=   grid)
        read(nml_unit, nml= filter)


        call checker()

    end subroutine read_nml
    

    subroutine open_nml(unit, fname)
        integer     , intent(out) :: unit
        character(*), intent(in)  :: fname
        
        integer :: stat

        open(NEWUNIT=unit , &
           & FILE=fname   , &
           & ACTION='READ', &
           & IOSTAT=stat    )

        if (stat /= 0) then
            write(*,*)
            write(*,'(a)')    'OpenFileError ---------------------------------------------'
            write(*,'(a)')    '|   Failed to open file'
            write(*,'(a)')    '|'
            write(*,'(a,i0)') '|   UNIT      : ', unit
            write(*,'(a)')    '|   FILE NAME : ' // trim(fname)
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

    end subroutine open_nml


    subroutine checker()
        
        if (nx <= 0) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid nx value in namelist'
            write(*,'(a)')    '|   nx must be more than 0'
            write(*,'(a,i0)') '|   Input : nx=', nx
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (ny <= 0) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid ny value in namelist'
            write(*,'(a)')    '|   ny must be more than 0'
            write(*,'(a,i0)') '|   Input : ny=', ny
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (nz <= 0) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid nz value in namelist'
            write(*,'(a)')    '|   nz must be more than 0'
            write(*,'(a,i0)') '|   Input : nz=', nz
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (filterlen <= 0) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid filterlen value in namelist'
            write(*,'(a)')    '|   filterlen must be more than 0'
            write(*,'(a,i0)') '|   Input : filterlen=', filterlen
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (mod(filterlen,2) == 0) then
            write(*,*)
            write(*,'(a)')    'InputError ------------------------------------------------'
            write(*,'(a)')    '|   Invalid filterlen value in namelist'
            write(*,'(a)')    '|   filterlen must be an odd number, but input is even'
            write(*,'(a,i0)') '|   Input : filterlen=', filterlen
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (filtername /= 'simple') then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid filtername in namelist'
            write(*,'(a)')    '|   Input : filtername=' // trim(filtername)
            write(*,'(a)')    '|   Available Filter Name are below :'
            write(*,'(a)')    '|       "simple"'
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (varnum <= 0) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid varnum value in namelist'
            write(*,'(a)')    '|   varnum must be more than 0'
            write(*,'(a,i0)') '|   Input : varnum=', varnum
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (irec_init<= 0) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid irec_init value in namelist'
            write(*,'(a)')    '|   irec_init must be more than 0'
            write(*,'(a,i0)') '|   Input : irec_init=', irec_init
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (tnum < filterlen) then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid tnum value in namelist'
            write(*,'(a)')    '|   tnum must be equal or more than filterlen'
            write(*,'(a,i0)') '|   Input : tnum=', tnum
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (input_fname == '') then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid input_fname in namelist'
            write(*,'(a)')    '|   Input_fname is not specified'
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

        if (output_fname == '') then
            write(*,*)
            write(*,'(a)')    'InputError -----------------------------------------------'
            write(*,'(a)')    '|   Invalid output_fname in namelist'
            write(*,'(a)')    '|   output_fname is not specified'
            write(*,'(a)')    '-----------------------------------------------------------'

            ERROR STOP
        endif

    end subroutine checker


    subroutine minuteTaker()
        
        write(*,*)
        write(*,'(a)')    '---'
        write(*,'(a)')    'INPUT  FILE : ' // trim(input_fname)
        write(*,'(a)')    'OUTPUT FILE : ' // trim(output_fname)
        write(*,'(a)')    '---'
        write(*,'(a,i0)') 'NX = ', nx
        write(*,'(a,i0)') 'NY = ', ny
        write(*,'(a,i0)') 'NZ = ', nz
        write(*,'(a)')    '---'
        write(*,'(a,i0)') 'FILTER LENGTH : ', filterlen
        write(*,'(a)')    'FILTER NAME   : ' // trim(filtername)
        write(*,'(a)')    '---'
        write(*,'(a,i0)') 'NUMBER OF VARIABLES     : ', varnum
        write(*,'(a,i0)') 'INITIAL RECORD OF INPUT : ', irec_init
        write(*,'(a,i0)') 'NUMBER OF TIME STEPS    : ', tnum
        write(*,'(a)')    '---'
        write(*,*)

    end subroutine minuteTaker

    
end module namelist

