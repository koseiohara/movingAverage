module namelist

    use globals, only : nx, ny, nz                                                , &
                      & filterlen, filtype_specifier, filtername, odd, even_center, &
                      & varnum, irec_init, tnum                                   , &
                      & input_fname, output_fname                                 , &
                      & variable, datetime_init, options                          , &
                      & nzmax, xmin, ymin, xstep, ystep, zlevels, tstep

    implicit none

    private
    public :: read_nml, minuteTaker

    real(4), parameter :: real32_error = -9999.
    integer, parameter :: int32_error  = -9999

    contains


    subroutine read_nml()

        integer, parameter :: nml_unit = 5

        namelist / grid    / nx, ny, nz
        namelist / filter  / filterlen, filtername, even_center
        namelist / recinfo / varnum, irec_init, tnum
        namelist / files   / input_fname, output_fname
        namelist / control / variable, datetime_init, options, xmin, ymin, xstep, ystep, zlevels, tstep

        nx = 0
        ny = 0
        nz = 0

        filterlen   = 0
        filtername  = ''
        even_center = ''

        varnum    = 0
        irec_init = 0
        tnum      = 0

        input_fname  = ''
        output_fname = ''

        variable      = ''
        datetime_init = ''
        options       = ''
        xmin          = real32_error
        ymin          = real32_error
        xstep         = real32_error
        ystep         = real32_error
        zlevels(1:nzmax) = real32_error
        tstep         = int32_error


        read(nml_unit, nml=  files)
        read(nml_unit, nml=recinfo)
        read(nml_unit, nml=   grid)
        read(nml_unit, nml= filter)
        read(nml_unit, nml=control)

        call checker()

        filtype_specifier = - mod(filterlen+1, 2)
        odd = (mod(filterlen, 2) == 1)

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
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "nx" value in namelist'
            write(0,'(A)')    '"nx" must be more than 0'
            write(0,'(A,I0)') 'Input : nx=', nx
            ERROR STOP
        endif

        if (ny <= 0) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "ny" value in namelist'
            write(0,'(A)')    '"ny" must be more than 0'
            write(0,'(A,I0)') 'Input : ny=', ny
            ERROR STOP
        endif

        if (nz <= 0 .OR. nz > nzmax) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "nz" value in namelist'
            write(0,'(A,I0)') '"nz" must be between 1 and ', nzmax
            write(0,'(A,I0)') 'Input : nz=', nz
            ERROR STOP
        endif

        if (filterlen <= 0) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "filterlen" value in namelist'
            write(0,'(A)')    '"filterlen" must be more than 0'
            write(0,'(A,I0)') 'Input : filterlen=', filterlen
            ERROR STOP
        endif

        !if (mod(filterlen,2) == 0) then
            !write(*,*)
            !write(*,'(a)')    'InputError ------------------------------------------------'
            !write(*,'(a)')    '|   Invalid filterlen value in namelist'
            !write(*,'(a)')    '|   filterlen must be an odd number, but input is even'
            !write(*,'(a,i0)') '|   Input : filterlen=', filterlen
            !write(*,'(a)')    '-----------------------------------------------------------'

            !ERROR STOP
        !endif

        if (filtername /= 'simple') then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "filtername" in namelist'
            write(0,'(A)')    'Input : filtername=' // trim(filtername)
            write(0,'(A)')    'Available Filter Name are below :'
            write(0,'(A)')    '- "simple"'
            ERROR STOP
        endif

        if (mod(filterlen,2) == 0) then
            if (even_center /= 'forward' .AND. even_center /= 'backward') then
                write(0,'(A)')    '<ERROR STOP>'
                write(0,'(A)')    'Invalid "even_center" in namelist'
                write(0,'(A)')    '"forward" and "backward" are available'
                write(0,'(A)')    'Input : even_center=' // trim(even_center)
                ERROR STOP
            endif
        endif

        if (varnum <= 0) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "varnum" value in namelist'
            write(0,'(A)')    '"varnum" must be more than 0'
            write(0,'(A,I0)') 'Input : varnum=', varnum
            ERROR STOP
        endif

        if (irec_init<= 0) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "irec_init" value in namelist'
            write(0,'(A)')    '"irec_init" must be more than 0'
            write(0,'(A,I0)') 'Input : irec_init=', irec_init
            ERROR STOP
        endif

        if (tnum < filterlen) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "tnum" value in namelist'
            write(0,'(A)')    '"tnum" must be equal or more than "filterlen"'
            write(0,'(A,I0)') 'Input : tnum=', tnum
            ERROR STOP
        endif

        if (trim(input_fname) == '') then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "input_fname" in namelist'
            write(0,'(A)')    '"Input_fname" is not specified'
            ERROR STOP
        endif

        if (trim(output_fname) == '') then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "output_fname" in namelist'
            write(0,'(A)')    '"output_fname" is not specified'
            ERROR STOP
        endif

        if (trim(variable) == '') then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "variable" : "variable" is not specified'
            ERROR STOP
        endif

        if (trim(datetime_init) == '') then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "datetime_init" : "datetime_init" is not specified'
            ERROR STOP
        endif

        if (xmin == real32_error) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "xmin" : "xmin" is not specified'
            ERROR STOP
        endif

        if (ymin == real32_error) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "ymin" : "ymin" is not specified'
            ERROR STOP
        endif
        
        if (xstep == real32_error) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "xstep" : "xstep" is not specified'
            ERROR STOP
        endif
        
        if (ystep == real32_error) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "ysetp" : "ystep" is not specified'
            ERROR STOP
        endif
        
        if (all(zlevels(1:nzmax) == real32_error)) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "zlevels" : "zlevels" is not specified'
            ERROR STOP
        endif
        
        if (any(zlevels(1:nz) < 0)) then
            write(0,'(A)')               '<ERROR STOP>'
            write(0,'(A)')               'Invalid "zlevels"'
            write(0,'(A)')               '"zlevels" contains negative value'
            write(0,'(A,*(F0.3,:,","))') 'zlevels(1:nz) = ', zlevels(1:nz)
            write(0,'(A,F0.3,A)')        'Note : ', real32_error, ' is the default value'
            ERROR STOP
        endif
        
        if (zlevels(nz+1) /= real32_error) then
            write(0,'(A)')      '<ERROR STOP>'
            write(0,'(A)')      'Invalid "zlevels"'
            write(0,'(A)')      'The provided "zlevels" is bigger than "nz"'
            write(0,'(A,F0.3)') 'zlevels(nz+1)=', zlevels(nz+1)
            ERROR STOP
        endif
        
        if (tstep == int32_error) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Invalid "tstep" : "tstep" is not specified'
            ERROR STOP
        endif

        if (tstep < 0.) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A)')    'Invalid "tstep" value'
            write(0,'(A,I0)') '"tstep" must be a positive value, but input is ', tstep
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

