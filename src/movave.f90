module movave

    use fileio       , only : finfo                                   , &
                            & fopen ,fclose, fread, fwrite            , &
                            & reset_record, get_record
    use debugger     , only : debug_open, debug_close                 , &
                            & debug_write, debug_linebreak
    use globals      , only : kp, nx, ny, nz                          , &
                            & filterlen, filtype_specifier, filtername, &
                            & varnum, irec_init, tnum                 , &
                            & input_fname, output_fname
    use movave_filter, only : filter, get_edgeCorrector

    implicit none

    private
    public :: movingAverage

    integer :: debug_rec_unit
    integer :: debug_fil_unit
    character(64), parameter :: debug_rec_fname='../output/DebugFile_Record.txt'
    character(64), parameter :: debug_fil_fname='../output/DebugFile_FilIdx.txt'

    contains


    subroutine movingAverage()
        type(finfo) :: input_file
        type(finfo) :: output_file

        call fopen(ftype  =input_file  , &  !! OUT
                 & fname  =input_fname , &  !! IN
                 & action ='read'      , &  !! IN
                 & recl   =kp*nx*ny*nz , &  !! IN
                 & record =irec_init   , &  !! IN
                 & recstep=0             )  !! IN
        
        call fopen(ftype  =output_file , &  !! OUT
                 & fname  =output_fname, &  !! IN
                 & action ='write'     , &  !! IN
                 & recl   =kp*nx*ny*nz , &  !! IN
                 & record =1           , &  !! IN
                 & recstep=1             )  !! IN

        call debug_open(debug_rec_unit, &  !! OUT
                      & debug_rec_fname )  !! IN
        call debug_open(debug_fil_unit, &  !! OUT
                      & debug_fil_fname )  !! IN


        call movingAverage_edge(input_file  , &  !! INOUT
                              & output_file , &  !! INOUT
                              & 'start'       )  !! IN

        call movingAverage_middle(input_file, &  !! INOUT
                                & output_file )  !! INOUT

        call movingAverage_edge(input_file  , &  !! INOUT
                              & output_file , &  !! INOUT
                              & 'end'         )  !! IN


        call debug_close(debug_rec_unit)  !! IN
        call debug_close(debug_fil_unit)  !! IN

        call fclose( input_file)  !! INOUT
        call fclose(output_file)  !! INOUT

    end subroutine movingAverage


    subroutine movingAverage_Edge(input_file, output_file, whichSide)
        type(finfo) , intent(inout) :: input_file
        type(finfo) , intent(inout) :: output_file
        character(*), intent(in)    :: whichSide

        real(kp) :: reader(nx,ny,nz)
        real(kp) :: aved(nx,ny,nz)
        real(kp) :: weight(filterlen)

        real(kp) :: edgeCorrector

        integer :: edgeLen
        integer :: tcount
        integer :: filcount

        integer :: center_rec
        integer :: records(filterlen)

        integer :: filter_firstIndex
        integer :: filter_lastIndex

        
        !! Length of the Start Edge
        edgeLen = ishft(filterlen, -1)
        !! Get Filter
        call filter(weight(1:filterlen))  !! OUT

        if (whichSide == 'start') then
            filter_firstIndex = edgeLen+1
            filter_lastIndex  = filterLen
        else if (whichSide == 'end') then
            filter_firstIndex = 1
            filter_lastIndex  = edgeLen+1
        else
            write(*,'(a)') 'Error : Invalid argument in whichSide'
            ERROR STOP
        endif

        center_rec = get_record(input_file)

        do tcount = 1, edgeLen
            aved(1:nx,1:ny,1:nz) = 0._kp

            call chooseRecords(center_rec        , &  !! IN
                             & records(1:filterlen))  !! OUT

            if (whichSide == 'start') then
                filter_firstIndex = edgeLen+2 - tcount
                filter_lastIndex  = filterLen
            else 
                filter_firstIndex = 1
                filter_lastIndex  = filterLen - tcount
            endif

            !! Compute moving Averaged Field at t=tcount
            do filcount = filter_firstIndex, filter_lastIndex
                call reset_record(input_file               , &  !! INOUT
                                & newrecord=records(filcount))  !! IN

                call debug_write(debug_rec_unit , &  !! IN
                               & input_file%record)  !! IN
                call debug_write(debug_fil_unit , &  !! IN
                               & filcount         )  !! IN

                call fread(input_file          , &  !! INOUT
                         & reader(1:nx,1:ny,1:nz))  !! OUT

                aved(1:nx,1:ny,1:nz) = aved(1:nx,1:ny,1:nz) + reader(1:nx,1:ny,1:nz) * weight(filcount)

            enddo

            call get_edgeCorrector(weight(1:filterlen), &  !! IN
                                 & filter_firstIndex  , &  !! IN
                                 & filter_lastIndex   , &  !! IN
                                 & edgeCorrector        )  !! OUT

            !! Edge Correction
            aved(1:nx,1:ny,1:nz) = aved(1:nx,1:ny,1:nz) / edgeCorrector
            call fwrite(output_file       , &  !! INOUT
                      & aved(1:nx,1:ny,1:nz))  !! IN

            call mklog(center_rec)  !! IN

            !! Input Record to hte next step
            center_rec = center_rec + varnum

            call debug_linebreak(debug_rec_unit)  !! IN
            call debug_linebreak(debug_fil_unit)  !! IN

        enddo

        call reset_record(input_file          , &  !! INOUT
                        & newrecord=center_rec  )  !! IN

    end subroutine movingAverage_Edge


    subroutine movingAverage_middle(input_file, output_file)
        type(finfo), intent(inout) :: input_file
        type(finfo), intent(inout) :: output_file

        real(kp) :: reader(nx,ny,nz)
        real(kp) :: aved(nx,ny,nz)
        real(kp) :: weight(filterlen)

        integer :: edgeLen

        integer :: center_rec
        integer :: records(filterlen)

        integer :: filcount
        integer :: tcount
        integer :: tend

        !! Length of the Start Edge
        edgeLen = ishft(filterlen, -1)
        !! Get Filter
        call filter(weight(1:filterlen))  !! OUT

        tend = tnum - filterlen + 1

        center_rec = get_record(input_file)

        do tcount = 1, tend
            aved(1:nx,1:ny,1:nz) = 0._kp

            call chooseRecords(center_rec        , &  !! IN
                             & records(1:filterlen))  !! OUT

            do filcount = 1, filterlen
                call reset_record(input_file               , &  !! INOUT
                                & newrecord=records(filcount))  !! IN

                call debug_write(debug_rec_unit , &  !! IN
                               & input_file%record)  !! IN
                call debug_write(debug_fil_unit , &  !! IN
                               & filcount         )  !! IN

                call fread(input_file          , &  !! INOUT
                         & reader(1:nx,1:ny,1:nz))  !! OUT

                aved(1:nx,1:ny,1:nz) = aved(1:nx,1:ny,1:nz) + reader(1:nx,1:ny,1:nz) * weight(filcount)

            enddo

            call fwrite(output_file       , &  !! INOUT
                      & aved(1:nx,1:ny,1:nz))  !! IN

            call mklog(center_rec)  !! IN

            !! Input Record to the next step
            center_rec = center_rec + varnum

            call debug_linebreak(debug_rec_unit)  !! IN
            call debug_linebreak(debug_fil_unit)  !! IN

        enddo

        call reset_record(input_file          , &  !! INOUT
                        & newrecord=center_rec  )  !! IN

    end subroutine movingAverage_middle


    subroutine chooseRecords(center_record, records)
        integer, intent(in) :: center_record
        integer, intent(out) :: records(filterlen)

        integer :: winglen
        integer :: i


        winglen = ishft(filterlen, -1)

        records(1:filterlen) = [(center_record + i*varnum, i=-winglen, winglen+filtype_specifier)]

    end subroutine chooseRecords


    subroutine mklog(t)
        integer, intent(in) :: t

        write(*,'(a,i8,a,i8)') 'COMPLETE ', t, ' / ', tnum

    end subroutine mklog

        
end module movave

