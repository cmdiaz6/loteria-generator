module loteria_functions
    implicit none
    contains

        subroutine tick(t)
            integer, intent(OUT) :: t
            call system_clock(t)
        end subroutine
        real function tock(t)
            integer, intent(in) :: t
            integer :: now
            integer :: clock_rate, clock_max

            call system_clock(now,count_rate=clock_rate,count_max=clock_max)
            if (now<t) then
                tock = real(clock_max - (now - t))/real(clock_rate)
            else
                tock = real(now - t)/real(clock_rate)
            end if
        end function

        function random_card( ncards ) result( irand )
            implicit none
            integer, intent(IN) :: ncards
            integer :: irand
            real :: rrand

            call random_number( rrand )
            irand = int( rrand * ncards ) + 1
        end function

        pure function check_anyfour( card ) result( win )
            implicit none
            logical, intent(IN) :: card(4,4)
            logical :: win, diag, antidiag
            integer :: iloop

            win = .false.
            do iloop = 1, 4
                ! check row or column is all .true.
                if ( all(card(iloop,:)) .or. all(card(:,iloop)) ) then
                    win = .true.
                    exit
                end if
            end do

            if (win) return

            ! check diagonal and anti-diagonal
            diag = .true.
            antidiag = .true.
            do iloop = 1, 4
                if ( .not. card( iloop, iloop) ) then
                    diag = .false.
                end if
                if ( .not. card( 5-iloop, iloop ) ) then
                    antidiag = .false.
                end if
            end do
            if ( diag .or. antidiag ) win = .true.

        end function

        pure function check_blackout( card ) result( win )
            implicit none
            logical, intent(IN) :: card(4,4)
            logical :: win

            win = .false.
            if ( all(card) ) win = .true.
        end function

        pure function check_corners( card ) result( win )
            implicit none
            logical, intent(IN) :: card(4,4)
            logical :: win

            win = .false.
            if ( card(1,1) .and. card(1,4) .and. card(4,1) .and. card(4,4) )  win = .true.
        end function

end module

program loteria_stats
    use loteria_functions
    implicit none
    integer, allocatable :: cards(:,:,:), free_list(:)
    logical, allocatable :: card_hits(:,:,:), used_cards(:)
    integer :: icard, iboard, irow, icol, inum, ipos, ierr
    integer :: iwinner, num_calls, ncards_left, check_start
    integer :: ncardsp1, ntmp
    logical :: win

    !inputs
    character(len=15) :: input_file = 'stat_inputs.nml'
    character(len=8)  :: win_condition
    !integer, parameter :: nboards = 50, ncards = 54, ngames=10000
    integer :: nboards, ncards, ngames

    ! stats
    integer, allocatable :: win_counts(:), bins(:)
    integer :: igame, iunit
    real    :: avg, stddev

    ! timings
    integer :: itime, itime2
    real    :: fill_time, check_time, random_time

    namelist /input_data/ win_condition, nboards, ngames, ncards

    ! initialize rng
    call random_seed()

    !win_condition='any_four'
    open(newunit=iunit,file=input_file,iostat=ierr)
    if (ierr /=0) print *, 'ERROR: namelist',input_file,'not found'
    read(unit=iunit, nml=input_data)
    if (ierr /=0) print *, 'ERROR: error reading namelist',input_file
    close(iunit)
    write(6,'(A,I10,3A,I5,A)') 'Running ',ngames,' simulations of ', win_condition, ' Loteria with ', nboards,' players'


    allocate( cards( 4, 4, nboards), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating cards'
    allocate( card_hits( 4, 4, nboards), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating card_hits'
    allocate( used_cards( ncards), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating used_cards'
    allocate( free_list( ncards), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating free_list'

    allocate( win_counts(ngames), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating ngames'
    win_counts(:) = 0

    ! don't bother checking if you don't have the minimum calls needed
    select case ( trim(win_condition) )
    case ('any_four', 'corners')
        check_start = 4
    case ('blackout')
        check_start = 16
    case default
        check_start = 1
    end select

    fill_time  = 0.0
    check_time = 0.0
    random_time = 0.0

    ncardsp1 = ncards + 1

    gameloop: do igame = 1, ngames

        cards(:,:,:) = 0
        card_hits(:,:,:) = .false.
        used_cards(:) = .false.
        win = .false.

        call tick(itime)
        ! fill cards randomly
        do iboard = 1,nboards
            ! keep track of used calls
            do icard = 1, ncards
                free_list(icard) = icard
            end do
            ncards_left = ncards

            do irow = 1, 4
                ntmp = ncardsp1 - (irow-1)*4
                do icol = 1, 4

                    !ncards_left = nc1 - ( (irow-1)*4 + icol )
                    ncards_left = ntmp - icol
                    ! call random number from 1 to however many calls are left
                    call tick(itime2)
                    ipos = random_card( ncards_left )
                    random_time = random_time + tock(itime2)

                    ! fill board
                    cards( icol, irow, iboard ) = free_list( ipos )

                    ! update list of free cards. move last card to free'd spot
                    if ( icard /= ncards_left ) free_list(ipos) = free_list(ncards_left)
                    free_list(ncards_left) = 0
                    !write(6,'(54I3)') free_list

                    !ncards_left = ncards_left - 1

                end do
            end do
        end do ! nboards
        
        fill_time = fill_time + tock(itime)
        call tick(itime)


        ! keep track of used calls
        do icard = 1, ncards
            free_list(icard) = icard
        end do
        ncards_left = ncards

        ! loop over card calls
        cardloop: do icard = 1,ncards

            ! call random number from 1 to however many calls are left
            ncards_left = ncards - icard + 1
            ipos = random_card( ncards_left ) ! get random position in free_list
            inum = free_list(ipos)            ! card number stored in free_list
            if (inum == 0) then
                print *,'ERROR: free_list(ipos) =',inum
                print *,'ipos=',ipos
            end if

            ! update list of free cards. move last card to free'd spot
            if ( inum /= ncards_left ) then
              free_list(ipos) = free_list(ncards_left)
            end if
            free_list(ncards_left) = 0

            ! loop over all boards
            do iboard = 1, nboards

                ! add your bean if it's on your board
                rowloop: do irow = 1, 4
                  do icol = 1, 4
                      if ( cards( icol, irow, iboard ) == inum ) then
                          card_hits( icol, irow, iboard ) = .true.
                          exit rowloop
                      end if
                  end do
                end do rowloop

                ! check if card matches winning conditions
                if ( icard >= check_start ) then
                    select case ( trim(win_condition) )
                    case ('any_four')
                        win = check_anyfour(  card_hits(:,:,iboard) )
                    case ('blackout')
                        win = check_blackout( card_hits(:,:,iboard) )
                    case ('corners')
                        win = check_corners(  card_hits(:,:,iboard) )
                    case default
                        print *, 'ERROR: win_condition ', win_condition, ' does not exist'
                    end select
                end if

                !print *,'--- board',iboard,'---'
                !do irow = 1, 4
                !    if (iboard==1) write(6,*) (card_hits( icol, irow, iboard ), icol = 1, 4)
                !end do

                if (win) then
                    iwinner = iboard
                    num_calls = icard
                    !print *,'someone won in',num_calls
                    !write(6,'(54I3)') used_cards(:)
                    exit cardloop
                end if
            end do ! nboards

        end do cardloop

        check_time = check_time + tock(itime)

        if (.not. win) then
            print *,'ERROR: no one won'
            print *,num_calls
            stop
        end if

        win_counts(igame) = num_calls

    end do gameloop


    ! print stats
    avg = 1.d0 * sum(win_counts) / ngames
    print *,'RESULTS'
    write(6,'(A,F10.2)') 'Average win time:', avg
    write(6,'(A,F10.2)') 

    ! std dev
    stddev = 0.0d0
    do igame = 1, ngames
      stddev = stddev + (1.d0*win_counts(igame) - avg)**2
    end do
    stddev = sqrt(stddev/(ngames-1))

    write(6,'(A,F10.2,A,F10.2,A)') '1 std dev ', avg-stddev/2, ' to ', avg+stddev/2, ' (68% of cases)'
    write(6,'(A,F10.2,A,F10.2,A)') '2 std dev ', avg-stddev,   ' to ', avg+stddev,   ' (95% of cases)'

    ! print timings
    !print *,''
    !print *,'TIMINGS'
    !write(6,'(A,F10.2)') 'fill time ', fill_time
    !write(6,'(A,F10.2)') 'check time', check_time 
    !write(6,'(A,F10.2)') 'random time', random_time 

    ! create bins for plotting
    allocate( bins(ngames), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating bins'
    bins(:) = 0
    do igame = 1, ngames
      num_calls = win_counts(igame)
      bins(num_calls) = bins(num_calls) + 1
    end do

    open(newunit=iunit, file='WIN_COUNTS.txt', form='formatted', status='unknown')
    rewind(iunit)
    write(iunit,*) 'calls  #_wins'
    do icard = 1, ncards
      write(iunit,*) icard, bins(icard)
    end do
    close(iunit)

    ! bye
end program
