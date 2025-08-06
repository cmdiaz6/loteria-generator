module loteria_functions
    contains
        function random_card( ncards ) result( irand )
            implicit none
            integer, intent(IN) :: ncards
            integer :: irand
            real :: rrand

            call random_seed()
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
    integer :: iwinner, num_calls, ncards_left
    logical :: win

    !inputs
    character(len=15) :: input_file = 'stat_inputs.nml'
    character(len=8)  :: win_condition
    !integer, parameter :: nboards = 50, ncards = 54, ngames=10000
    integer :: nboards, ncards, ngames

    ! stats
    integer, allocatable :: win_counts(:)
    integer :: igame, iunit
    real    :: avg, stddev

    namelist /input_data/ win_condition, nboards, ngames, ncards

    !win_condition='any_four'
    open(newunit=iunit,file=input_file,iostat=ierr)
    if (ierr /=0) print *, 'ERROR: namelist',input_file,'not found'
    read(unit=iunit, nml=input_data)
    if (ierr /=0) print *, 'ERROR: error reading namelist',input_file
    close(iunit)
    print *, 'inputs:',win_condition, nboards, ngames, ncards


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


    gameloop: do igame = 1, ngames

        cards(:,:,:) = 0
        card_hits(:,:,:) = .false.
        used_cards(:) = .false.
        win = .false.

        ! fill cards randomly
        do iboard = 1,nboards
            ! keep track of used calls
            do icard = 1, ncards
                free_list(icard) = icard
            end do
            ncards_left = ncards

            do irow = 1, 4
                do icol = 1, 4

                    ncards_left = ncards + 1 - ( (irow-1)*4 + icol )
                    ! call random number from 1 to however many calls are left
                    ipos = random_card( ncards_left )
                    inum = free_list( ipos )

                    ! fill board
                    cards( icol, irow, iboard ) = inum

                    ! update list of free cards. move last card to free'd spot
                    if ( icard /= ncards_left ) then
                      free_list(ipos) = free_list(ncards_left)
                    end if
                    free_list(ncards_left) = 0
                    !write(6,'(54I3)') free_list

                end do
            end do
        end do

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
                select case ( trim(win_condition) )
                case ('any_four')
                    win = check_anyfour( card_hits(:,:,iboard) )
                case ('blackout')
                    win = check_blackout( card_hits(:,:,iboard) )
                case ('corners')
                    win = check_corners( card_hits(:,:,iboard) )
                case default
                    print *, 'ERROR: win_condition ', win_condition, ' does not exist'
                end select

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
            end do
        end do cardloop

        if (.not. win) then
            print *,'ERROR: no one won'
            print *,num_calls
            stop
        end if

        win_counts(igame) = num_calls

    end do gameloop


    ! print stats
    print *, 'win condition:   ', win_condition
    print *, 'number of games: ', ngames

    avg = 1.d0 * sum(win_counts) / ngames
    print *, 'Average win time:', avg

    ! std dev
    stddev = 0.0d0
    do igame = 1, ngames
      stddev = stddev + (1.d0*win_counts(igame) - avg)**2
    end do
    stddev = sqrt(stddev/(ngames-1))

    print *,'1 std dev', avg-stddev/2, ' to ', avg+stddev/2, ' (68% of cases)'
    print *,'2 std dev', avg-stddev,   ' to ', avg+stddev,   ' (95% of cases)'

    ! write to file for plotting later
    open(newunit=iunit, file='WIN_COUNTS.txt', form='formatted', status='unknown')
    rewind(iunit)
    write(iunit,*) win_counts(:)
    close(iunit)


end program
