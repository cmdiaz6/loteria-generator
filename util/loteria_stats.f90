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
    integer, allocatable :: cards(:,:,:)
    logical, allocatable :: card_hits(:,:,:), used_cards(:)
    integer :: icard, iboard, irow, icol, inum, ierr
    integer :: iwinner, num_calls
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

    allocate( win_counts(ngames), stat=ierr )
    if (ierr /=0) print *, 'ERROR: allocating ngames'
    win_counts(:) = 0


    gameloop: do igame = 1, ngames

        cards(:,:,:) = 0
        card_hits(:,:,:) = .false.
        used_cards(:) = .false.

        ! fill cards randomly
        do iboard = 1,nboards
            do irow = 1, 4
                do icol = 1, 4
                    do
                        ! pick a number
                        inum = random_card( ncards )
                        ! if it's already picked try a new number
                        ! todo: optimize this by popping used options out of an array like in the secret santa program
                        if ( any(cards(:,:,iboard) == inum) ) then
                            !print *, iboard, 'oops! hit the same number', inum, 'trying again'
                            cycle
                        else
                            cards( icol, irow, iboard ) = inum
                            exit
                        end if

                    end do
                end do
            end do
            !print *, iboard, ' - output: ', cards(iboard,:,:)
        end do

        ! loop over card calls
        cardloop: do icard = 1,ncards
            ! call random number from 1 to 54
            do
                inum = random_card( ncards )
                if ( used_cards(inum) ) then
                    cycle ! try again
                else
                    used_cards(inum) = .true.
                    exit
                end if
            end do

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

                if (win) then
                    iwinner = iboard
                    num_calls = icard
                    !print *,'someone won in',num_calls
                    !print *,cards(:,:,iboard)
                    !print *,used_cards(:)
                    exit cardloop
                end if
            end do
        end do cardloop

        win_counts(igame) = num_calls

    end do gameloop


    ! print stats
    !print *, win_counts(:)
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



    ! write to file for plotting
    open(newunit=iunit, file='WIN_COUNTS.txt', form='formatted', status='unknown')
    rewind(iunit)
    write(iunit,*) win_counts(:)
    close(iunit)


end program
