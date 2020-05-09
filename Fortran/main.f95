! Tic-Tac-Toe


program tictactoe
     implicit none
   
     integer :: i, move, turn
     character(1), dimension(3,3) :: tictac
     character(1) :: winner, replay
     logical :: over, checkPlay
   
     do
       write(*,*) "Play Tic-Tac-Toe. Enter 1-9 to play:"
       write(*,*) " "
       write(*,*) " 1 | 2 | 3 "
       write(*,*) "---+---+---"
       write(*,*) " 4 | 5 | 6 "
       write(*,*) "---+---+---"
       write(*,*) " 7 | 8 | 9 "
       write(*,*) " "
   
       ! Create the board
       call makeBoard(tictac)
   
       do
         do 
          !turn 0 = player 1
           turn = 0
           write(*,*) "P1 move? "
           read(*,*) move
           if (move < 1 .and. move > 9) then
             write(*,*) "Invalid input."
             cycle
           else if (checkPlay(tictac,move)) then
             exit
           else 
             write(*,*) "Invalid move, box already occupied."
             cycle
           end if
         end do
   
         if (move == 1) tictac(1,1) = "x"
         if (move == 2) tictac(1,2) = "x"
         if (move == 3) tictac(1,3) = "x"
         if (move == 4) tictac(2,1) = "x"
         if (move == 5) tictac(2,2) = "x"
         if (move == 6) tictac(2,3) = "x"
         if (move == 7) tictac(3,1) = "x"
         if (move == 8) tictac(3,2) = "x"
         if (move == 9) tictac(3,3) = "x"
   
         do
           if (turn == 0) write(*,*) "After P1 move..."
           if (turn == 1) write(*,*) "After P2 move..."
           do i=1,3
             write(*, *) " ", tictac(i,1), " | ", tictac(i,2), " | ", tictac(i,3)
             if (i < 3) write(*,*) "---+---+---"
           end do
           call isOver(tictac,over,winner)
           if (over) exit
           if (turn == 1) exit

           !
           do 
               !turn 1 = player 2
               turn = 1
               write(*,*) "P2 move? "
               read(*,*) move
               if (move < 1 .and. move > 9) then
                 write(*,*) "Invalid input."
                 cycle
               else if (checkPlay(tictac,move)) then
                 exit
               else 
                 write(*,*) "Invalid move, box already occupied."
                 cycle
               end if
             end do
       
             if (move == 1) tictac(1,1) = "o"
             if (move == 2) tictac(1,2) = "o"
             if (move == 3) tictac(1,3) = "o"
             if (move == 4) tictac(2,1) = "o"
             if (move == 5) tictac(2,2) = "o"
             if (move == 6) tictac(2,3) = "o"
             if (move == 7) tictac(3,1) = "o"
             if (move == 8) tictac(3,2) = "o"
             if (move == 9) tictac(3,3) = "o"
           !
         end do
         !check if the game is over
         call isOver(tictac,over,winner)
         if (over) exit
       end do
   
       write(*,*) "The game is over!"
       if (winner == "d") then
         write(*,*) "The game is a draw."
       else
         write(*,*) "The winner is: ", winner
       end if
       do
         write(*,*) "Play again? (y/n)"
         read(*,*) replay
         if(replay == "y" .or. replay == "n") exit
         write(*,*) "Invalid input."
       end do
       if(replay == "n") exit
     end do
   end
   
   ! Subroutine to check to see if the game is over.    
   ! =========================================
   subroutine isOver(tictac,over,winner)
     character(1), dimension(3,3) :: tictac
     character(1) :: winner
     logical :: over, isWon, disWon
     integer :: ir, ic
     character(1), parameter :: blank = " ", draw = "d"
   
     ! Assume game is over at start.
     over = .true.
   
     ! Check for a winner.
     ! Check rows for a winner.
     do ir = 1, 3
       if (isWon(tictac(ir,1),tictac(ir,2),tictac(ir,3))) then
         winner = tictac(ir,1)
         return
       end if
     end do
     ! No winner by rows, check columns for a winner.
     do ic = 1, 3
       if (isWon(tictac(1,ic),tictac(2,ic),tictac(3,ic))) then
         winner = tictac(1,ic)
         return
       end if
     end do
     ! No winner by rows or columns, check diagonals.
     disWon = isWon(tictac(1,1),tictac(2,2),tictac(3,3)) .or. isWon(tictac(1,3),tictac(2,2),tictac(3,1)) 
     if (disWon) then
       winner = tictac(2,2)
       return
     end if
     ! No winner at all. See if game is a draw.
     ! Check each row for an empty space.
     do ir = 1,3
       do ic = 1,3
         if (tictac(ir,ic) == blank) then
           over = .false.
           return
         end if
       end do
     end do
   
     ! No blank found, game is a draw.
     winner = draw
   
     return    
   end
   
   ! check if one of the players won
   ! =========================================
   logical function isWon(t1,t2,t3)
     character :: t1,t2,t3
   
     if (t1 == "x" .and. t2 == "x" .and. t3 == "x") then
       isWon = .true.
       return
     else if (t1 == "o" .and. t2 == "o" .and. t3 == "o") then
       isWon = .true. 
     else
       isWon = .false.
     end if
   end
   
   ! Subroutine to create board
   ! =========================================  
   subroutine makeBoard(tictac)
     implicit none
     integer :: i, j
     character(1), dimension(3,3) :: tictac
   
     do i = 1,3
       do j = 1,3
         tictac(i,j) = " "
       end do
     end do
     return
   end
   
   ! Subroutine to check human play.  
   ! ========================================= 
   logical function checkPlay(tictac,move)
     character(1), dimension(3,3) :: tictac
     integer :: move
   
     checkPlay = .false.
     select case (move)
       case (1)
         if (tictac(1,1) == " ") checkPlay = .true.
       case (2)
         if (tictac(1,2) == " ") checkPlay = .true.
       case (3)
         if (tictac(1,3) == " ") checkPlay = .true.
       case (4)
         if (tictac(2,1) == " ") checkPlay = .true.
       case (5)
         if (tictac(2,2) == " ") checkPlay = .true.
       case (6)
         if (tictac(2,3) == " ") checkPlay = .true.
       case (7)
         if (tictac(3,1) == " ") checkPlay = .true.
       case (8)
         if (tictac(3,2) == " ") checkPlay = .true.
       case (9)
         if (tictac(3,3) == " ") checkPlay = .true.
     end select
   end