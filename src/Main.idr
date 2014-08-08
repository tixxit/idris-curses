module Main

import Ncurses

main : IO ()
main = ncursesMain $ (do
  window <- initscr
  cbreak
  noecho
  intrflush window False
  ln <- lines
  cl <- cols
  putStrLn window $ (show ln) ++ " x " ++ (show cl)
  getch window
  -- keypad window True
  endwin)
