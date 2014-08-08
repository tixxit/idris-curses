module Ncurses

%lib C "ncurses"
%include C "Ncurses/ncurses_extra.h"
%link C "Ncurses/ncurses_extra.o"

%access public

data NcursesError = NullWindow

data NcursesIO : Type -> Type where
  ncursesIO : IO (Either NcursesError a) -> NcursesIO a

unliftIO : NcursesIO a -> IO (Either NcursesError a)
unliftIO (ncursesIO ioe) = ioe

liftIO : IO a -> NcursesIO a
liftIO ioa = ncursesIO (map Right ioa)

instance Functor NcursesIO where
  map f (ncursesIO io) = ncursesIO (map (map f) io)

instance Applicative NcursesIO where
  pure a = ncursesIO (pure (pure a))
  (ncursesIO f) <$> (ncursesIO a) = ncursesIO io where
    io : IO (Either NcursesError b)
    io = do
      f' <- f
      a' <- a
      return (f' <$> a')

instance Monad NcursesIO where
  (ncursesIO a) >>= k = ncursesIO io where
    io : IO (Either NcursesError b)
    io = do
      a' <- a
      case a' of
        Left err => return (Left err)
        Right a => unliftIO (k a)

data Window = WindowPtr Ptr

private
cBool : Bool -> Int
cBool True = 1
cBool False = 0

private
liftError : IO Int -> NcursesIO ()
liftError code = ncursesIO (map liftErr code) where
  liftErr code = if code == -1 then Left NullWindow else Right ()

--
-- Global Variables
--

lines : NcursesIO Int
lines = liftIO $ mkForeign (FFun "getLines" [] FInt)

cols : NcursesIO Int
cols = liftIO $ mkForeign (FFun "getCols" [] FInt)

--
-- Window/Screen Management
--

initscr : NcursesIO Window
initscr = liftIO $ map WindowPtr $ mkForeign (FFun "initscr" [] FPtr)

endwin : NcursesIO ()
endwin = liftError $ mkForeign (FFun "endwin" [] FInt)

--
-- I/O
--

refresh : Window -> NcursesIO ()
refresh (WindowPtr p) = liftError $ mkForeign (FFun "wrefresh" [FPtr] FInt) p

getch : Window -> NcursesIO ()
getch (WindowPtr p) = liftError $ mkForeign (FFun "wgetch" [FPtr] FInt) p

putStr : Window -> String -> NcursesIO ()
putStr (WindowPtr ptr) s = liftError $ mkForeign (FFun "wprintw" [FPtr, FString] FInt) ptr s

putStrLn : Window -> String -> NcursesIO ()
putStrLn w s = putStr w (s ++ "\n")

--
-- Input Options
--

cbreak : NcursesIO ()
cbreak = liftError $ mkForeign (FFun "cbreak" [] FInt)

nocbreak : NcursesIO ()
nocbreak = liftError $ mkForeign (FFun "nocbreak" [] FInt)

echo : NcursesIO ()
echo = liftError $ mkForeign (FFun "echo" [] FInt)

noecho : NcursesIO ()
noecho = liftError $ mkForeign (FFun "noecho" [] FInt)

halfdelay : NcursesIO ()
halfdelay = liftError $ mkForeign (FFun "halfdelay" [] FInt)

intrflush : Window -> Bool -> NcursesIO ()
intrflush (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "intrflush" [FPtr, FInt] FInt) ptr (cBool p)

keypad : Window -> Bool -> NcursesIO ()
keypad (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "keypad" [FPtr, FInt] FInt) ptr (cBool p)

meta : Window -> Bool -> NcursesIO ()
meta (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "meta" [FPtr, FInt] FInt) ptr (cBool p)

nodelay : Window -> Bool -> NcursesIO ()
nodelay (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "nodelay" [FPtr, FInt] FInt) ptr (cBool p)

raw : NcursesIO ()
raw = liftError $ mkForeign (FFun "raw" [] FInt)

noraw : NcursesIO ()
noraw = liftError $ mkForeign (FFun "noraw" [] FInt)

noqiflush : NcursesIO ()
noqiflush = liftIO $ mkForeign (FFun "noqiflush" [] FUnit)

qiflush : NcursesIO ()
qiflush = liftIO $ mkForeign (FFun "qiflush" [] FUnit)

notimeout : Window -> Bool -> NcursesIO ()
notimeout (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "notimeout" [FPtr, FInt] FInt) ptr (cBool p)

timeout : Int -> NcursesIO ()
timeout delay = liftIO $ mkForeign (FFun "timeout" [FInt] FUnit) delay

wtimeout : Window -> Int -> NcursesIO ()
wtimeout (WindowPtr ptr) delay =
  liftIO $ mkForeign (FFun "wtimeout" [FPtr, FInt] FUnit) ptr delay

-- fd is a file descriptor? What to do...
typeahead : Int -> NcursesIO ()
typeahead fd = liftError $ mkForeign (FFun "typeahead" [FInt] FInt) fd

--
-- Output Options.
--

clearok : Window -> Bool -> NcursesIO ()
clearok (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "clearok" [FPtr, FInt] FInt) ptr (cBool p)

idlok : Window -> Bool -> NcursesIO ()
idlok (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "idlok" [FPtr, FInt] FInt) ptr (cBool p)

idcok : Window -> Bool -> NcursesIO ()
idcok (WindowPtr ptr) p =
  liftIO $ mkForeign (FFun "idcok" [FPtr, FInt] FUnit) ptr (cBool p)

immedok : Window -> Bool -> NcursesIO ()
immedok (WindowPtr ptr) p =
  liftIO $ mkForeign (FFun "immedok" [FPtr, FInt] FUnit) ptr (cBool p)

leaveok : Window -> Bool -> NcursesIO ()
leaveok (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "leaveok" [FPtr, FInt] FInt) ptr (cBool p)

setscrreg : Int -> Int -> NcursesIO ()
setscrreg top bot =
  liftError $ mkForeign (FFun "setscrreg" [FInt, FInt] FInt) top bot

wsetscrreg : Window -> Int -> Int -> NcursesIO ()
wsetscrreg (WindowPtr ptr) top bot =
  liftError $ mkForeign (FFun "wsetscrreg" [FPtr, FInt, FInt] FInt) ptr top bot

scrollok : Window -> Bool -> NcursesIO ()
scrollok (WindowPtr ptr) p =
  liftError $ mkForeign (FFun "scrollok" [FPtr, FInt] FInt) ptr (cBool p)

nl : NcursesIO ()
nl = liftError $ mkForeign (FFun "nl" [] FInt)

nonl : NcursesIO ()
nonl = liftError $ mkForeign (FFun "nonl" [] FInt)

runNcurses : (NcursesError -> IO b) -> (a -> IO b) -> NcursesIO a -> IO b
runNcurses f g nio = do
  e <- unliftIO nio
  case e of
    Left err => f err
    Right a => g a

ncursesMain : NcursesIO () -> IO ()
ncursesMain = runNcurses logError return where
  logError NullWindow = putStrLn "NULL window"

-- lines : IO Int
-- lines = mkForeign (FFun "LINES" [] FUnit) 
