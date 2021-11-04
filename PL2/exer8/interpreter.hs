-- Syntax
data P = Pint Integer | Ptrue | Pfalse
       | Padd | Pneg | Pmul | Pdiv | Plt | Peq | Pand | Pnot
       | Pnop | Pdup | Ppop | Pswap | Pswap2
       | Pseq P P | Pcond P P | Ploop P

-- Denotational semantics
data V = VI Integer | VB Bool
type S = [V]

fix :: (a -> a) -> a
fix f = f (fix f)

funF f p ((VB b): s) = if b then f p $ sem p s else s

sem :: P -> S -> S
sem (Pint n) s = (VI n):s
sem Ptrue s = (VB True):s
sem Pfalse s = (VB False):s
sem Padd ((VI n1):(VI n2):s) = (VI $ n1 + n2):s
sem Pneg ((VI n):s) = (VI $ -n):s
sem Pmul ((VI n1):(VI n2):s) = (VI $ n1 * n2):s
sem Pdiv ((VI n1):(VI n2):s) = (VI $ n2 `div` n1):(VI $ n2 `mod` n1):s
sem Pand ((VB b1):(VB b2):s) = (VB $ b2 && b1):s
sem Pnot ((VB b):s) = (VB $ not b):s
sem Plt ((VI n1):(VI n2):s) = (VB $ n2 < n1):s
sem Peq ((VI n1):(VI n2):s) = (VB $ n2 == n1):s
sem Pnop s = s
sem Pdup (v:s) = v:v:s
sem Ppop (v:s) = s
sem Pswap (v:u:s) = u:v:s
sem Pswap2 (v1:v2:v3:s) = v2:v3:v1:s
sem (Pseq p1 p2) s = sem p2 $ sem p1 s
sem (Pcond p1 p2) ((VB b):s) = if b then sem p1 s else sem p2 s
sem (Ploop p) s = fix funF p s

-- Main function: interpreter

main = do
  input <- getContents
  mapM_ print $ sem (read input) []

-- Pretty-printing

instance Show P where
  showsPrec d (Pint n) = showsPrec 0 n
  showsPrec d Ptrue = ("true" ++)
  showsPrec d Pfalse = ("false" ++)
  showsPrec d Padd = ("+" ++)
  showsPrec d Pneg = ("-" ++)
  showsPrec d Pmul = ("*" ++)
  showsPrec d Pdiv = ("/" ++)
  showsPrec d Plt = ("<" ++)
  showsPrec d Peq = ("=" ++)
  showsPrec d Pand = ("and" ++)
  showsPrec d Pnot = ("not" ++)
  showsPrec d Pnop = ("nop" ++)
  showsPrec d Pdup = ("dup" ++)
  showsPrec d Ppop = ("pop" ++)
  showsPrec d Pswap = ("swap" ++)
  showsPrec d Pswap2 = ("swap2" ++)
  showsPrec d (Pseq p1 p2) =
    showParen (d > 0) $ showsPrec 1 p1 . (" " ++) . showsPrec 0 p2
  showsPrec d (Pcond p1 p2) =
    ("cond [" ++) . showsPrec 0 p1 . (" | " ++)
                  . showsPrec 0 p2 . ("]" ++)
  showsPrec d (Ploop p) = ("loop [" ++) . showsPrec 0 p . ("]" ++)

instance Show V where
  showsPrec d (VI n) = showsPrec d n
  showsPrec d (VB True) = ("true" ++)
  showsPrec d (VB False) = ("false" ++)

-- Parsing

next (x : r) = [(x, r)]
next s = []

instance Read P where
  readsPrec d s =
    readParen False   (\s -> [(Pint n, r)      | (n, r) <- readsPrec 0 s]) s ++
    readParen False   (\s -> [(Ptrue, r)       | ("true", r) <- lex s]) s ++
    readParen False   (\s -> [(Pfalse, r)      | ("false", r) <- lex s]) s ++
    readParen False   (\s -> [(Padd, r)        | ("+", r) <- lex s]) s ++
    readParen False   (\s -> [(Pneg, r)        | ("-", r) <- lex s]) s ++
    readParen False   (\s -> [(Pmul, r)        | ("*", r) <- lex s]) s ++
    readParen False   (\s -> [(Pdiv, r)        | ("/", r) <- lex s]) s ++
    readParen False   (\s -> [(Plt, r)         | ("<", r) <- lex s]) s ++
    readParen False   (\s -> [(Peq, r)         | ("=", r) <- lex s]) s ++
    readParen False   (\s -> [(Pand, r)        | ("and", r) <- lex s]) s ++
    readParen False   (\s -> [(Pnot, r)        | ("not", r) <- lex s]) s ++
    readParen False   (\s -> [(Pnop, r)        | ("nop", r) <- lex s]) s ++
    readParen False   (\s -> [(Pdup, r)        | ("dup", r) <- lex s]) s ++
    readParen False   (\s -> [(Ppop, r)        | ("pop", r) <- lex s]) s ++
    readParen False   (\s -> [(Pswap, r)       | ("swap", r) <- lex s]) s ++
    readParen False   (\s -> [(Pswap2, r)      | ("swap2", r) <- lex s]) s ++
    readParen (d > 0) (\s -> [(Pseq p1 p2, r)  | (p1, t) <- readsPrec 1 s,
                                                 (p2, r) <- readsPrec 0 t]) s ++
    readParen False   (\s -> [(Pcond p1 p2, r) | ("cond", t1) <- lex s,
                                                 ("[", t2) <- lex t1,
                                                 (p1, t3) <- readsPrec 0 t2,
                                                 ("|", t4) <- lex t3,
                                                 (p2, t5) <- readsPrec 0 t4,
                                                 ("]", r) <- lex t5]) s ++
    readParen False   (\s -> [(Ploop p, r)     | ("loop", t1) <- lex s,
                                                 ("[", t2) <- lex t1,
                                                 (p, t3) <- readsPrec 0 t2,
                                                 ("]", r) <- lex t3]) s
