import           Control.Applicative
import           Data.List           (nub)
import qualified Data.Map            as M
import           Text.Trifecta

-- J9-54
-- AK-QJo
-- KQ-JTs
-- 7h6h-43
-- 44+
-- JT
-- JTs
-- 9c7c

data AST = Or AST AST |
  RangeA Rank Rank Rank Rank
 | RangeO Rank Rank Rank Rank
 | RangeS Rank Rank Rank Rank
 | Range Rank Rank Rank Rank Suit Suit
 | ComboA Rank Rank
 | ComboS Rank Rank
 | ComboO Rank Rank
 | ComboOne Combo
 | Empty
 deriving Show

-- instance Show AST where
--   show (RangeA r1 r2 r3 r4) = printf "%c%c-%c%c" (show r1) (show r2) (show r3) (show r4)

parseRank :: Parser Rank
parseRank = fmap rankFromChar $ oneOf "AKQJT98765432"

parseSuit :: Parser Suit
parseSuit = fmap suitFromChar $ oneOf "cdsh"

parseComboOne :: Parser AST
parseComboOne = ComboOne <$> (Combo <$> card <*> card)
                  where card = (Card <$> parseRank <*> parseSuit)

parseComboO :: Parser AST
parseComboO = ComboO <$> parseRank <*> parseRank <* char 'o'

parseComboS :: Parser AST
parseComboS = ComboS <$> parseRank <*> parseRank <* char 's'

parseComboA :: Parser AST
parseComboA = ComboA <$> parseRank <*> parseRank

parseRange :: Parser AST
parseRange = Range <$> parseRank <*> (parseRank <* char '-') <*>
                       parseRank <*> parseRank <*>
                       parseSuit <*> parseSuit

parseRangeA :: Parser AST
parseRangeA = RangeA <$> parseRank <*> (parseRank <* char '-') <*>
                         parseRank <*> parseRank

parseRangeO :: Parser AST
parseRangeO = RangeO <$> parseRank <*> (parseRank <* char '-') <*>
                         parseRank <*> parseRank <* char 'o'

parseAST :: Parser AST
parseAST = (foldr Or Empty) <$> asts
  where asts = choice [
          try parseRangeO,
          try parseRangeA,
          try parseRange,
          try parseComboOne,
          try parseComboS,
          try parseComboO,
          try parseComboA
          ] `sepBy` (token $ char ',')


data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | T | J | Q | K | A
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show r = case M.lookup r rankCharMap of
             Just c -> [c]
             Nothing -> error "Show rank"

data Suit = Club | Diamond | Heart | Spade
  deriving (Eq, Enum, Bounded)

instance Show Suit where
  show Club = "♣"
  show Diamond = "♦"
  show Heart = "♥"
  show Spade = "♠"

rankCharMap :: M.Map Rank Char
rankCharMap = M.fromList $ zip [R2 .. A] "23456789TJQKA"

charRankMap :: M.Map Char Rank
charRankMap = M.foldWithKey (flip M.insert) M.empty rankCharMap

suitFromChar :: Char -> Suit
suitFromChar x = case x of
  'c' -> Club
  'd' -> Diamond
  's' -> Spade
  'h' -> Heart
  _ -> error "suitFromChar"

rankFromChar :: Char -> Rank
rankFromChar c = case M.lookup c charRankMap of
  Just x -> x
  Nothing -> error "rankFromChar"

data Card = Card Rank Suit
  deriving Eq

instance Show Card where
  show (Card r s) = (show r) ++ (show s)

data Combo = Combo Card Card
  deriving Eq

instance Show Combo where
  show (Combo c c') = (show c) ++ (show c')

interpret :: AST -> [Combo]
interpret Empty = []
interpret (Or ast ast') = nub $ (interpret ast) ++ (interpret ast')
interpret (ComboOne c) = [c]

