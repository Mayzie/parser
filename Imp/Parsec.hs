
-- | Parser combinators.
module Imp.Parsec where
import Control.Monad

-------------------------------------------------------------------------------
--   A parser is a function that takes a list of input tokens, and produces
--   a list of the possible parses for these tokens. Each element in the
--   result list is a tuple consisting of the parsed value and the remaining
--   input tokens.
data Parser token a
        = MkParser ([token] -> [(a, [token])])


-- | Apply a parser to a list of input tokens.
parse  :: Parser token a -> [token] -> [(a, [token])]
parse (MkParser parseA) tokens
 = parseA tokens


-- | A parser that produces a result without consuming any input.
result :: a -> Parser token a
result x 
 = MkParser $ \tokens -> [(x, tokens)]


-- | A parser that always fails, producing no possible parses.
zero  :: Parser token a
zero    
 = MkParser $ \_ -> []


-- | A parser that consumes the fist token of the input, failing if there
--   aren't any.
item  :: Parser token token
item    
 =  MkParser $ \tokens 
 -> case tokens of
        []      -> []
        t:ts    -> [(t, ts)]


-------------------------------------------------------------------------------
-- | A parser that accepts a single token that satisfies the given predicate,
--   producing the given value if it matches.
satisfies :: (token -> Bool) -> Parser token token
satisfies match' 
 =  MkParser $ \tokens
 -> case tokens of
        []      -> []
        t : ts
         | match' t      -> [(t, ts)]
         | otherwise    -> []


-- | Like 'satisfies', except that the worker function also produces the 
--   result of the parser.
from :: (token -> Maybe a) -> Parser token a
from eat
 =  MkParser $ \tokens
 -> case tokens of
        []      -> []
        t : ts
         -> case eat t of
                Nothing -> []
                Just x  -> [(x, ts)]


-- | Accept only the given token.
only :: Eq token => token -> Parser token token
only token
 = satisfies (== token)


-- | Parse just the given character.
char :: Char -> Parser Char Char
char c  = satisfies (== c) 


-- | Parse a digit.
digit :: Parser Char Char
digit   = satisfies (\d -> d >= '0' && d <= '9')


-- | Parse a lower-case letter.
lower :: Parser Char Char
lower   = satisfies (\d -> d >= 'a' && d <= 'z') 


-- | Parse an upper-case letter.
upper :: Parser Char Char
upper   = satisfies (\d -> d >= 'A' && d <= 'Z')


-- | Parse some whitespace.
space    :: Parser Char Char
space    = plus (char ' ') 
         $ plus (char '\t')
                (char '\n')


------------------------------------------------------------------------------
-- | A parser that accepts strings parsed by either of two others,
--   including both.
plus :: Parser token a -> Parser token a -> Parser token a
plus parserA parserB
 =  MkParser $ \tokens
 -> parse parserA tokens ++ parse parserB tokens


-- | Parse an upper or lower case letter.
letter   :: Parser Char Char
letter   = plus upper lower


-- | Parse an alphanumeric character.
alphanum :: Parser Char Char
alphanum = plus letter digit


-------------------------------------------------------------------------------
-- | A parser that accepts strings parsed by either of two others,
--   but not both. We prefer the results produced by the first parser.
alt :: Parser token a -> Parser token a -> Parser token a
alt parserA parserB
 =  MkParser $ \tokens
 -> case parse parserA tokens of
        []      -> parse parserB tokens
        res     -> res


-- | Try to apply one of the parsers in the given list,
--   failing if none succeed.
alts  :: [Parser token a] -> Parser token a
alts []            = zero
alts (a : as)      = alt a (alts as)


-- | Try to apply one of the parsers in the given list,
--   using the default parser if none succeed.
altss :: [Parser token a] -> Parser token a -> Parser token a
altss [] def       = def
altss (a : as) def = alt a (altss as def)


-------------------------------------------------------------------------------
-- | Define a flipped version of 'map' that has the arguments
--   in the other order.
forEach :: [a] -> (a -> b) -> [b]
forEach xs f = map f xs


-- | Apply a parser followed by another parser,
--   producing a tuple containing both results.
follows :: Parser token a -> Parser token b -> Parser token (a, b)
follows parserA parserB
 =  MkParser $ \tokens
 -> concat
 $  forEach (parse parserA tokens)  $ \(resultA, tokens')
 -> forEach (parse parserB tokens') $ \(resultB, tokens'')
 -> ((resultA, resultB), tokens'')


-- | Apply a parser followed by another parser,
--   where we can use the result of the first parse to construct
--   the second parser.
bind   :: Parser token a -> (a -> Parser token b) -> Parser token b
bind parserA mkParserB
 =  MkParser $ \tokens
 -> concat
 $  forEach (parse parserA             tokens)  $ \(resultA, tokens')
 -> forEach (parse (mkParserB resultA) tokens') $ \(resultB, tokens'')
 -> (resultB, tokens'')


-------------------------------------------------------------------------------
-- | Parse zero or more things, yielding a list of those things.
some    :: Parser token a -> Parser token [a]
some parserA 
 = alt  (do     x       <- parserA
                xs      <- some parserA
                result  (x : xs))
        (result [])


-- | Parse one or more things, yielding a list of those things.
many    :: Parser token a -> Parser token [a]
many parserA 
 = alt  (do     x       <- parserA
                xs      <- some parserA
                return  (x : xs))

        (do     x       <- parserA
                return  [x])


-- | Parse a natural number.
nat :: Parser Char Int
nat = fmap read (many digit)


-------------------------------------------------------------------------------
-- | The Functor class defines a function 'fmap' that allows us to apply
--   a worker function to the elements of a container-like structure.
--   The 'fmap' function is a generalisation of the standard 'map' function
--   for lists. 
-- 
--   We can treat a parser as a container by imagining that it holds the value
--   that it parses. If the parser fails then the container is empty.
--
instance Functor (Parser input) where
 fmap f parser
  = MkParser $  \tokens
  -> forEach (parse parser tokens)      $ \(res, tokens')
  -> (f res, tokens')


-- | The Applicative class is used when working with containers that hold
--   functions. If we have a container holding a function, and a container
--   holding the argument, then we can apply the function to the argument,
--   producing the result in a new container.
--  
--   For example:
--     > Just negate <*> Just 5 
--     Just (-5)
--
--     > Just negate <*> Nothing
--     Nothing
--
--   The methods of the applicative class can be defined in terms the ones
--   of the Monad class, so we do that.
instance Applicative (Parser input) where
 pure   = return
 (<*>)  = ap


-- | Define a monad for parsers, so we can use the do notation and
--   other functions defined for monads.
--
--   The 'return' function produces a new container holding a value.
--       'result' function produces a parser that yields a value
--                without consuming any input.

--   The '>>='    function opens a container and uses the value inside
--                to construct a new value in a new container.
--       'bind'   function parses a value, then uses that value to 
--                produce a new parser.
--
instance Monad (Parser input) where
 return = result
 (>>=)  = bind


-------------------------------------------------------------------------------
-- | Parse the given sequence of tokens.
--
--   The 'sequence' function is from the Control.Monad library, 
--   knows nothing about parsers, and was discussed in the week 5 tutorial.
--
match :: Eq a => [a] -> Parser a [a]
match xs = sequence $ map (\x -> satisfies (== x)) xs


