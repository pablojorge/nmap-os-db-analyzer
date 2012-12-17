module XML ( 
    -- types
    Attribute(..),
    Element(..),
    Document(..),
    -- classes
    AsXML(..)
) where

data Attribute = Attribute String String

data Element = Element String [Attribute] [Element]

data Document = Document Element

class AsXML a where
    document :: a -> Document

instance Show Attribute where
    show (Attribute name value) = name ++ "=\"" ++ value ++ "\""
    showList (x:xs) = showChar ' ' . shows x . showl xs
        where showl (x:xs) = showChar ' ' . shows x . showl xs
              showl [] = showString ""
    showList [] = showString ""

instance Show Element where
    show (Element name attr children) 
        | (null children) = "<" ++ name ++ (show attr) ++ "/>\n"
        | otherwise = "<" ++ name ++ (show attr) ++ ">\n" ++ 
                      (show children) ++
                      "</" ++ name ++ ">\n"
    showList (x:xs) = shows x . showl xs
        where showl (x:xs) = shows x . showl xs
              showl [] = showString ""
    showList [] = showString ""

instance Show Document where
    show (Document element) = show element
