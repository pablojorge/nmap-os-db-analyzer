-- http://www.graphviz.org/doc/info/lang.html

module Graph ( 
    -- types
    Attribute(..),
    Directed(..),
    Statement(..),
    Graph(..),
    -- classes
    AsGraph(..),
) where

data Attribute = Label String |
                 Style String |
                 Color String |
                 Fillcolor String |
                 Shape String |
                 Fontname String |
                 Fontsize Int |
                 Fontcolor String

data Directed = Directed | NonDirected

data Statement = Node String [Attribute] |
                 Edge Directed String String [Attribute] |
                 Attributes String [Attribute] |
                 SubGraph String [Statement]

data Graph = Graph Directed String [Statement] 

class AsGraph a where
    graph :: a -> Graph

instance Show Attribute where
    show (Label label) = "label=\"" ++ label ++ "\""
    show (Style style) = "style=" ++ style
    show (Color color) = "color=\"" ++ color ++ "\""
    show (Fillcolor fillcolor) = "fillcolor=\"" ++ fillcolor ++ "\""
    show (Shape shape) = "shape=" ++ shape
    show (Fontname fontname) = "fontname=\"" ++ fontname ++ "\""
    show (Fontsize fontsize) = "fontsize=\"" ++ (show fontsize) ++ "\""
    show (Fontcolor fontcolor) = "fontcolor=\"" ++ fontcolor ++ "\""

instance Show Statement where
    show (Node id list) = id ++ " " ++ (show list) ++ "\n"
    show (Edge Directed a b list) = a ++ " -> " ++ b ++ (show list) ++ "\n"
    show (Edge NonDirected a b list) = a ++ " -- " ++ b ++ (show list) ++ "\n"
    show (Attributes element list) = element ++ " " ++ (show list) ++ "\n"
    show (SubGraph id list) = 
        "subgraph " ++ id ++ " {\n" ++ (show list) ++ "}\n"
    showList (x:xs) = shows x . showl xs
        where showl (x:xs) = shows x . showl xs
              showl [] = showString ""
    showList [] = showString ""

instance Show Graph where
    show (Graph Directed id list) = 
        "digraph " ++ id ++ " {\n" ++ (show list) ++ "}\n"
    show (Graph NonDirected id list) = 
        "graph " ++ id ++ " {\n" ++ (show list) ++ "}\n"
