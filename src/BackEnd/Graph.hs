module Graph where

class Graph graph where
  nodes :: graph node -> [node]