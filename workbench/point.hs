data Point
  = Aff { x :: Double
        , y :: Double}
  | Proj { x :: Double
         , y :: Double
         , z :: Double}

instance Show Point where
  show (Aff xx yy) = "(" ++ show xx ++ ", " ++ show yy ++ ")"
  show (Proj xx yy zz) =
    "(" ++ show (xx / zz) ++ " : " ++ show (yy / zz) ++ " : " ++ show 1.0 ++ ")"
