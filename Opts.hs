{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( opts
    , execParser
    ) where


import           Options.Applicative

import           WaTor.Types


opts' :: Parser Params
opts' =   Params
      <$> option auto
                 (  long "initial-sharks"
                 <> metavar "INITIAL_SHARKS_PROBABILITY" <> value 0.1
                 <> help "The probability that an initial location will\
                         \ be a shark. Default = 0.1.")
      <*> option auto
                 (  long "initial-fish"
                 <> metavar "INITIAL_FISH_PROBABILITY" <> value 0.2
                 <> help "The probability that an initial location will\
                         \ be a fish. Default = 0.2.")
      <*> option auto
                 (  long "reproduce-shark"
                 <> metavar "SHARK_REPRODUCE_AGE" <> value 10
                 <> help "The age at which shark reproduce. Default = 10.")
      <*> option auto
                 (  long "reproduce-fish"
                 <> metavar "FISH_REPRODUCE_AGE" <> value 10
                 <> help "The age at which fish reproduce. Default = 10.")
      <*> option auto
                 (  long "initial-shark-energy"
                 <> metavar "INITIAL_SHARK_ENERGY" <> value 15
                 <> help "The amount of energy sharks are created with.\
                         \ Default = 15.")
      <*> option auto
                 (  long "fish-energy"
                 <> metavar "EATEN_FISH_ENERGY" <> value 10
                 <> help "The amount of energy a shark gets from eating\
                         \ a fish. Default = 10.")
      <*> option auto
                 (  long "width"
                 <> metavar "CELLS" <> value 500
                 <> help "The width of the view area in cells. Default = 500.")
      <*> option auto
                 (  long "height"
                 <> metavar "CELLS" <> value 309
                 <> help "The height of the view area in cells. Default = 309.")
      <*> option auto
                 (  long "scaling"
                 <> metavar "ZOOM" <> value 2
                 <> help "The size to zoom the elements to. Default = 2.")
      <*> option auto
                 (  long "speed"
                 <> metavar "FRAMES_PER_SECOND" <> value 7
                 <> help "The frames/second in the simulation. Default = 7.")
      <*> optional (strOption
                       (  long "count-log" <> metavar "FILENAME"
                       <> help "A file name for the counts. This will be TSV."))

opts :: ParserInfo Params
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Wa-Tor population dynamics simulation."
            <> header "wa-tor - population dynamics")
