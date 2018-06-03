module TestDataHex (
  testDataHex,
  ) where

testDataHex = map snd testDataHex'

-- TODO: データに間違いがないか、手作業で確認する

testDataHex' = [
  ((146,1),("x,y,z"," 1  ")),
  ((146,1),("-y,x-y,z"," 3+ 0,0,z")),
  ((146,1),("y-x,-x,z"," 3- 0,0,z")),
  ((146,1),("x+2/3,y+1/3,z+1/3"," t (2/3,1/3,1/3) ")),
  ((146,1),("-y+2/3,x-y+1/3,z+1/3"," 3+(0,0,1/3) 1/3,1/3,z")),
  ((146,1),("y-x+2/3,-x+1/3,z+1/3"," 3-(0,0,1/3) 1/3,0,z")),
  ((146,1),("x+1/3,y+2/3,z+2/3"," t (1/3,2/3,2/3) ")),
  ((146,1),("-y+1/3,x-y+2/3,z+2/3"," 3+(0,0,2/3) 0,1/3,z")),
  ((146,1),("y-x+1/3,-x+2/3,z+2/3"," 3-(0,0,2/3) 1/3,1/3,z")),
  ((148,1),("-x,-y,-z","-1  0,0,0")),
  ((148,1),("y,y-x,-z","-3+ 0,0,z; 0,0,0")),
  ((148,1),("x-y,x,-z","-3- 0,0,z; 0,0,0")),
  ((148,1),("-x+2/3,-y+1/3,-z+1/3","-1  1/3,1/6,1/6")),
  ((148,1),("y+2/3,y-x+1/3,-z+1/3","-3+ 1/3,-1/3,z; 1/3,-1/3,1/6")),
  ((148,1),("x-y+2/3,x+1/3,-z+1/3","-3- 1/3,2/3,z; 1/3,2/3,1/6")),
  ((148,1),("-x+1/3,-y+2/3,-z+2/3","-1  1/6,1/3,1/3")),
  ((148,1),("y+1/3,y-x+2/3,-z+2/3","-3+ 2/3,1/3,z; 2/3,1/3,1/3")),
  ((148,1),("x-y+1/3,x+2/3,-z+2/3","-3- -1/3,1/3,z; -1/3,1/3,1/3")),
  ((155,1),("y,x,-z"," 2  x,x,0")),
  ((155,1),("x-y,-y,-z"," 2  x,0,0")),
  ((155,1),("-x,y-x,-z"," 2  0,y,0")),
  ((155,1),("y+2/3,x+1/3,-z+1/3"," 2 (1/2,1/2,0) x,x-1/6,1/6")),
  ((155,1),("x-y+2/3,-y+1/3,-z+1/3"," 2 (1/2,0,0) x,1/6,1/6")),
  ((155,1),("-x+2/3,y-x+1/3,-z+1/3"," 2  1/3,y,1/6")),
  ((155,1),("y+1/3,x+2/3,-z+2/3"," 2 (1/2,1/2,0) x,x+1/6,1/3")),
  ((155,1),("x-y+1/3,-y+2/3,-z+2/3"," 2  x,1/3,1/3")),
  ((155,1),("-x+1/3,y-x+2/3,-z+2/3"," 2 (0,1/2,0) 1/6,y,1/3")),
  ((160,1),("-y,-x,z"," m  x,-x,z")),
  ((160,1),("y-x,y,z"," m  x,2x,z")),
  ((160,1),("x,x-y,z"," m  2x,x,z")),
  ((160,1),("-y+2/3,-x+1/3,z+1/3"," g (1/6,-1/6,1/3) x+1/2,-x,z")),
  ((160,1),("y-x+2/3,y+1/3,z+1/3"," g (1/6,1/3,1/3) x+1/4,2x,z")),
  ((160,1),("x+2/3,x-y+1/3,z+1/3"," g (2/3,1/3,1/3) 2x,x,z")),
  ((160,1),("-y+1/3,-x+2/3,z+2/3"," g (-1/6,1/6,2/3) x+1/2,-x,z")),
  ((160,1),("y-x+1/3,y+2/3,z+2/3"," g (1/3,2/3,2/3) x,2x,z")),
  ((160,1),("x+1/3,x-y+2/3,z+2/3"," g (1/3,1/6,2/3) 2x-1/2,x,z")),
  ((161,1),("-y,-x,z+1/2"," c  x,-x,z")),
  ((161,1),("y-x,y,z+1/2"," c  x,2x,z")),
  ((161,1),("x,x-y,z+1/2"," c  2x,x,z")),
  ((161,1),("-y+2/3,-x+1/3,z+5/6"," g (1/6,-1/6,5/6) x+1/2,-x,z")),
  ((161,1),("y-x+2/3,y+1/3,z+5/6"," g (1/6,1/3,5/6) x+1/4,2x,z")),
  ((161,1),("x+2/3,x-y+1/3,z+5/6"," g (2/3,1/3,5/6) 2x,x,z")),
  ((161,1),("-y+1/3,-x+2/3,z+1/6"," g (-1/6,1/6,1/6) x+1/2,-x,z")),
  ((161,1),("y-x+1/3,y+2/3,z+1/6"," g (1/3,2/3,1/6) x,2x,z")),
  ((161,1),("x+1/3,x-y+2/3,z+1/6"," g (1/3,1/6,1/6) 2x-1/2,x,z")),
  ((167,1),("y,x,-z+1/2"," 2  x,x,1/4")),
  ((167,1),("x-y,-y,-z+1/2"," 2  x,0,1/4")),
  ((167,1),("-x,y-x,-z+1/2"," 2  0,y,1/4")),
  ((167,1),("y+2/3,x+1/3,-z+5/6"," 2 (1/2,1/2,0) x,x-1/6,5/12")),
  ((167,1),("x-y+2/3,-y+1/3,-z+5/6"," 2 (1/2,0,0) x,1/6,5/12")),
  ((167,1),("-x+2/3,y-x+1/3,-z+5/6"," 2  1/3,y,5/12")),
  ((167,1),("y+1/3,x+2/3,-z+1/6"," 2 (1/2,1/2,0) x,x+1/6,1/12")),
  ((167,1),("x-y+1/3,-y+2/3,-z+1/6"," 2  x,1/3,1/12")),
  ((167,1),("-x+1/3,y-x+2/3,-z+1/6"," 2 (0,1/2,0) 1/6,y,1/12")),
  ((146,2),("z,x,y"," 3+ x,x,x")),
  ((146,2),("y,z,x"," 3- x,x,x")),
  ((148,2),("-z,-x,-y","-3+ x,x,x; 0,0,0")),
  ((148,2),("-y,-z,-x","-3- x,x,x; 0,0,0")),
  ((155,2),("-z,-y,-x"," 2  -x,0,x")),
  ((155,2),("-y,-x,-z"," 2  x,-x,0")),
  ((155,2),("-x,-z,-y"," 2  0,y,-y")),
  ((160,2),("z,y,x"," m  x,y,x")),
  ((160,2),("y,x,z"," m  x,x,z")),
  ((160,2),("x,z,y"," m  x,y,y")),
  ((161,2),("z+1/2,y+1/2,x+1/2"," n (1/2,1/2,1/2) x,y,x")),
  ((161,2),("y+1/2,x+1/2,z+1/2"," n (1/2,1/2,1/2) x,x,z")),
  ((161,2),("x+1/2,z+1/2,y+1/2"," n (1/2,1/2,1/2) x,y,y")),
  ((167,2),("-z+1/2,-y+1/2,-x+1/2"," 2  -x+1/2,1/4,x")),
  ((167,2),("-y+1/2,-x+1/2,-z+1/2"," 2  x,-x+1/2,1/4")),
  ((167,2),("-x+1/2,-z+1/2,-y+1/2"," 2  1/4,y+1/2,-y")),
  ((168,1),("-x,-y,z"," 2  0,0,z")),
  ((168,1),("y,y-x,z"," 6- 0,0,z")),
  ((168,1),("x-y,x,z"," 6+ 0,0,z")),
  ((169,1),("-y,x-y,z+1/3"," 3+(0,0,1/3) 0,0,z")),
  ((169,1),("y-x,-x,z+2/3"," 3-(0,0,2/3) 0,0,z")),
  ((169,1),("-x,-y,z+1/2"," 2 (0,0,1/2) 0,0,z")),
  ((169,1),("y,y-x,z+5/6"," 6-(0,0,5/6) 0,0,z")),
  ((169,1),("x-y,x,z+1/6"," 6+(0,0,1/6) 0,0,z")),
  ((170,1),("-y,x-y,z+2/3"," 3+(0,0,2/3) 0,0,z")),
  ((170,1),("y-x,-x,z+1/3"," 3-(0,0,1/3) 0,0,z")),
  ((170,1),("y,y-x,z+1/6"," 6-(0,0,1/6) 0,0,z")),
  ((170,1),("x-y,x,z+5/6"," 6+(0,0,5/6) 0,0,z")),
  ((171,1),("y,y-x,z+2/3"," 6-(0,0,2/3) 0,0,z")),
  ((171,1),("x-y,x,z+1/3"," 6+(0,0,1/3) 0,0,z")),
  ((172,1),("y,y-x,z+1/3"," 6-(0,0,1/3) 0,0,z")),
  ((172,1),("x-y,x,z+2/3"," 6+(0,0,2/3) 0,0,z")),
  ((173,1),("y,y-x,z+1/2"," 6-(0,0,1/2) 0,0,z")),
  ((173,1),("x-y,x,z+1/2"," 6+(0,0,1/2) 0,0,z")),
  ((174,1),("x,y,-z"," m  x,y,0")),
  ((174,1),("-y,x-y,-z","-6- 0,0,z; 0,0,0")),
  ((174,1),("y-x,-x,-z","-6+ 0,0,z; 0,0,0")),
  ((176,1),("x,y,-z+1/2"," m  x,y,1/4")),
  ((176,1),("-y,x-y,-z+1/2","-6- 0,0,z; 0,0,1/4")),
  ((176,1),("y-x,-x,-z+1/2","-6+ 0,0,z; 0,0,1/4")),
  ((177,1),("y-x,y,-z"," 2  x,2x,0")),
  ((177,1),("x,x-y,-z"," 2  2x,x,0")),
  ((178,1),("y,x,-z+1/3"," 2  x,x,1/6")),
  ((178,1),("-x,y-x,-z+2/3"," 2  0,y,1/3")),
  ((178,1),("-y,-x,-z+5/6"," 2  x,-x,5/12")),
  ((178,1),("y-x,y,-z+1/2"," 2  x,2x,1/4")),
  ((178,1),("x,x-y,-z+1/6"," 2  2x,x,1/12")),
  ((179,1),("y,x,-z+2/3"," 2  x,x,1/3")),
  ((179,1),("-x,y-x,-z+1/3"," 2  0,y,1/6")),
  ((179,1),("-y,-x,-z+1/6"," 2  x,-x,1/12")),
  ((179,1),("x,x-y,-z+5/6"," 2  2x,x,5/12")),
  ((180,1),("-y,-x,-z+2/3"," 2  x,-x,1/3")),
  ((180,1),("x,x-y,-z+1/3"," 2  2x,x,1/6")),
  ((181,1),("-y,-x,-z+1/3"," 2  x,-x,1/6")),
  ((181,1),("x,x-y,-z+2/3"," 2  2x,x,1/3")),
  ((182,1),("-y,-x,-z+1/2"," 2  x,-x,1/4")),
  ((182,1),("x,x-y,-z+1/2"," 2  2x,x,1/4")),
  ((183,1),("x-y,-y,z"," m  x,0,z")),
  ((183,1),("-x,y-x,z"," m  0,y,z")),
  ((184,1),("y,x,z+1/2"," c  x,x,z")),
  ((184,1),("x-y,-y,z+1/2"," c  x,0,z")),
  ((184,1),("-x,y-x,z+1/2"," c  0,y,z"))
  ]
