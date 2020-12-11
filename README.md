1. Flagging Data

Manual flag 6 for certain values ​​that cannot be found by any routine, but should be excluded from the knowledge of the engineers.

| tool | SparcFlags(rs) |
| script | flagger.R |
| link | http://sparcwiki.awi-potsdam.de/doku.php?id=database:flagger |

2. Checking Data

Each timeseries should be checked thrice by engineers.

| tool | SparcChecks |
| script | checker.R |
| link | http://sparcwiki.awi-potsdam.de/doku.php?id=observatory:data:check |
|....|....|
| tool | SparcFlags(rs) [check 1 & 2]|
| script | flagger.R |
| link | http://sparcwiki.awi-potsdam.de/doku.php?id=database:flagger |
|....|....|
| tool | TrendViewer(rs) [check 3]|
| script | trendz.R |
| link | http://sparcwiki.awi-potsdam.de/doku.php?id=database:trendz2 |

