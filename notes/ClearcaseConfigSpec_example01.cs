element * CHECKEDOUT
element -dir * /main/LATEST

element -file * .../mydev/LATEST

element -file * .../baselinedev/LATEST -time 30-Aug.23:00 -mkbranch mydev
# The previous line with a timestamp won't pick up version 0 created after the timestamp.
element -file * .../baselinedev/0 -mkbranch mydev

element -file * /main/LATEST -time 30-Aug.23:00 -mkbranch baselinedev
# The previous line with a timestamp won't pick up version 0 created after the timestamp.
element -file * /main/0 -mkbranch baselinedev

