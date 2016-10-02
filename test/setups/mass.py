#!/usr/bin/env python3

for i in range(1,2000):
    x = """---
name: f{n}
description: Function Nr. {n}
returns: varchar
---
RETURN f{m}();""".format(n=i,m=i-1)
    with open("function-many/functions.d/f{}.sql".format(i),"w") as f:
        f.write(x)
