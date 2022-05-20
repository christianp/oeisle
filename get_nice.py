from oeis import search
import json

f = open('entries.json','w')

def write(entry,nocomma=False):
    f.write(json.dumps(entry)+('' if nocomma else ',')

total, nice = search(query='keyword:nice')
f.write('[')
for e in nice:
    write(e)
i = len(nice)
while i<total:
    print(f'{i}/{total}')
    total, more = search(query='keyword:nice',start=i)
    nice += more
    for j,e in enumerate(more):
        write(e,i+j==total-1)
    i += len(more)

f.write(']')
f.close()
