import json

num_items = 10

with open('entries.json.fixed') as f:
    entries = []
    for i,line in enumerate(f.readlines()):
        try:
            d = json.loads(line)
            entries.append(d)
        except Exception as e:
            print(i)
            print(e)

entries[0].keys()
def shorten(entry):
    d = {k:entry[k] for k in ('number','name')}
    d['seq'] = [int(s) for s in entry['data'].split(',')[:num_items]]
    return d

short_entries = [shorten(e) for e in entries]
small_entries = [e for e in short_entries if all(x>=0 and x<10 for x in e['seq'])]

with open('entries.mjs','w') as f:
    f.write('export default ')
    f.write(json.dumps([[e['number'],e['name']]+e['seq'] for e in small_entries]))
    f.write(';')
