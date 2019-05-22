
import random, re, sys

cmd = None

for arg in sys.argv[1:]:
    cmd = arg

with open("config.hpp", "rt") as file:
    lines = file.read().split("\n")

DEF_ID = "^#define ([_A-Za-z]+)"

indent = dict()
max_len = 0
for line in lines:
    m = re.match(DEF_ID, line)
    if m:
        name = m.group(1)
        max_len = max(max_len, len(name))

for line in lines:
    m = re.match(DEF_ID, line)
    if m:
        name = m.group(1)
        indent[name] = (max_len - len(name) + 1) * " "

for i, line in enumerate(lines):
    m = re.match(DEF_ID + " +(.+) //" + 3 * " ([0-9]+)", line)
    if m:
        n = m.group(1)
        v = m.group(2)
        d = m.group(3)
        l = int(m.group(4))
        h = int(m.group(5))

        s = indent[n]

        if cmd == "random":
            v = random.randint(l, h)
            line = "#define {n}{s}{v} // {d} {l} {h}".format(**locals())
            lines[i] = line
        elif cmd == "default":
            v = d
            line = "#define {n}{s}{v} // {d} {l} {h}".format(**locals())
            lines[i] = line

with open("config.hpp", "wt") as file:
    file.write("\n".join(lines))
