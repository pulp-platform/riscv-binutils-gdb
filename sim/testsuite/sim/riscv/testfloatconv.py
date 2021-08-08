#!/usr/bin/python3

import sys
import argparse
import fileinput

parser = argparse.ArgumentParser(prog='testfloatconv',
                                 description="""Convert testfloat output
                                 to c arrays
example:
./testfloat_gen f16_add -prefix "name=fadd16 type=uint16_t" | \
  ./testfloatconv.py
""")

parser.version = '0.1'

if __name__ == "__main__":
    fi = fileinput.input()

    # expect a header like this:
    # name=[arrayname] type=[typename] where
    # arrayname is the c array's name and typename is the array's type
    header = next(fi)
    fields = header.rstrip().split(' ')
    if (len(fields) != 2):
        print('expected header with name=str type=str', file=sys.stderr)
        exit(1)
    if not fields[0].startswith('name='):
        print('expected header that starts with name=str', file=sys.stderr)
        exit(1)
    if not fields[1].startswith('type='):
        print('expected header that contains type=str', file=sys.stderr)
        exit(1)

    # generate array header
    typestr = fields[0].split('=')[1]
    namestr = fields[1].split('=')[1]
    print('%s %s[] = {' % (namestr, typestr))

    try:
        for line in fi:
            print(','.join(map(lambda x: '0x'+x, line.rstrip().split(' '))),
                  end=',\n')
        print('};')
    except BrokenPipeError:
        pass
