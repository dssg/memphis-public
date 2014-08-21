from sys import argv

accumulator = None
bsFound = None

for line in open(argv[1]):
    if 'doMatching' in line:
        if accumulator is not None:
            mn = min(accumulator)
            print mn

            print ', '.join([var for var, val in zip(varaccumulator, accumulator) if abs(val - mn) < 0.001])
            print
            

        print line[:-1]
        accumulator = []
        varaccumulator = []
        bsFound = None

    elif 'KS Bootstrap' in line:
        bsFound = True
        val = line.split(' ')[-2]
        if val == '2.22e-16':
            val = 0
        accumulator.append(float(val))

    elif '* (V' in line:
        if bsFound != False: # true or None
            varaccumulator.append(' '.join(line.split(' ')[2:4]))
        else:
            varaccumulator[len(varaccumulator) - 1] = ' '.join(line.split(' ')[2:4])
        bsFound = False
    

mn = min(accumulator)
print mn

print ', '.join([var for var, val in zip(varaccumulator, accumulator) if abs(val - mn) < 0.001])
print

