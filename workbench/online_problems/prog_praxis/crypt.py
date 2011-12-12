from itertools import cycle, imap

def crypt(key, infile, outfile):
    kstream = cycle(key)
    text_xor = lambda a, b: chr(ord(a) ^ ord(b))
    with open(infile) as inf:
        with open(outfile, 'w') as outf:
            outf.write(''.join(imap(text_xor, inf.read(), kstream)))
                                  
                      
if __name__ == "__main__":
    crypt("abcd", "crypt.py", "out.txt")
    crypt("abcd", "out.txt", "clean.txt")
