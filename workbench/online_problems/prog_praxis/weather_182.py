import sys
import urllib

def weather(s, c):
    a = "http://weather.noaa.gov/pub/data/forecasts/city/%s/%s.txt" % (s, c)
    u = urllib.urlopen(a)
    for l in u: print l
    u.close()

if __name__ == '__main__':
    weather(sys.argv[1], sys.argv[2])
