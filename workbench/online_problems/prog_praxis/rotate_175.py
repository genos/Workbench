# Python answer to "Rotate an Array", 10/12/2010

def rotate(array, nelts):
    n = nelts % len(array)
    return array[n:] + array[:n]
