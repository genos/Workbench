package main

import "fmt"

type ByteSize uint64

const (
	_           = iota // ignore first value by assigning blank identifier
	KB ByteSize = 1 << (10 * iota)
	MB
	GB
	TB
	PB
	YB
)

func ilog_2(n ByteSize) ByteSize {
	var i, nn ByteSize = 0, n
	for ; nn > 1; i++ {
		nn >>= 1
	}
	return i
}

func main() {
	for _, v := range []ByteSize{KB, MB, GB, TB, PB, YB} {
		fmt.Println(v)
		fmt.Println(ilog_2(v))
	}
}
