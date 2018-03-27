package tar

import (
	"io"
	"compress/gzip"
)

func getGZipCompatibleReader(r io.Reader) io.Reader {
	if gzipReader, err := gzip.NewReader(r);  err != nil {
		return r
 	} else {
		return gzipReader
	}
}
