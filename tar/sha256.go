package tar

import (
	"io"
	"crypto/sha256"
	"hash"
)

type DigestReader struct {
	hash      hash.Hash
	rawReader io.Reader
}

func (dr DigestReader) Read(buf []byte) (int, error) {
	i, err := dr.rawReader.Read(buf)
	dr.hash.Write(buf[:i])
	return i, err
}

func (dr *DigestReader) getHash() []byte {
	return dr.hash.Sum([]byte{})
}

func NewDigestReader(r io.Reader) DigestReader {
	return DigestReader{
		hash:      sha256.New(),
		rawReader: r,
	}
}
