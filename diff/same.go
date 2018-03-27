package diff

import (
	"github.com/tomingtoming/slicediff"
	"github.com/tomingtoming/tardiff/tar"
)

func IsSame(s, d slicediff.Comparerable) bool {
	if src, ok := s.(tar.TarElem); !ok {
		panic("???")
	} else if dst, ok := d.(tar.TarElem); !ok {
		panic("???")
	} else {
		return src.Mode == dst.Mode && src.Uname == dst.Uname && src.Uid == dst.Uid && src.Gname == dst.Gname && src.Gid == dst.Gid && src.HashValue == dst.HashValue && src.Size == dst.Size && src.Name == dst.Name && src.Linkname == dst.Linkname
	}
}
