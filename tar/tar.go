package tar

import (
	"archive/tar"
	"os"
	"log"
	"io"
	"time"
	"fmt"
	"io/ioutil"
)

func GetInfo(path string) ([]TarElem, error) {
	file, err := os.Open(path)
	defer file.Close()
	if err != nil {
		log.Fatalf("File open error: %s", path)
		return nil, err
	} else {
		return getTarInfoSlice(getGZipCompatibleReader(file), "")
	}
}

func getTarInfoSlice(r io.Reader, prefix string) ([]TarElem, error) {
	tarInfoSlice := []TarElem{}
	tarReader := tar.NewReader(r)
	for {
		if tarHeader, err := tarReader.Next(); err == io.EOF {
			break
		} else if err != nil {
			return nil, err
		} else {
			newTarElemName := prefix + tarHeader.Name
			digestReader := NewDigestReader(tarReader)
			if tarElems, err := getTarInfoSlice(getGZipCompatibleReader(digestReader), newTarElemName + "/"); err == nil {
				tarInfoSlice = append(tarInfoSlice, tarElems...)
			}
			ioutil.ReadAll(digestReader)
			newTarElem := TarElem{
				Type:      tarHeader.Typeflag,
				Mode:      tarHeader.FileInfo().Mode(),
				Uname:     tarHeader.Uname,
				Uid:       tarHeader.Uid,
				Gname:     tarHeader.Gname,
				Gid:       tarHeader.Gid,
				ModTime:   tarHeader.ModTime,
				HashValue: fmt.Sprintf("%064x", digestReader.getHash()),
				Size:      tarHeader.Size,
				Name:      prefix + tarHeader.Name,
				Linkname:  tarHeader.Linkname,
			}
			tarInfoSlice = append(tarInfoSlice, newTarElem)
		}
	}
	return tarInfoSlice, nil
}

type TarElem struct {
	Type      byte
	Mode      os.FileMode
	Uname     string
	Uid       int
	Gname     string
	Gid       int
	ModTime   time.Time
	HashValue string
	Size      int64
	Name      string
	Linkname  string
}

func (t TarElem) Key() string {
	return t.Name
}

func (t TarElem) String() string {
	str := fmt.Sprintf("%s %s %s %s %4s %s", t.Mode.String(), t.showOwner(), t.ModTime, t.HashValue[:7], t.showSize(), t.Name)
	if t.Linkname != "" {
		str += " -> "
		str += t.Linkname
	}
	return str
}

func (t TarElem) showType() rune {
	switch t.Type {
	case tar.TypeDir:
		return 'd'
	case tar.TypeSymlink:
		return 'l'
	case tar.TypeReg, tar.TypeRegA:
		return '-'
	default:
		return '?'
	}
}

var permExpr map[rune]string = map[rune]string{
	'0': "---",
	'1': "--x",
	'2': "-w-",
	'3': "-wx",
	'4': "r--",
	'5': "r-x",
	'6': "rw-",
	'7': "rwx",
}

func (t TarElem) showPerm() string {
	show := ""
	for _, ru := range fmt.Sprintf("%03o", t.Mode) {
		show += permExpr[ru]
	}
	return show
}

func (t TarElem) showOwner() string {
	return fmt.Sprintf("%s(%d)/%s(%d)", t.Uname, t.Uid, t.Gname, t.Gid)
}

func (t TarElem) showSize() string {
	n := t.Size
	switch {
	case n < 1000:
		return fmt.Sprintf("%d", n)
	case n < 10000:
		return fmt.Sprintf("%d", n / 1000) + "K"
	case n < 1000000:
		return fmt.Sprintf("%d", n / 1000.0) + "K"
	case n < 10000000:
		return fmt.Sprintf("%d", n / 1000000) + "M"
	case n < 1000000000:
		return fmt.Sprintf("%d", n / 1000000.0) + "M"
	case n < 10000000000:
		return fmt.Sprintf("%d", n / 1000000000) + "G"
	case n < 1000000000000:
		return fmt.Sprintf("%d", n / 1000000000.0) + "G"
	case n < 10000000000000:
		return fmt.Sprintf("%d", n / 1000000000000) + "T"
	case n < 1000000000000000:
		return fmt.Sprintf("%d", n / 1000000000000.0) + "T"
	default:
		return fmt.Sprintf("%d", n)
	}
}
