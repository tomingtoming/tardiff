package diff

import (
	"time"
)

type DiffConfig struct {
	M bool
	U bool
	G bool
	T bool
	S bool
	H bool
	P bool
}

var DefaultDiffConfig = DiffConfig{
	M: true,
	U: true,
	G: true,
	T: false,
	S: true,
	H: true,
	P: true,
}

type TarInfo struct {
	Mode    int64
	Uname   string
	Uid     int
	Gname   string
	Gid     int
	ModTime time.Time
	Size    int64
	MD5     string
	Path    string
}

func GetRepresentation(tarInfo TarInfo, config *DiffConfig) TarInfo {
	representation := TarInfo{}
	if config.M {
		representation.Mode = tarInfo.Mode
	}
	if config.U {
		representation.Uid = tarInfo.Uid
		representation.Uname = tarInfo.Uname
	}
	if config.G {
		representation.Gid = tarInfo.Gid
		representation.Gname = tarInfo.Gname
	}
	if config.T {
		representation.ModTime = tarInfo.ModTime
	}
	if config.S {
		representation.Size = tarInfo.Size
	}
	if config.H {
		representation.MD5 = tarInfo.MD5
	}
	if config.P {
		representation.Path = tarInfo.Path
	}
	return representation
}

func GetRepresentations(tarInfos *[]TarInfo, config *DiffConfig) map[TarInfo]TarInfo {
	representations := make(map[TarInfo]TarInfo)
	for _, tarInfo := range *tarInfos {
		representations[GetRepresentation(tarInfo, config)] = tarInfo
	}
	return representations
}
