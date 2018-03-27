package main

import (
	"github.com/codegangsta/cli"
	"os"
	"log"
	"github.com/tomingtoming/slicediff"
	"github.com/tomingtoming/tardiff/tar"
	"github.com/tomingtoming/tardiff/diff"
	"github.com/fatih/color"
)

func main() {
	app := cli.NewApp()
	app.Name = "tardiff"
	app.Usage = "compare tar files"
	app.Action = tardiff
	app.Version = "0.0.1"
	app.Run(os.Args)
}

func tardiff(c *cli.Context) {
	if c.NArg() != 2 {
		cli.ShowAppHelp(c)
	} else {
		srcPath := c.Args().Get(0)
		dstPath := c.Args().Get(1)
		srcElmChan := make(chan []tar.TarElem, 1)
		srcErrChan := make(chan error, 1)
		dstElmChan := make(chan []tar.TarElem, 1)
		dstErrChan := make(chan error, 1)
		go getInfoGoroutineWrapper(srcPath, srcElmChan, srcErrChan)
		go getInfoGoroutineWrapper(dstPath, dstElmChan, dstErrChan)
		select {
		case srcElems := <-srcElmChan:
				select {
				case dstElems := <-dstElmChan:
					for _, diff := range slicediff.NewDiff(asCompareableSlice(srcElems), asCompareableSlice(dstElems), diff.IsSame) {
						if del, ok := diff.(slicediff.Del); ok {
							color.Red(del.String())
						} else if mod, ok := diff.(slicediff.Mod); ok {
							color.Yellow(mod.String())
						} else if add, ok := diff.(slicediff.Add); ok {
							color.Green(add.String())
						}
					}
				case err := <-dstErrChan:
					log.Fatal(err)
				}
		case err := <-srcErrChan:
			log.Fatal(err)
		}
	}
}

func asCompareableSlice(es []tar.TarElem) []slicediff.Comparerable {
	cs := []slicediff.Comparerable{}
	for _, e := range es {
		cs = append(cs, e)
	}
	return cs
}

func getInfoGoroutineWrapper(path string, elemChan chan []tar.TarElem, errChan chan error) {
	if srcElems, err := tar.GetInfo(path); err != nil {
		errChan <- err
	} else {
		elemChan <- srcElems
	}
}
