package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

var origin, destination string
var verbose bool
var mindepth, maxdepth int

func init() {
	flag.StringVar(&origin, "i", "", "directory of origin")
	flag.StringVar(&destination, "o", "", "directory of destination")
	flag.BoolVar(&verbose, "v", false, "verbose output")
	flag.IntVar(&mindepth, "d", 1, "directory depth")
	flag.Parse()

	if origin == "" {
		fmt.Println("origin is missing")
		os.Exit(1)
	}
	if destination == "" {
		destination = origin
	}
	maxdepth = mindepth
}

func findDirectories() []string {
	output, err := exec.
		Command("bash", "-c", fmt.Sprintf("find %s -type d -mindepth %d -maxdepth %d", origin, mindepth, maxdepth)).
		Output()
	if err != nil {
		panic(err)
	}
	lines := strings.Split(string(output), "\n")
	directories := make([]string, len(lines))
	for i, value := range lines {
		directories[i] = string(value)
	}
	return directories
}

func fileExists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}

func img2cbr(dir string) {
	cbr := strings.Replace(dir+".cbr", origin, destination, 1)
	if fileExists(cbr) {
		if verbose {
			fmt.Printf("File already exists, skipping -- %s\n", cbr)
		}
		return
	}

	if err := os.MkdirAll(filepath.Dir(cbr), 0755); err != nil {
		fmt.Fprintln(os.Stderr, err)
	}

	cmd := exec.Command("zip", "-r", cbr, dir)
	if verbose {
		fmt.Println(cmd.String())
	}
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "convertion error: %s\n", err)
	}
}

func clearScreenANSI() {
	fmt.Print("\033[H\033[2J")
}

func printProgress(current, total int) {
	if !verbose {
		clearScreenANSI()
		for i := 0; i < current; i++ {
			fmt.Print(".")
		}
	}
	currentProgress := current * 100 / total
	fmt.Printf("(%d / %d) %d%s\n", current, total, currentProgress, "%")
}

func main() {
	directories := findDirectories()
	total := len(directories) - 1
	for i, dir := range directories {
		printProgress(i, total)
		if dir == "" {
			continue
		}
		img2cbr(dir)
	}
}
