default: build

build-linux:
	GOOS=linux GOARCH=amd64 go build -o bin/img2cbr.linux main.go

build-darwin:
	GOOS=darwin GOARCH=amd64 go build -o bin/img2cbr.mac main.go

build-windows:
	GOOS=windows GOARCH=amd64 go build -o bin/img2cbr.exe main.go

build: build-linux build-darwin build-windows
