# mess

## Installation

```sh
stack setup
stack install
```

## Usage

```sh
mess-html --help && mess-fetch --help

# Download all of the messages and render them as html pages
mess-fetch dump.bin -l address@host.com -p my-vk-password
mess-html dump.bin
w3m index.html

# Download attached photos for local viewing
php -S 127.0.0.1:8080
wget -r -k -H http://127.0.0.1:8080/index.html
w3m 127.0.0.1/index.html
```
