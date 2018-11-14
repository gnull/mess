# mess

This pair of utilities is capable of downloading one's messages from <vk.com>
website and generating a static pure-html website for browsing them locally. It
doesn't remove or modify any of your data at Vk. Currently support for message
attachmets is quite limited, although photos are supported — the generated
webpages embed ones localed at Vk servers.

The complete size of generated website usually doesn't exceed few megabytes.
Once the website is produced, it can be viewed with any browser without
further need for these *mess* tools. Although one will need an internet
connection for viewing it, since it embeds photos located at Vk servers. To
overcome this inconvenience one may use `wget -r` as illustrated by the example
below.

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

## Roadmap

  * Add support for new attachment types
    * Stickers — for overall prettiness of the output
    * Gifs — because they are used quite frequently
    * Posts — the same as Gifs
  * Add a tool for dynamically viewing the dump file obtained with `mess-fetch`.
    This tool should perform flexible filtering of dialogs and messages, display
    various statistics, show attachments and various inter-references between
    pages.
