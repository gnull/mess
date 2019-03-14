# mess

This repo contains a set of utilities which can be used to fetch one's messages
from vk.com, download all of the message attachments, and produce a fully static
html website which can be viewed with any browser locally without further need
for any additional processing. There is also an option to produce this website
without downloading attachments to your computer, in this case the website will
contain references to attachment URLs at vk.com.

Supported attachment types are:

  * Photos (maximum size only)
  * Stickers (fixed size only)
  * Links
  * Voice Messages (mp3 format only)
  * Wall posts
  * Files (aka Documents)
  * Videos

## Installation

These tools can be installed using Stack tool. You can obtain it at
https://haskellstack.org. Once you have stack, installing Mess is as easy as
running:

```sh
stack install
```

## Usage

This project contains three tools:

 * `mess-fetch` — for fetching messages and saving them to a local binary *dump*
   file;
 * `mess-cache` — for dowloading all *attachments* which are referenced by
   messages in a given *dump* file;
 * `mess-html` — for producing an html website given *attachments* and *dump*
   obtained by the previous two utilities.

All three tools print help text if run with `--help` flag, as well as bash
completion which can be fed into your shell by running a command like:

```
source <(mess-fetch --bash-completion-script `which mess-fetch`)
```
