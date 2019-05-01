---
title: "Template"
date:    2019-03-03T16:00:00+09:00
lastmod: 2019-03-03T16:00:00+09:00
draft: false
tags: ["template"]
categories: ["Notes"]
resources:
- name: header
  src: 'header.jpg'
authors:
- otaon
---
This is a simple preview for styles in Den.

<!--more-->

## Header

A paragraph is like this.

A paragraph might be very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very long.

### Headers
#### Headers



## Code blocks


    text code blocks
    text code blocks
    text code blocks
    text code blocks

## Highlighted Code Blocks

```python
print('hello world')
```

## Blockquotes


> This is a blockquote with two paragraphs. This is first paragraph.
>
> This is second pragraph.
>

## Lists


* Red
* Green
* Blue

1. Red
2. Green
3. Blue
    * A
    * B
    * C
      1. D
      2. E
      3. F


## Task List

- [ ] a task list item
- [ ] list syntax ~~required~~
- [ ] normal **formatting**
- [ ] incomplete
- [x] completed


## Formatted Texts

| Name              | Markdown              | HTML tag             |
| ----------------- | --------------------- | -------------------- |
| *Emphasis*        | \*Emphasis\*          | `<em></em>`          |
| **Strong**        | \*\*Strong\*\*        | `<strong></strong>`  |
| `code`            | \`code\`              | `<code></code>`      |
| ~~Strikethrough~~ | \~\~Strikethrough\~\~ | `<del></del`         |
| __Underline__     | \_\_underline\_\_     | `<u></u>`            |
| <kbd>Key</kbd>    | \<kbd\>Key\</kbd\>    | `<kbd></kbd>`            |

## Tables

| A     | B     | C     |
| ----- | ----- | ----- |
| a     | b     | c     |
| d     | e     | f     |

## Footnotes

You can create footnotes like this[^footnote].

[^footnote]: Here is the *text* of the **footnote**.


## Horizontal Rules

A rule.

------

A rule.

******

## Links

This is [an example](http://example.com/"Title") inline link.

## Images


![Globe](https://upload.wikimedia.org/wikipedia/commons/thumb/6/67/Octicons-globe.svg/240px-Octicons-globe.svg.png)


## Twiiter

{{< tweet 877500564405444608 >}}


## Google Map

{{< googlemaps id="17_6iCOL6LkRjIFGPKmXBxjsvbBc" height="400">}}


## Figure

{{<figure src="/images/globe.svg" alt="Globe" align="aligncenter" width="300" caption="**Globe**">}}


## Highlight for Shells

{{< shhighlight bash "hl_lines=2 4" >}}
# test
echo test
# just a test
echo hello world
{{< /shhighlight >}}
