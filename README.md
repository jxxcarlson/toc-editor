
## TOC-Editor

This project demonstrates a drag-and-drop editor for
a hierarchircal table of contents (HTOC).  Items can
be dragged vertically and the buttons < and > 
permit the user to change the indentation level.
Numbering is automatic. For example, one might
have an HTOC that looks like this:


```
1 Mount Yadaax-Vor
2 The Kraken Roars
3 The Magic Stone
4 A Quiet Pool
5 Lost on the River
6 Crossing the Desert
7 A Strange Beast
```

The user can rearange it be dragging items
and using the buttons (not shown) to make 
it look like this.

```
1 A Quiet Pool
2 Mount Yadaax-Vor
  2.1 The Magic Stone
  2.2 A Strange Beast
      2.2.1 The Kraken Roars
3 Lost on the River
4 Crossing the Desert
```

The demo app (`./demo/Main.elm`) uses the drag-and-drop library
[annaghi/dnd-list](https://package.elm-lang.org/packages/annaghi/dnd-list/latest/)
and the module `TOC` in this repo.  The latter makes use of rose trees
to number items of the list.

