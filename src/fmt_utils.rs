// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for pretty-printing code.

use std::io::{Result, Write};

struct Line<'a>(Vec<&'a str>);

struct Block<'a> {
    /// The prefix line, that could be concatenated to a previous block's tail.
    hang: Option<Line<'a>>,
    /// The body, which must have its own lines.
    body: Rect<'a>,
    /// The tail, to which more content could be concatenated.
    tail: Option<Line<'a>>,
    /// Width of the prefix line.
    width_hang: u32,
    /// Width of the body block.
    width_body: u32,
    /// Width of the trailing line.
    width_tail: u32,
}

enum Rect<'a> {
    // TODO: Add comment line variant to be able to not measure comment width.
    Line(Line<'a>),
    Stack(Vec<Rect<'a>>),
    Indent(Box<Rect<'a>>),
}

impl<'a> Line<'a> {
    pub fn empty() -> Line<'a> {
        Line(Vec::new())
    }

    pub fn width(&self) -> u32 {
        use unicode_width::UnicodeWidthStr;
        self.0
            .iter()
            .map(|fragment| fragment.width())
            .sum::<usize>() as u32
    }
}

impl<'a> Rect<'a> {
    pub fn width(&self) -> u32 {
        match self {
            Rect::Line(line) => line.width(),
            Rect::Stack(blocks) => blocks.iter().map(|l| l.width()).max().unwrap_or(0),
            Rect::Indent(body) => 2 + body.width(),
        }
    }

    pub fn stack(self, rhs: Rect<'a>) -> Rect<'a> {
        match (self, rhs) {
            (Rect::Stack(mut xs), Rect::Stack(mut ys)) => {
                xs.append(&mut ys);
                Rect::Stack(xs)
            }
            (Rect::Indent(x), Rect::Indent(y)) => Rect::Indent(Box::new(x.stack(*y))),
            (Rect::Stack(mut xs), y) => {
                xs.push(y);
                Rect::Stack(xs)
            }
            (x, Rect::Stack(mut ys)) => {
                ys.insert(0, x);
                Rect::Stack(ys)
            }
            (x, y) => Rect::Stack(vec![x, y]),
        }
    }

    pub fn indent(self) -> Rect<'a> {
        Rect::Indent(Box::new(self))
    }

    pub fn write(&self, indent: usize, out: &'a mut dyn Write) -> Result<()> {
        match self {
            Rect::Line(line) => {
                let spaces = [b' '; 64];
                debug_assert!(indent < spaces.len());
                out.write_all(&spaces[..indent])?;
                for fragment in line.0.iter() {
                    out.write_all(fragment.as_bytes())?;
                }
                out.write_all(b"\n")?;
            }
            Rect::Stack(blocks) => {
                for block in blocks {
                    block.write(indent, out)?;
                }
            }
            Rect::Indent(body) => {
                body.write(indent + 2, out)?;
            }
        }
        Ok(())
    }
}

impl<'a> Block<'a> {
    pub fn new(hang: Option<Line<'a>>, body: Rect<'a>, tail: Option<Line<'a>>) -> Block<'a> {
        Block {
            width_hang: hang.as_ref().map(|x| x.width()).unwrap_or(0),
            width_body: body.width(),
            width_tail: tail.as_ref().map(|x| x.width()).unwrap_or(0),
            hang,
            body,
            tail,
        }
    }

    pub fn line(line: &'a str) -> Block<'a> {
        let mid = Rect::Line(Line(vec![line]));
        Block {
            width_hang: 0,
            width_body: mid.width(),
            width_tail: 0,
            hang: None,
            body: mid,
            tail: None,
        }
    }

    pub fn into_rect(self) -> Rect<'a> {
        match (self.hang, self.tail) {
            (Some(hang), Some(tail)) => Rect::Line(hang).stack(self.body).stack(Rect::Line(tail)),
            (None, Some(tail)) => self.body.stack(Rect::Line(tail)),
            (Some(hang), None) => Rect::Line(hang).stack(self.body),
            (None, None) => self.body,
        }
    }
}

impl<'a> std::ops::Add for Block<'a> {
    type Output = Block<'a>;

    fn add(self, that: Block<'a>) -> Block<'a> {
        let mut body = self.body;
        match (self.tail, that.hang) {
            (Some(mut x), Some(mut y)) => {
                x.0.append(&mut y.0);
                body = body.stack(Rect::Line(x));
            }
            (Some(x), None) => body = body.stack(Rect::Line(x)),
            (None, Some(y)) => body = body.stack(Rect::Line(y)),
            (None, None) => {}
        };
        let width_body = self.width_body.max(that.width_body);
        let body = body.stack(that.body);

        Block {
            hang: self.hang,
            body,
            tail: that.tail,
            width_hang: self.width_hang,
            width_body,
            width_tail: that.width_tail,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn block_stack_list() {
        let block = Block::new(
            Some(Line(vec!["A", " ", "B"])),
            Rect::Line(Line(vec!["B", "C"]))
                .stack(Rect::Line(Line(vec!["P", "Q"])))
                .indent(),
            Some(Line(vec!["X", "Z"])),
        );

        let mut out = Vec::new();
        block.into_rect().write(0, &mut out);
        assert_eq!(out, b"A B\n  BC\n  PQ\nXZ\n",);
    }
}
