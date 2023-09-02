// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for pretty-printing code.

struct Line<'a>(Vec<&'a str>);

struct Block<'a> {
    /// The prefix line, that could be concatenated to a previous block's tail.
    hang: Option<Line<'a>>,
    /// The body, which must have its own lines.
    body: Inner<'a>,
    /// The tail, to which more content could be concatenated.
    tail: Option<Line<'a>>,
    /// Width of the prefix line.
    width_hang: u32,
    /// Width of the body block.
    width_body: u32,
    /// Width of the trailing line.
    width_tail: u32,
}

enum Inner<'a> {
    Line(Line<'a>),
    Stack(Vec<Inner<'a>>),
    Indent(Box<Inner<'a>>),
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

impl<'a> Inner<'a> {
    pub fn width(&self) -> u32 {
        match self {
            Inner::Line(line) => line.width(),
            Inner::Stack(lines) => lines.iter().map(|l| l.width()).max().unwrap_or(0),
            Inner::Indent(body) => 2 + body.width(),
        }
    }

    pub fn stack(self, rhs: Inner<'a>) -> Inner<'a> {
        match (self, rhs) {
            (Inner::Stack(mut xs), Inner::Stack(mut ys)) => {
                xs.append(&mut ys);
                Inner::Stack(xs)
            }
            (Inner::Indent(x), Inner::Indent(y)) => Inner::Indent(Box::new(x.stack(*y))),
            (Inner::Stack(mut xs), y) => {
                xs.push(y);
                Inner::Stack(xs)
            }
            (x, Inner::Stack(mut ys)) => {
                ys.insert(0, x);
                Inner::Stack(ys)
            }
            (x, y) => Inner::Stack(vec![x, y]),
        }
    }

    pub fn indent(self) -> Inner<'a> {
        Inner::Indent(Box::new(self))
    }
}

impl<'a> Block<'a> {
    pub fn new(hang: Option<Line<'a>>, body: Inner<'a>, tail: Option<Line<'a>>) -> Block<'a> {
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
        let mid = Inner::Line(Line(vec![line]));
        Block {
            width_hang: 0,
            width_body: mid.width(),
            width_tail: 0,
            hang: None,
            body: mid,
            tail: None,
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
                body = body.stack(Inner::Line(x));
            }
            (Some(x), None) => body = body.stack(Inner::Line(x)),
            (None, Some(y)) => body = body.stack(Inner::Line(y)),
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
