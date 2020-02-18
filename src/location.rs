//! Source file location primitives

use std::fmt;
use std::ops::{Deref, DerefMut};
use std::path::Path;

/// Create a new source file location
///
/// This will create a location for the start of a source file.
pub fn new_location() -> impl Span + fmt::Display {
    SourceLocation::default().span()
}

/// Create a new source file location that knows the source path to the file.
///
/// This will create a location for the start of the source file with the given path.
pub fn path_location<P>(path: P) -> impl Span + fmt::Display
where
    P: AsRef<Path> + Clone + fmt::Debug,
{
    FileSource::new(path).span()
}

/// Object that can track a single location in source
///
/// This tracks the location in a source file as each character is read. It can also be used to
/// present a human-readable form indication where in the source the character is.
pub trait Location: Sized + Clone + fmt::Debug {
    /// The type of span produced by this location
    type Span: Span + fmt::Debug;

    /// Get the chracter index in the source (0-indexed)
    fn character(&self) -> usize;

    /// Get the row in the source (0-indexed)
    fn row(&self) -> usize;

    /// Get the column in the row (0-indexed)
    fn column(&self) -> usize;

    /// Move to the location after the given character
    fn after(&mut self, next_character: char);

    /// Create an empty span starting at this location
    fn span(self) -> Self::Span;

    /// Bind to an object
    fn bind<T>(self, object: T) -> Meta<T, Self> {
        Meta::new(object, self)
    }
}

/// Object that can track a span withing a source
///
/// This tracks a region of characters from a source file. It can also be used to present
/// a human-readable location of the range in the source.
pub trait Span: Sized + Clone + fmt::Debug {
    /// The kind of location referred to by this span
    type Location: Location;

    /// Get the start of the span
    fn start(&self) -> &Self::Location;

    /// Get a mutable reference to the start
    fn start_mut(&mut self) -> &mut Self::Location;

    /// Get the end of the span
    ///
    /// The end itself is not part of the span
    fn end(&self) -> &Self::Location;

    /// Get a mutable reference to the end
    fn end_mut(&mut self) -> &mut Self::Location;

    /// Move the start position of a span
    fn move_start(&mut self, new_start: Self::Location) {
        *self.start_mut() = new_start;
    }

    /// Move the end position of a span
    fn move_end(&mut self, new_end: Self::Location) {
        *self.end_mut() = new_end;
    }

    /// Create a span following the given span
    fn following_span(&self) -> Self {
        let mut span = self.clone();
        span.move_start(span.end().clone());
        span
    }

    /// Take the current span and set this span to the empty span following it
    fn take(&mut self) -> Self {
        let span = self.clone();
        self.move_start(self.end().clone());
        span
    }

    /// Advance the end of a span by adding a character
    fn after(&mut self, next_character: char) {
        self.end_mut().after(next_character);
    }

    /// Bind to an object
    fn bind<T>(self, object: T) -> Meta<T, Self> {
        Meta::new(object, self)
    }
}

/// A source file location
#[derive(Debug, Default, Clone, Copy)]
struct SourceLocation {
    character: usize,
    row: usize,
    column: usize,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

impl Location for SourceLocation {
    type Span = SourceSpan;

    fn character(&self) -> usize {
        self.character
    }

    fn row(&self) -> usize {
        self.row
    }

    fn column(&self) -> usize {
        self.column
    }

    fn after(&mut self, next_character: char) {
        self.character += 1;

        if next_character == '\n' {
            self.row += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    fn span(self) -> Self::Span {
        SourceSpan {
            start: self,
            end: self,
        }
    }
}

/// A span within a source file, non-inclusive of the end
#[derive(Debug, Clone, Copy)]
struct SourceSpan {
    start: SourceLocation,
    end: SourceLocation,
}

impl fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Span for SourceSpan {
    type Location = SourceLocation;

    fn start(&self) -> &Self::Location {
        &self.start
    }

    fn start_mut(&mut self) -> &mut Self::Location {
        &mut self.start
    }

    fn end(&self) -> &Self::Location {
        &self.end
    }

    fn end_mut(&mut self) -> &mut Self::Location {
        &mut self.end
    }
}

/// A locator in a particular file
#[derive(Debug, Clone, Copy)]
struct FileSource<P, I> {
    inner: I,
    path: P,
}

impl<P: AsRef<Path>> FileSource<P, SourceLocation> {
    fn new(path: P) -> Self {
        FileSource {
            inner: Default::default(),
            path,
        }
    }
}

impl<P: AsRef<Path>, I: fmt::Display> fmt::Display for FileSource<P, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.path.as_ref().display(), self.inner)
    }
}

impl<P: AsRef<Path> + Clone + fmt::Debug, I: Location> Location for FileSource<P, I> {
    type Span = FileSource<P, I::Span>;

    fn character(&self) -> usize {
        self.inner.character()
    }

    fn row(&self) -> usize {
        self.inner.row()
    }

    fn column(&self) -> usize {
        self.inner.column()
    }

    fn after(&mut self, next_character: char) {
        self.inner.after(next_character)
    }

    fn span(self) -> Self::Span {
        FileSource {
            inner: self.inner.span(),
            path: self.path,
        }
    }
}

impl<P: AsRef<Path> + Clone + fmt::Debug, I: Span + fmt::Debug> Span for FileSource<P, I> {
    type Location = I::Location;

    fn start(&self) -> &Self::Location {
        self.inner.start()
    }

    fn start_mut(&mut self) -> &mut Self::Location {
        self.inner.start_mut()
    }

    fn end(&self) -> &Self::Location {
        self.inner.end()
    }

    fn end_mut(&mut self) -> &mut Self::Location {
        self.inner.end_mut()
    }
}

/// Structure to tag an object with a given location
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Meta<T, I> {
    inner: T,
    location: I,
}

impl<T, I> Meta<T, I> {
    /// Bind an object to a location
    pub fn new(inner: T, location: I) -> Self {
        Meta { inner, location }
    }

    pub fn inner(self) -> T {
        self.inner
    }

    pub fn split(self) -> (T, I) {
        (self.inner, self.location)
    }

    pub fn map<A>(self, mut map: impl FnMut(T) -> A) -> Meta<A, I> {
        let Meta { inner, location } = self;
        let inner = map(inner);
        Meta { inner, location }
    }
}

impl<T, I: Clone> Meta<T, I> {
    pub fn as_ref(&self) -> Meta<&T, I> {
        Meta {
            inner: &self.inner,
            location: self.location.clone(),
        }
    }

    pub fn as_mut(&mut self) -> Meta<&mut T, I> {
        Meta {
            inner: &mut self.inner,
            location: self.location.clone(),
        }
    }
}

impl<T: fmt::Display, I: fmt::Display> fmt::Display for Meta<T, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.location, self.inner)
    }
}

impl<T, I> std::error::Error for Meta<T, I>
where
    T: std::error::Error,
    I: fmt::Display + fmt::Debug,
{
}

impl<T, I> Deref for Meta<T, I> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, I> DerefMut for Meta<T, I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: Clone + fmt::Debug, I: Location> Location for Meta<T, I> {
    type Span = Meta<T, I::Span>;

    fn character(&self) -> usize {
        self.location.character()
    }

    fn row(&self) -> usize {
        self.location.row()
    }

    fn column(&self) -> usize {
        self.location.column()
    }

    fn after(&mut self, next_character: char) {
        self.location.after(next_character)
    }

    fn span(self) -> Self::Span {
        Meta {
            location: self.location.span(),
            inner: self.inner,
        }
    }
}

impl<T: Clone + fmt::Debug, I: Span> Span for Meta<T, I> {
    type Location = I::Location;

    fn start(&self) -> &Self::Location {
        self.location.start()
    }

    fn start_mut(&mut self) -> &mut Self::Location {
        self.location.start_mut()
    }

    fn end(&self) -> &Self::Location {
        self.location.end()
    }

    fn end_mut(&mut self) -> &mut Self::Location {
        self.location.end_mut()
    }
}
