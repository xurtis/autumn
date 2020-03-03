//! Source file location primitives

use std::fmt;
use std::ops::{Deref, DerefMut};
use std::path::Path;

/// Create a new source file location
///
/// This will create a location for the start of a source file.
pub fn new_location() -> Span {
    Span::default()
}

/// Create a new source file location that knows the source path to the file.
///
/// This will create a location for the start of the source file with the given path.
pub fn path_location<P: AsRef<Path>>(path: P) -> FileSource<P, Span> {
    FileSource::new(Span::default(), path)
}

/// Location of a single character within a text source
///
/// This tracks the location in a text source as each character is read. It can also be used to
/// present a human-readable form indication where in the source the character is.
#[derive(Debug, Default, Clone, Copy)]
pub struct Location {
    character: usize,
    byte: usize,
    row: usize,
    column: usize,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

impl Location {
    /// Get the chracter index in the source (0-indexed)
    pub fn character(&self) -> usize {
        self.character
    }

    /// Get the bytes index in the source (0-indexed)
    pub fn byte(&self) -> usize {
        self.byte
    }

    /// Get the row in the source (0-indexed)
    pub fn row(&self) -> usize {
        self.row
    }

    /// Get the column in the row (0-indexed)
    pub fn column(&self) -> usize {
        self.column
    }

    /// Move to the location after the given character
    pub fn after(&mut self, next_character: char) {
        self.character += 1;
        self.byte += next_character.len_utf8();

        if next_character == '\n' {
            self.row += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    /// Create an empty span starting at this location
    pub fn span(self) -> Span {
        Span {
            start: self,
            end: self,
        }
    }

    /// Bind to an object
    pub fn bind<T>(self, object: T) -> Meta<T, Self> {
        Meta { inner: object, location: self }
    }
}

/// The location of a sequence of characters within a text source
///
/// This tracks a span of characters from a text source. It can also be used to present
/// a human-readable location of the range in the source.
#[derive(Default, Debug, Clone, Copy)]
pub struct Span {
    start: Location,
    end: Location,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Span {
    /// Get the start of the span
    pub fn start(&self) -> &Location {
        &self.start
    }

    /// Get the end of the span
    ///
    /// The end itself is not part of the span
    pub fn end(&self) -> &Location {
        &self.end
    }

    /// Get the number of characters between the start and the end
    pub fn characters(&self) -> usize {
        self.end().character() - self.start().character()
    }

    /// Get the number of bytes between the start and the end
    pub fn bytes(&self) -> usize {
        self.end().byte() - self.start().byte()
    }

    /// Move the start position of a span
    pub fn move_start(&mut self, new_start: Location) {
        self.start = new_start;
    }

    /// Move the end position of a span
    pub fn move_end(&mut self, new_end: Location) {
        self.end = new_end;
    }

    /// Create a span following the given span
    pub fn following_span(&self) -> Self {
        let mut span = self.clone();
        span.move_start(span.end().clone());
        span
    }

    /// Take the current span and set this span to the empty span following it
    pub fn take(&mut self) -> Self {
        let span = self.clone();
        self.move_start(self.end().clone());
        span
    }

    /// Advance the end of a span by adding a character
    pub fn after(&mut self, next_character: char) {
        self.end.after(next_character);
    }

    /// Bind to an object
    pub fn bind<T>(self, object: T) -> Meta<T, Self> {
        Meta { inner: object, location: self }
    }
}

/// A location within a file on disk
#[derive(Debug, Clone, Copy)]
pub struct FileSource<P, I> {
    inner: I,
    path: P,
}

impl<P: AsRef<Path>, I> FileSource<P, I> {
    pub fn new(inner: I, path: P) -> Self {
        FileSource { inner, path }
    }
}

impl<P: AsRef<Path>, I: fmt::Display> fmt::Display for FileSource<P, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.path.as_ref().display(), self.inner)
    }
}

impl<P> FileSource<P, Location> {
    pub fn span(self) -> FileSource<P, Span> {
        FileSource {
            inner: self.inner.span(),
            path: self.path,
        }
    }
}

impl<P: AsRef<Path>, I> AsRef<Path> for FileSource<P, I> {
    fn as_ref(&self) -> &Path {
        self.path.as_ref()
    }
}

impl<P, I> Deref for FileSource<P, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<P, I> DerefMut for FileSource<P, I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// Structure to tag an object with a positional information
///
/// This can be used to associate any value with a particular [`Location`](struct.Location.html) or
/// [`Span`](struct.Span.html) within a source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Meta<T, I> {
    inner: T,
    location: I,
}

impl<T, I> Meta<T, I> {
    /// Unwrap the meta and return only the inner value
    pub fn inner(self) -> T {
        self.inner
    }

    /// Get a reference to the inner value
    pub fn inner_ref(&self) -> &T {
        &self.inner
    }

    /// Get a reference to the inner value
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    /// Split a meta into a separate value and location
    pub fn split(self) -> (T, I) {
        (self.inner, self.location)
    }

    /// Map the value within the meta
    pub fn map<A>(self, mut map: impl FnMut(T) -> A) -> Meta<A, I> {
        let Meta { inner, location } = self;
        let inner = map(inner);
        Meta { inner, location }
    }
}

impl<T, I: Clone> Meta<T, I> {
    /// Create a new meta referencing the original value
    pub fn as_ref(&self) -> Meta<&T, I> {
        Meta {
            inner: &self.inner,
            location: self.location.clone(),
        }
    }

    /// Create a new meta mutably referencing the original value
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

impl<T> Meta<T, Location> {
    /// Turn a meta referring to a single [`Location`](struct.Location.html) to an empty
    /// [`Span`](struct.Span.html) starting at the same location.
    pub fn span(self) -> Meta<T, Span> {
        Meta {
            location: self.location.span(),
            inner: self.inner,
        }
    }
}
