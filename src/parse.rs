use crate::location::{Meta, Span};
use std::rc::Rc;

/// A parser takes an input source and produces an array of potential tagged values and an array of
/// errors.
pub trait Parser<T, E = ()> {
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E>;
}

impl<T, E, F> Parser<T, E> for F
where
    F: for<'s> Fn(&'s str, Span) -> ParseResult<'s, T, E>,
{
    fn parse<'s>(&self, source: &'s str, location: Span) -> ParseResult<'s, T, E> {
        self(source, location)
    }
}

/// The result of executing a parser
///
/// When executing a parser over a given input source the parser may correctly parse one or more
/// interpreteations of the source and produce a result value or a group of errors for each
/// interpretation.
#[derive(Debug)]
pub struct ParseResult<'s, T, E = ()> {
    start: Span,
    success: bool,
    failure: bool,
    results: Vec<InnerResult<'s, T, E>>,
}

impl<'s, T, E> ParseResult<'s, T, E> {
    /// Produce an iterator over references to all values produced from successful parses.
    ///
    /// Only parses that produced a value and had no associated errors or exceptions are produced.
    pub fn values<'r>(&'r self) -> impl Iterator<Item = &'r T> + 'r {
        self.results.iter().flat_map(InnerResult::value)
    }

    /// Produce an iterator over all values produced from successful parses.
    ///
    /// Only parses that produced a value and had no associated errors or exceptions are produced.
    /// Values that are produced are removed from the result. Parses with associated errors or
    /// exceptions remain in the result.
    pub fn take_values(&mut self) -> impl Iterator<Item = T> {
        let mut success = Vec::new();

        let mut results = Vec::new();
        ::std::mem::swap(&mut self.results, &mut results);

        for result in results {
            if result.is_failure() {
                self.results.push(result);
            } else if let Some(result) = result.take_value() {
                success.push(result);
            }
        }

        success.into_iter()
    }

    /// Check if a parse was successful.
    ///
    /// A parse is considered successful if there was at least one execution of the parse over the
    /// input that produced a result and there were no complete executions of the parser that
    /// produced any errors.
    pub fn is_success(&self) -> bool {
        self.success && !self.failure
    }

    /// Check if there were no complete parses of the input.
    pub fn is_none(&self) -> bool {
        !self.failure && !self.success
    }

    /// Check if there was only a single successful parse of the input
    pub fn single_parse(&self) -> bool {
        self.is_success() && self.results.len() == 1
    }

    pub(crate) fn none(location: Span) -> Self {
        ParseResult {
            start: location,
            success: false,
            failure: false,
            results: vec![],
        }
    }

    pub(crate) fn or(self, other: Self) -> Self {
        let ParseResult {
            start,
            mut success,
            mut failure,
            mut results,
        } = self;
        let ParseResult {
            success: other_success,
            failure: other_failure,
            results: mut other_results,
            ..
        } = other;
        success |= other_success;
        failure |= other_failure;
        results.append(&mut other_results);
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    pub(crate) fn map<A, F>(self, map: &F) -> ParseResult<'s, A, E>
    where
        F: Fn(T) -> A,
    {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results.into_iter().map(|result| result.map(map)).collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    pub(crate) fn catch(self) -> Self {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results.into_iter().map(InnerResult::catch).collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }
}

impl<'s, T, E> ParseResult<'s, T, E> {
    /// Produce an iterator over references to all errors from all complete parses
    pub fn errors<'r>(&'r self) -> impl Iterator<Item = Meta<impl AsRef<E>, Span>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::errors)
            .map(|error| error.as_ref().clone())
    }

    /// Produce an iterator over references to all uncaught exceptions from all complete parses
    pub fn exceptions<'r>(&'r self) -> impl Iterator<Item = Meta<impl AsRef<E>, Span>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::exceptions)
            .map(|error| error.as_ref().clone())
    }

    pub(crate) fn meta(self) -> ParseResult<'s, Meta<T, Span>, E> {
        let ParseResult {
            start,
            success,
            failure,
            results,
        } = self;
        let results = results.into_iter().map(InnerResult::meta).collect();
        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }
}

impl<'s, T, E: Clone> ParseResult<'s, T, E> {
    /// Produce an iterator over to all errors from all complete parses
    ///
    /// Clones each error.
    pub fn cloned_errors<'r>(&'r self) -> impl Iterator<Item = Meta<E, Span>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::errors)
            .map(|error| error.as_ref().clone().map(|error| error.as_ref().clone()))
    }

    /// Produce an iterator over to all uncaught exceptions from all complete parses
    ///
    /// Clones each exception.
    pub fn cloned_exceptions<'r>(&'r self) -> impl Iterator<Item = Meta<E, Span>> + 'r {
        self.results
            .iter()
            .flat_map(InnerResult::exceptions)
            .map(|error| error.as_ref().clone().map(|error| error.as_ref().clone()))
    }
}

impl<'s, T, E> ParseResult<'s, T, E> {
    pub(crate) fn error(value: T, error: E, source: &'s str, location: Span) -> Self {
        ParseResult {
            start: location.clone(),
            success: false,
            failure: true,
            results: vec![InnerResult::failure(value, error, source, location)],
        }
    }

    pub(crate) fn exception(value: T, error: E, source: &'s str, location: Span) -> Self {
        ParseResult {
            start: location.clone(),
            success: false,
            failure: true,
            results: vec![InnerResult::exception(value, error, source, location)],
        }
    }

    pub(crate) fn success(value: T, source: &'s str, location: Span) -> Self {
        ParseResult {
            start: location.clone(),
            success: true,
            failure: false,
            results: vec![InnerResult::success(value, source, location)],
        }
    }

    pub(crate) fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, E>
    where
        F: Fn(T, &'s str, Span) -> ParseResult<'s, A, E>,
    {
        let ParseResult { start, results, .. } = self;
        let mut new_results = vec![];
        let mut success = false;
        let mut failure = false;
        let iter = results.into_iter().map(|result| result.and_then(next));
        for ParseResult {
            mut results,
            success: new_success,
            failure: new_failure,
            ..
        } in iter
        {
            success |= new_success;
            failure |= new_failure;
            new_results.append(&mut results);
        }
        ParseResult {
            start,
            success,
            failure,
            results: new_results,
        }
    }
}

#[derive(Debug)]
struct InnerResult<'s, T, E = ()> {
    value: Meta<T, Span>,
    exceptions: List<Meta<Rc<E>, Span>>,
    errors: List<Meta<Rc<E>, Span>>,
    source: &'s str,
    location: Span,
}

impl<'s, T, E> InnerResult<'s, T, E> {
    fn value(&self) -> Option<&T> {
        if !self.is_failure() {
            Some(&self.value)
        } else {
            None
        }
    }

    fn take_value(self) -> Option<T> {
        if !self.is_failure() {
            Some(self.value.inner())
        } else {
            None
        }
    }

    fn exceptions(&self) -> List<Meta<Rc<E>, Span>> {
        self.exceptions.reverse()
    }

    fn errors(&self) -> List<Meta<Rc<E>, Span>> {
        self.errors.reverse()
    }

    fn is_failure(&self) -> bool {
        self.exceptions.length() > 0 || self.errors.length() > 0
    }

    fn map<A, F: Fn(T) -> A>(self, map: F) -> InnerResult<'s, A, E> {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        let value = value.map(map);
        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }

    fn catch(self) -> Self {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        let errors = errors.concat(&exceptions);
        let exceptions = List::new();
        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }
}

impl<'s, T, E> InnerResult<'s, T, E> {
    fn meta(self) -> InnerResult<'s, Meta<T, Span>, E> {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        let value = location.clone().bind(value);
        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }
}

impl<'s, T, E> InnerResult<'s, T, E> {
    fn success(value: T, source: &'s str, mut location: Span) -> Self {
        let value = location.take().bind(value);
        InnerResult {
            value,
            exceptions: List::new(),
            errors: List::new(),
            source,
            location,
        }
    }

    fn failure(value: T, error: E, source: &'s str, mut location: Span) -> Self {
        let parse_location = location.take();
        let value = parse_location.clone().bind(value);
        let error = parse_location.bind(Rc::new(error));
        InnerResult {
            value,
            exceptions: List::new(),
            errors: List::single(error),
            source,
            location,
        }
    }

    fn exception(value: T, exception: E, source: &'s str, mut location: Span) -> Self {
        let parse_location = location.take();
        let value = parse_location.clone().bind(value);
        let exception = parse_location.bind(Rc::new(exception));
        InnerResult {
            value,
            exceptions: List::single(exception),
            errors: List::new(),
            source,
            location,
        }
    }

    fn and_then<A, F>(self, next: &F) -> ParseResult<'s, A, E>
    where
        F: Fn(T, &'s str, Span) -> ParseResult<'s, A, E>,
    {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;
        // Take out the current value
        let (value, start) = value.split();
        // Get the results of parsing after the current value
        let result = next(value, source, location.clone());
        let ParseResult {
            success,
            mut failure,
            results,
            ..
        } = result;

        // Process the results
        let results: Vec<_> = results
            .into_iter()
            .map(|result| result.combine(&start, exceptions.clone(), errors.clone()))
            .collect();

        failure |= results.iter().any(InnerResult::is_failure);

        ParseResult {
            start,
            success,
            failure,
            results,
        }
    }

    fn combine(
        self,
        start: &Span,
        existing_exceptions: List<Meta<Rc<E>, Span>>,
        existing_errors: List<Meta<Rc<E>, Span>>,
    ) -> Self {
        let InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        } = self;

        // Adjust the location of the value
        let (value, mut value_location) = value.split();
        value_location.move_start(start.start().clone());
        let value = value_location.bind(value);

        // Move the start of all the uncaught exceptions
        let exceptions: List<_> = exceptions
            .concat(&existing_exceptions)
            .map(|meta| {
                let (exception, mut location) = meta.as_ref().clone().split();
                location.move_start(start.start().clone());
                location.bind(exception)
            })
            .collect();

        // Concatenate all of the errors
        let errors = existing_errors.concat(&errors);

        InnerResult {
            value,
            exceptions,
            errors,
            source,
            location,
        }
    }
}

/// Persistent linked-list data structure
///
/// This list data structure is used as it reduces the number of duplicates maintained in the
/// parser as it considers multiple potentially valid alternative parses.
///
/// It is used internally to maintain the set of errors produces for a particular parse as well as
/// for parsers returning lists of values.
///
/// The list is actually implemented as a first-in last-out stack structure so items in a parser
/// that produce a list will be the reverse of the order in which they were parsed.
///
/// Lists are also iterators over their elements. The iterator item of the list is an `Rc` of its
/// elements.
///
/// ```rust
/// # use autumn::List;
/// let values = &[0, 1, 2, 3, 4, 5, 6, 7, 8];
/// let listA = values[1..5].iter().fold(List::single(0), |list, value| list.push(*value));
/// let listB = values[5..9].iter().fold(List::new(), |list, value| list.push(*value));
/// let joined = listA.concat(&listB);
///
/// for (list_value, array_value) in joined.reverse().zip(values.iter()) {
///     assert_eq!(*list_value, *array_value);
/// }
/// ```
#[derive(Debug)]
pub struct List<T>(Rc<Node<T>>, usize);

impl<T> List<T> {
    /// Create a new, empty list
    pub fn new() -> Self {
        List(Rc::new(Node::Tail), 0)
    }

    /// Create a list with a single element
    pub fn single(value: T) -> Self {
        Self::new().push(value)
    }

    /// Get the number of items in the list
    ///
    /// ```rust
    /// # use autumn::List;
    /// let list = List::single(1).push(2).push(3);
    /// assert_eq!(list.length(), 3);
    /// ```
    pub fn length(&self) -> usize {
        self.1
    }

    /// Push an item onto the list
    pub fn push(&self, value: T) -> Self {
        let List(node, length) = self;
        let node = Node::Node(Rc::new(value), node.clone());
        List(Rc::new(node), *length + 1)
    }

    fn push_rc(&self, value: Rc<T>) -> Self {
        let List(node, length) = self;
        let node = Node::Node(value, node.clone());
        List(Rc::new(node), *length + 1)
    }

    /// Create the reversed copy of the list
    ///
    /// ```rust
    /// # use autumn::List;
    /// let values = &[1, 2, 3, 4, 5, 6];
    /// let list = values.iter().fold(List::new(), |list, value| list.push(*value));
    ///
    /// for (list_value, array_value) in list.reverse().zip(values.iter()) {
    ///     assert_eq!(*array_value, *list_value);
    /// }
    /// ```
    pub fn reverse(&self) -> Self {
        let mut reversed = List::new();
        for node in self.clone() {
            reversed = reversed.push_rc(node);
        }
        return reversed;
    }

    /// Concatenate two lists
    ///
    /// ```rust
    /// # use autumn::List;
    /// let list = List::single(1).push(2).push(3).concat(&List::single(4).push(5).push(6));
    /// # assert_eq!(list.length(), 6);
    /// ```
    pub fn concat(&self, other: &Self) -> Self {
        let mut joined = self.clone();
        for node in other.reverse() {
            joined = joined.push_rc(node);
        }
        joined
    }
}

impl<T> Clone for List<T> {
    fn clone(&self) -> Self {
        List(self.0.clone(), self.1)
    }
}

impl ToString for List<char> {
    fn to_string(&self) -> String {
        let mut string = String::with_capacity(self.1);
        for character in self.reverse() {
            string.push(*character);
        }
        string
    }
}

impl<T> Iterator for List<T> {
    type Item = Rc<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((next, tail)) = self.0.pair() {
            *self = List(tail, self.1 - 1);
            Some(next)
        } else {
            None
        }
    }
}

impl<T> ::std::iter::FromIterator<Rc<T>> for List<T> {
    fn from_iter<I: IntoIterator<Item = Rc<T>>>(iter: I) -> Self {
        let mut list = List::new();
        for item in iter {
            list = list.push_rc(item);
        }
        list
    }
}

impl<T> ::std::iter::FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut list = List::new();
        for item in iter {
            list = list.push(item);
        }
        list
    }
}

#[derive(Debug)]
enum Node<T> {
    Node(Rc<T>, Rc<Node<T>>),
    Tail,
}

impl<T> Node<T> {
    fn pair(&self) -> Option<(Rc<T>, Rc<Node<T>>)> {
        match self {
            Node::Node(value, next) => Some((value.clone(), next.clone())),
            Node::Tail => None,
        }
    }
}

impl<T> Clone for Node<T> {
    fn clone(&self) -> Self {
        match self {
            Node::Node(value, next) => Node::Node(value.clone(), next.clone()),
            Node::Tail => Node::Tail,
        }
    }
}
